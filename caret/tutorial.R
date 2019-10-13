# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

# Load the caret package
library(caret)

# Import dataset
orange <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')

# Structure of the dataframe
str(orange)

# See top 6 rows and 10 columns
head(orange[, 1:10])
# Create the training and test datasets
set.seed(100)

##  creating the training data set. Leaving the last 20% for testing
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(orange$Purchase, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- orange[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- orange[-trainRowNumbers,]

# Store X and Y for later use.
x = trainData[, 2:18]
y = trainData$Purchase

# view data
library(skimr)
skimmed <- skim_to_wide(trainData)
skimmed[, c(1:5, 9:11, 13, 15:16)]

##  fill in missing data using impution
# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
preProcess_missingdata_model

# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData <- predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData)

##  converting categorical variables to numeric to be used in machine learning
# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(Purchase ~ ., data=trainData)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = trainData)

# Convert to dataframe
trainData <- data.frame(trainData_mat)

# See the structure of the new dataset
str(trainData)

## preprocessing our data by converting all numeric data to be scaled between 0 and 1
preProcess_range_model <- preProcess(trainData, method='range')
trainData <- predict(preProcess_range_model, newdata = trainData)

# Append the Y variable
trainData$Purchase <- y

apply(trainData[, 1:10], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

## visualize the importance of certain predictor variables using boxplots
featurePlot(x = trainData[, 1:18], 
            y = trainData$Purchase, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# from this we can see list price off and loyalch to have significant differences
#  what other means are significantly differt??
# Let's do a similar exercise with density plots.

# In this case, For a variable to be important, I would expect the density curves to be significantly different 
# for the 2 classes, both in terms of the height (kurtosis) and placement (skewness).
# Take a look at the density curves of the two categories for 'LoyalCH', 'STORE', 
# 'StoreID', 'WeekofPurchase'. Are they different?
featurePlot(x = trainData[, 1:18], 
            y = trainData$Purchase, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# From the density plot we can see store.no, weekofpurchase, week of purchase, and loyalch
# to be important predictors

##   feature selection using recursive feature elimination (rfe)?
# Most machine learning algorithms are able to determine what features are important to predict 
# the Y. But in some scenarios, you might be need to be careful to include only variables that 
# may be significantly important and makes strong business sense.
# 
# RFE works in 3 broad steps:
#   
# Step 1: Build a ML model on a training dataset and estimate the feature importances on the test dataset.
# 
# Step 2: Keeping priority to the most important variables, iterate through by building models of 
# given subset sizes, that is, subgroups of most important predictors determined from step 1. 
# Ranking of the predictors is recalculated in each iteration.
# 
# Step 3: The model performances are compared across different subset sizes to arrive at the 
# optimal number and list of final predictors.

# install.packages('e1071')
library(e1071)
set.seed(100)
options(warn=-1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 1:18], y=trainData$Purchase,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

# part from the x and y datasets, RFE also takes two important parameters.
# 
# sizes
# rfeControl
# 
# The sizes determines what all model sizes (the number of most important features) 
# the rfe should consider. In above case, it iterates models of size 1 to 5, 10, 15 and 18.
# 
# The rfeControl parameter on the other hand receives the output of the rfeControl() 
# as values. If you look at the call to rfeControl() we set what type of algorithm 
# and what cross validation method should be used.
# 
# In above case, the cross validation method is repeatedcv which implements 
# k-Fold cross validation repeated 5 times, which is rigorous enough for our case.
# 
# Once rfe() is run, the output shows the accuracy and kappa 
# (and their standard deviation) for the different model sizes we 
# provided. The final selected model subset size is marked with a * in the rightmost Selected column.
# 
# From the above output, a model size of 3 with LoyalCH, PriceDiff 
# and StoreID seems to achieve the optimal accuracy.


##   train() the model and interpret the results?
# what models does caret suppport?
# See available algorithms in caret

modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames

# And if you want to know more details like the hyperparameters and 
# if it can be used of regression or classification problem, then do a modelLookup(algo).
# 
# Once you have chosen an algorithm, building the model is fairly easy using the train() function.
# 
# Let's train a Multivariate Adaptive Regression Splines (MARS) model by setting the method='earth'.
# 
# The MARS algorithm was named as 'earth' in R because of a possible trademark conflict with 
# Salford Systems. May be a rumor. Or not.

modelLookup('earth')

# Set the seed for reproducibility
set.seed(100)

# Train the model using randomForest and predict on the training data itself.
model_mars = train(Purchase ~ ., data=trainData, method='earth')
fitted <- predict(model_mars)

# how is using train() different from using the algorithm's function directly?
#   
#   The difference is, besides building the model train() does multiple other things like:
#   
#   Cross validating the model
# Tune the hyper parameters for optimal model performance
# Choose the optimal model based on a given evaluation metric
# Preprocess the predictors (what we did so far using preProcess())
# The train function also accepts the arguments used by the algorithm specified in the method argument.

# You can see what is the Accuracy and Kappa for various combinations of the hyper 
# parameters - interaction.depth and n.trees. And it says 'Resampling: 
# Bootstrapped (25 reps)' with a summary of sample sizes.
# 
# Looks like train() has already done a basic cross validation and hyper parameter 
# tuning. And that is the default behaviour.
# 
# The chosen model and its parameters is reported in the last 2 lines of the output.
# 
# When we used model_mars to predict the Y, this final model was automatically 
# used by predict() to compute the predictions.
# 
# Plotting the model shows how the various iterations of hyperparameter search performed.

plot(model_mars, main="Model Accuracies with MARS")

##  How to compute variable importance?

varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")

##  Prepare the test dataset and predict
# A default MARS model has been selected.
# 
# Now in order to use the model to predict on new data, the data has to be preprocessed 
# and transformed just the way we did on the training data.
# 
# If you recall, we did the pre-processing in the following sequence:
#   
#   Missing Value imputation -> One-Hot Encoding -> Range Normalization
# 
# You need to pass the testData through these models in the same sequence:
#   
#   preProcess_missingdata_model -> dummies_model -> preProcess_range_model

# Step 1: Impute missing values 
testData2 <- predict(preProcess_missingdata_model, testData)  

# Step 2: Create one-hot encodings (dummy variables)
testData3 <- predict(dummies_model, testData2)

# Step 3: Transform the features to range between 0 and 1
testData4 <- predict(preProcess_range_model, testData3)

# View
head(testData4[, 1:10])

## Predict on testData
# The test dataset is prepared. Let's predict the Y.

# Predict on testData
predicted <- predict(model_mars, testData4)
head(predicted)

##  Confusion Matrix
# The confusion matrix is a tabular representation to compare the predictions (data) 
# vs the actuals (reference). By setting mode='everything' pretty much most 
# classification evaluation metrics are computed.

# Compute the confusion matrix
confusionMatrix(reference = testData$Purchase, data = predicted, mode='everything', positive='MM')

##  How to do hyperparameter tuning to optimize the model for better performance?
# There are two main ways to do hyper parameter tuning using the train():
#   
# Set the tuneLength
# Define and set the tuneGrid
# tuneLength corresponds to the number of unique values for the tuning parameters caret 
# will consider while forming the hyper parameter combinations.
# 
# Caret will automatically determine the values each parameter should take.
# 
# Alternately, if you want to explicitly control what values should be considered 
# for each parameter, then, you can define the tuneGrid and pass it to train().

# Let's see an example of both these approaches but first let's setup the trainControl().

##  Setting up the trainControl()
# The train() function takes a trControl argument that accepts the output of trainControl().
# 
# Inside trainControl() you can control how the train() will:
#   
#   Cross validation method to use.
# How the results should be summarised using a summary function
# Cross validation method can be one amongst:
#   
#   'boot': Bootstrap sampling
# 'boot632': Bootstrap sampling with 63.2% bias correction applied
# 'optimism_boot': The optimism bootstrap estimator
# 'boot_all': All boot methods.
# 'cv': k-Fold cross validation
# 'repeatedcv': Repeated k-Fold cross validation
# 'oob': Out of Bag cross validation
# 'LOOCV': Leave one out cross validation
# 'LGOCV': Leave group out cross validation
# The summaryFunction can be twoClassSummary if Y is binary class or multiClassSummary 
# if the Y has more than 2 categories.
# 
# By settiung the classProbs=T the probability scores are generated instead of directly 
# predicting the class based on a predetermined cutoff of 0.5.

# Define the training control
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 

##  Hyper Parameter Tuning using tuneLength
# Let's take the train() function we used before, plus, additionally set the tuneLength, trControl and metric.

# Step 1: Tune hyper parameters by setting tuneLength
set.seed(100)
model_mars2 = train(Purchase ~ ., data=trainData, method='earth', tuneLength = 5, metric='ROC', trControl = fitControl)
model_mars2

# Step 2: Predict on testData and Compute the confusion matrix
predicted2 <- predict(model_mars2, testData4)
confusionMatrix(reference = testData$Purchase, data = predicted2, mode='everything', positive='MM')

##  Hyper Parameter Tuning using tuneGrid
# Alternately, you can set the tuneGrid instead of tuneLength.

# Step 1: Define the tuneGrid
marsGrid <-  expand.grid(nprune = c(2, 4, 6, 8, 10), 
                         degree = c(1, 2, 3))

# Step 2: Tune hyper parameters by setting tuneGrid
set.seed(100)
model_mars3 = train(Purchase ~ ., data=trainData, method='earth', metric='ROC', tuneGrid = marsGrid, trControl = fitControl)
model_mars3

# Step 3: Predict on testData and Compute the confusion matrix
predicted3 <- predict(model_mars3, testData4)
confusionMatrix(reference = testData$Purchase, data = predicted3, mode='everything', positive='MM')

##  How to evaluate performance of multiple machine learning algorithms?
# Caret provides the resamples() function where you can provide multiple machine l
# earning models and collectively evaluate them.
# 
# Let's first train some more algorithms.

##  Training Adaboost
set.seed(100)

# Train the model using adaboost
model_adaboost = train(Purchase ~ ., data=trainData, method='adaboost', tuneLength=2, trControl = fitControl)
model_adaboost

##  Training Random Forest
set.seed(100)

# Train the model using rf
model_rf = train(Purchase ~ ., data=trainData, method='rf', tuneLength=5, trControl = fitControl)
model_rf

##  Training xgBoost Dart
set.seed(100)
 
# Train the model using MARS
model_xgbDART = train(Purchase ~ ., data=trainData, method='xgbDART', tuneLength=5, trControl = fitControl, verbose=F)
model_xgbDART


##  Training SVM
set.seed(100)

# Train the model using MARS
model_svmRadial = train(Purchase ~ ., data=trainData, method='svmRadial', tuneLength=15, trControl = fitControl)
model_svmRadial

##  Run resamples() to compare the models
# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, RF=model_rf, XGBDART=model_xgbDART, MARS=model_mars3, SVM=model_svmRadial))

# Summary of the models performances
summary(models_compare)

# Let's plot the resamples summary output.

# Draw box plots to compare models
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# bwplot(models_compare, scales=scales)
# 
# In the above output you can see clearly how the algorithms performed in terms of ROC, 
# Specificity and Sensitivity and how consistent has it been.
# 
# The xgbDART model appears to be the be best performing model overall because of the 
# high ROC. But if you need a model that predicts the positives better, you might want 
# to consider MARS, given its high sensitivity.
# 
# Either way, you can now make an informed decision on which model to pick.
##  How to ensemble predictions from multiple models using caretEnsemble?
# So we have predictions from multiple individual models. To do this we had to run 
# the train() function once for each model, store the models and pass it to the res
# 
# The caretEnsemble package lets you do just that.
# 
# All you have to do is put the names of all the algorithms you want to run in a 
# vector and pass it to caretEnsemble::caretList() instead of caret::train().

library(caretEnsemble)

# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')

set.seed(100)
models <- caretList(Purchase ~ ., data=trainData, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)

# Plot the resamples output to compare the models.

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

##  How to combine the predictions of multiple models to form a final prediction
# That one function simplified a whole lot of work in one line of code.
# 
# Here is another thought: Is it possible to combine these predicted values from 
# multiple models somehow and make a new ensemble that predicts better?
#   
# Turns out this can be done too, using the caretStack(). You just need to make sure 
# you don't use the same trainControl you used to build the models.

# Create the trainControl
set.seed(101)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

# A point to consider: The ensembles tend to perform better if the 
# predictions are less correlated with each other.
# 
# So you may want to try passing different types of models, both high and 
# low performing rather than just stick to passing high accuracy models to the caretStack.

print(stack.glm)


# Predict on testData
stack_predicteds <- predict(stack.glm, newdata=testData4)
head(stack_predicteds)










##### election data #####

# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

# Load the caret package
library(caret)

# Import dataset
orange <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')
election <- read.csv('CleanedElection.csv')

# Structure of the dataframe
str(orange)
str(election)
# See top 6 rows and 10 columns
head(orange[, 1:10])
head(election[,1:10])

# Create the training and test datasets
set.seed(100)

# Step 1: Get row numbers for the training data we want an 80:20 training:test split
trainRowNumbersOrange <- createDataPartition(orange$Purchase, p=0.8, list=FALSE)
trainRowNumbersElection <- createDataPartition(election$county_nam, p = 0.8, list = F)

# Step 2: Create the training  dataset
trainDataOrange <- orange[trainRowNumbersOrange,]
trainDataElection <- election[trainRowNumbersElection,]

# Step 3: Create the test dataset
testDataOrange <- orange[-trainRowNumbersOrange,]
testDataElection <- election[-trainRowNumbersElection,]

# Store X and Y for later use.
xOrange = trainDataOrange[, 2:18]
yOrange = trainDataOrange$Purchase
xElection = trainDataElection[, 2:18]
yElection = trainDataElection$
  
  # Filling in missing data
  # Create the knn imputation model on the training data
  preProcess_missingdata_modelorange <- preProcess(trainDataOrange, method='knnImpute')
preProcess_missingdata_modelorange
preProcess_missingdata_modelelection <- preProcess(trainDataElection, method = 'knnImpute')
preProcess_missingdata_modelelection

# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainDataOrange <- predict(preProcess_missingdata_modelorange, newdata = trainDataOrange)
anyNA(trainDataOrange)
trainDataElection <- predict(preProcess_missingdata_modelelection, newdata = trainDataElection)
anyNA(trainDataElection)

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_modelorange <- dummyVars(Purchase ~ ., data=trainDataOrange)
dummies_modelelection <- dummyVars(county_nam ~., data = trainDataElection)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_matOrange <- predict(dummies_modelorange, newdata = trainDataOrange)
trainData_matElection <- predict(dummies_modelelection, newdata = trainDataElection)

# # Convert to dataframe
trainDataOrange <- data.frame(trainData_matOrange)
trainDataElection <- data.frame(trainData_matElection)

# # See the structure of the new dataset
str(trainDataOrange)
str(trainDataElection)

# rescaling the numerical data to be between 0 and 1
preProcess_range_modelorange <- preProcess(trainDataOrange, method='range')
trainDataOrange <- predict(preProcess_range_modelorange, newdata = trainDataOrange)
preProcess_range_modelelection <- preProcess(trainDataElection, method = 'range')
trainDataElection <- predict(preProcess_range_modelelection, newdata = trainDataElection)

# Append the Y variable
trainData$Purchase <- y

apply(trainData[, 1:10], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})