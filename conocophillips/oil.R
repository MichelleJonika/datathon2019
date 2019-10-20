#load in training dataset
train <- read.csv("conocophillips/equip_failures_training_set.csv", na.strings = "na")
na.train <- na.omit(train)
#laod in testing dataset
test <- read.csv("conocophillips/equip_failures_test_set.csv")

#remove measure54 because it has no variance
train$sensor54_measure <- NULL
##  fill in missing data using impution
# Create the knn imputation model on the training data
library(caret)
preProcess_missingdata_model <- preProcess(train, method='knnImpute')
preProcess_missingdata_model


# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData <- predict(preProcess_missingdata_model, newdata = train)
anyNA(trainData)

#set seed for reproducibility
set.seed(100)

#model using svmlinear3
model_mars = train(target ~ ., data=trainData, method='svmLinear3')
fitted <- predict(model_mars)

library(dplyr)
train.sample <- sample_frac(trainData, 0.1)
ts.index <- sample_frac(as.data.frame(1:nrow(train.sample)), 0.8)
ts.train <- train.sample[ts.index$`1:nrow(train.sample)`,]
ts.test <- train.sample[-ts.index$`1:nrow(train.sample)`,]

ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10, classProbs = T )
plsfit <- train(factor(target) ~., 
                data = ts.train, 
                method = "pls", 
                preProc = c("center", "scale"),
                tunelength = 150,
                trControl = ctrl,
                metric = "ROC")
