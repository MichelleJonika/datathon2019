data <- read.csv('shelf-life-study-data-for-analytics-challenge_prediction.csv')
shelf_life_study_data_for_analytics_challenge_prediction <- read.csv("pepsi.data.csv", na.strings = '')

num <- 0
badrows <- c()
for(i in 1:nrow(shelf_life_study_data_for_analytics_challenge_prediction)){
  c <- sum(is.na(shelf_life_study_data_for_analytics_challenge_prediction[i,]))
  if(c > .5 * ncol(shelf_life_study_data_for_analytics_challenge_prediction)){
    num = num + 1
    badrows <- c(badrows, i)
  }
}
num
shelf_life_study_data_for_analytics_challenge_prediction <- shelf_life_study_data_for_analytics_challenge_prediction[-badrows,]

badcols <- c()
for(i in 1:ncol(shelf_life_study_data_for_analytics_challenge_prediction)){
  c<- sum(is.na(shelf_life_study_data_for_analytics_challenge_prediction[,i]))
  if(c > .5*nrow(shelf_life_study_data_for_analytics_challenge_prediction)){
    badcols <- c(badcols,i)
  }
}
shelf_life_study_data_for_analytics_challenge_prediction <- shelf_life_study_data_for_analytics_challenge_prediction[,-badcols[1:3]]
trainRowNumbers <- createDataPartition(shelf_life_study_data_for_analytics_challenge_prediction$Difference.From.Fresh, p=0.8, list=FALSE)
# Step 2: Create the training  dataset
trainData <- shelf_life_study_data_for_analytics_challenge_prediction[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- shelf_life_study_data_for_analytics_challenge_prediction[-trainRowNumbers,]
##  converting categorical variables to numeric to be used in machine learning
# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(DifferenceFromFresh ~ ., data=trainData)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = trainData)

# Convert to dataframe
trainData <- data.frame(trainData_mat)

# See the structure of the new dataset
str(trainData)

## preprocessing our data by converting all numeric data to be scaled between 0 and 1
preProcess_range_model <- preProcess(trainData, method='range')
trainData <- predict(preProcess_range_model, newdata = trainData)
##  fill in missing data using impution
# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
preProcess_missingdata_model

# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData <- predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData)

