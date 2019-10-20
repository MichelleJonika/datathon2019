#load in training dataset
train <- read.csv("conocophillips/equip_failures_training_set.csv", na.strings = "na")
na.train <- na.omit(train)
#laod in testing dataset
test <- read.csv("conocophillips/equip_failures_test_set.csv", na.strings = "na")

#remove measure54 because it has no variance
train$sensor54_measure <- NULL
test$sensor54_measure <- NULL
##  fill in missing data using impution
# Create the knn imputation model on the training data
library(caret)
preProcess_missingdata_model <- preProcess(train[,-2], method='knnImpute')
preProcess_missingdata_model.test <- preProcess(test, method = "knnImpute")
preProcess_missingdata_model


# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData <- predict(preProcess_missingdata_model, newdata = train)
testData <- predict(preProcess_missingdata_model.test, newdata = test)
anyNA(trainData)
anyNA(testData)

#set seed for reproducibility
set.seed(100)

#model using svmlinear3
model_mars = train(target ~ ., data=trainData, method='svmLinear3')
fitted <- predict(model_mars)
trainData <- trainData[,-1]
trainData$sensor19_measure <- NULL
trainData$sensor58_measure <- NULL
trainData.pos <- trainData[trainData$target == 1,]
trainData.neg <- trainData[trainData$target != 1,]

testData <- testData[,-1]
testData$sensor19_measure <- NULL
testData$sensor58_measure <- NULL

library(dplyr)
train.sample.neg <- sample_frac(trainData.neg, 0.017)
train.sample.pos <- sample_frac(trainData.pos, .5)
train.sample.pos$target <- "good"
train.sample.neg$target <- "bad"
train.sample <- rbind(train.sample.neg, train.sample.pos)
ts.index <- sample_frac(as.data.frame(1:nrow(train.sample)), 0.8)
ts.train <- train.sample[ts.index$`1:nrow(train.sample)`,]
ts.test <- train.sample[-ts.index$`1:nrow(train.sample)`,]

ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10, classProbs = T)

#this is our final model
rffit <- train(factor(target) ~., 
               data = ts.train, 
               method = "rf", 
               preProc = c("center", "scale"),
               tunelength = 150,
               trControl = ctrl,
               metric = "ROC")

rf.pred <- predict(rffit, ts.test)
rf.CM <- confusionMatrix(reference = as.factor(ts.test$target), 
                         data = as.factor(rf.pred), 
                         mode = "everything", 
                         positive = "good")

rf.testpred <- predict(rffit, testData)
rf.testpred <- cbind(1:length(rf.testpred), rf.testpred)
rf.testpred <- as.data.frame(rf.testpred)
rf.testpred$rf.testpred <- rf.testpred$rf.testpred -1
#makes table of confusion matrix values
ctable <- as.table(matrix(c(186,7,10,98), nrow = 2, byrow = T))
fourfoldplot(ctable, color = c("maroon", "black"), conf.level = 0, margin = 1,
             main = "Confusion Matrix")
