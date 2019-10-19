library(caret)
library(dplyr)
library(RANN)
pepsi_data<- read.csv("../shelf-life-study-data-for-analytics-challenge_prediction.csv", na.strings = "", stringsAsFactors = FALSE)
pepsi_nas <- is.na(pepsi_data[,1:16])
pepsi_nas_filter <- apply(pepsi_nas, 1, sum)
pepsi_data <- pepsi_data[pepsi_nas_filter<8,1:15]
pepsi_data <- pepsi_data[,c(-10,-12,-15, -1,-2)]


trainPepsi_data <- createDataPartition(pepsi_data$Product.Type, p=0.8, list=FALSE)
trainpepsi <- pepsi_data[trainPepsi_data,]
testpepsi <- pepsi_data[-trainPepsi_data,]
preProcess_missingdata_model <- preProcess(trainpepsi, method='knnImpute')
preProcess_missingdata_model2 <- preProcess(testpepsi, method='knnImpute')
testpepsi <- predict(preProcess_missingdata_model2, newdata = testpepsi)
trainpepsi<- predict(preProcess_missingdata_model, newdata = trainpepsi)
pepsi_data[is.na(pepsi_data)] = "Unknown"

testpepsi[is.na(testpepsi)] = "Unknown"
trainpepsi[is.na(trainpepsi)] = "Unknown"

model_mars = train(Difference.From.Fresh ~ ., data=trainpepsi, method='earth')
fitted <- predict(model_mars, testpepsi)
plot(model_mars, main="Model Accuracies with MARS")
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")
prediction <- predict(model_mars, testpepsi)
prediction <- data.frame(prediction>0)
actual <- data.frame(testpepsi[,5]>0)
confmat <- confusionMatrix(reference = as.factor(actual$testpepsi...5....0), data = as.factor(prediction$y), mode = 'everything', positive = "TRUE" )
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 

model_rf = train(Difference.From.Fresh ~ Sample.Age..Weeks. + Moisture.... + Processing.Agent.Stability.Index, data=trainpepsi, method='rf', tuneLength=5, trControl = fitControl)















model_svmRadial = train(Difference.From.Fresh ~ ., data=trainpepsi, method='svmRadial', tuneLength=15, trControl = fitControl)
