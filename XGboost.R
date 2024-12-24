## Load Required Libraries ##
library(xgboost)
library(caret)

## Split the data to train(80%) and test(20%) ##
#set.seed(123)
trainIndex <- sample(1:nrow(encoded_data), 0.8 * nrow(encoded_data))
trainData <- encoded_data[trainIndex, ]  # Training set
testData <- encoded_data[-trainIndex, ]  # Test set

trainLabel <- encoded_data$LogAskPrice[trainIndex]
testLabel <- encoded_data$LogAskPrice[-trainIndex]

## Exclude the logAskPrice column ##
featuresTrain <- trainData[, !colnames(trainData) %in% "LogAskPrice"]
featuresTest <- testData[, !colnames(testData) %in% "LogAskPrice"]
trainMatrix <- as.matrix(featuresTrain)
testMatrix <- as.matrix(featuresTest)

# Ensure trainLabel is numeric
train_label <- as.numeric(trainLabel)

# Verify data dimensions
if (nrow(trainMatrix) != length(trainLabel)) {
  stop("Mismatch between rows in trainData and length of trainLabel")
}

## Applying XGboost data model ##
XGB_model <- xgboost(data = trainMatrix,
                     label = trainLabel,
                     nrounds = 100,
                     objective = "reg:squarederror",
                     verbose = 0)
 


## Predict the Target col ##
predictions <- predict(XGB_model, testMatrix)

## calculate the residuals (rmse) ##
rmse <- sqrt(mean((predictions - testLabel)^2))
mae <- mean(abs(predictions - testLabel))
ss_total <- sum((testLabel - mean(testLabel))^2)
ss_residual <- sum((testLabel - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)

print(paste("RMSE:", format(rmse, scientific = FALSE)))
print(paste("MAE:", format(mae, scientific = FALSE)))
print(paste("R-squared:", format(r_squared, scientific = FALSE)))










