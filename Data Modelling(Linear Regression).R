library(caret)

# Data Splitting for Model
# Set seed for reproducibility
# Split data into training (80%) and testing (20%) sets 
trainIndex <- createDataPartition(encoded_data$LogAskPrice, p = 0.8, list = FALSE)
trainData <- encoded_data[trainIndex, ]
testData <- encoded_data[-trainIndex, ]
na.omit(trainData)
na.omit(testData)

# Modelling
model <- lm(LogAskPrice ~ Age+kmDriven+Transmission+Owner+FuelType+isLuxury+kmPerYear, data = trainData)
summary(model)
summary(model)$r.squared

# Make predictions on the test data 
predictions <- predict(model, newdata=testData)

# Calculating RMSE
error_lr<-testData$LogAskPrice-predictions
RMSE_lr<-sqrt(mean(error_lr^2))
RMSE_lr

# Plotting Predicited price vs Actual price
plot(testData$LogAskPrice,predicted_price, main="Plot of Actual vs Predicted Selling Price", 
     col = c("darkblue","red"), 
     xlab = "Actual Car Selling Price", 
     ylab = "Predicted Car Selling Price")

plot(model)
