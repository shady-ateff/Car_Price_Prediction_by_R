install.packages("randomForest")
library(randomForest)
# Step 1: Train-Test Split
set.seed(123)
n <- nrow(data_frame)
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- data_frame[train_indices, ]
test_data <- data_frame[-train_indices, ]

# Step 2: Train Random Forest Model
sum(is.na(data_frame))
rf_model <- randomForest(AskPrice ~ ., data = train_data, ntree = 100)

# Step 3: Make Predictions

predictions_rf <- predict(rf_model, newdata = test_data)

# Step 4: Model Evaluation

evaluate <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))  # Mean Absolute Error
  mse <- mean((actual - predicted)^2)  # Mean Squared Error
  r2 <- cor(actual, predicted)^2       # Coefficient of Determination (R²)
  return(c(MAE = mae, MSE = mse, R2 = r2))
}

# Actual values from test set
actual_values <- test_data$AskPrice

# Evaluate Random Forest Model
rf_metrics <- evaluate(actual_values, predictions_rf)

cat("\nRandom Forest Evaluation Metrics:\n")
print(rf_metrics)

# Accuracy Calculation (using MAPE)
calculate_accuracy <- function(actual, predicted) {
  mape <- mean(abs((actual - predicted) / actual)) * 100  # Mean Absolute Percentage Error
  accuracy <- 100 - mape  # Accuracy as 100 - MAPE
  return(accuracy)
}

# Calculate Random Forest Accuracy
rf_accuracy <- calculate_accuracy(actual_values, predictions_rf)
cat("\nRandom Forest Accuracy (%): ", rf_accuracy, "\n")

# Step 5: Visualization
# 1. Actual vs Predicted Scatter Plot
plot(actual_values, predictions_rf,
     main = "Actual vs Predicted Prices (Random Forest)",
     xlab = "Actual Prices", ylab = "Predicted Prices",
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)  # Line y = x for reference

# 2. Residual Plot
residuals_rf <- actual_values - predictions_rf
plot(predictions_rf, residuals_rf,
     main = "Residual Plot (Random Forest)",
     xlab = "Predicted Prices", ylab = "Residuals",
     col = "green", pch = 16)
abline(h = 0, col = "red", lwd = 2)  # Reference line at residual = 0

