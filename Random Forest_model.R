install.packages("randomForest")
library(dplyr)
library(randomForest)
library(caret)

# Load  featured dataset
data_frame <- read.csv("feature_extraction_data.csv")

# Replace Inf values with the column mean
data_frame[] <- lapply(data_frame, function(x) {
  x[is.infinite(x)] <- mean(x[!is.infinite(x)], na.rm = TRUE)
  return(x)
})

# Step 1: Train-Test Split
set.seed(123)
n <- nrow(data_frame)
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- data_frame[train_indices, ]
test_data <- data_frame[-train_indices, ]

# Step 2: Train Random Forest Model

rf_model <- randomForest(AskPrice ~ ., data = train_data, ntree = 300,mtry=3)

# Step 3: Make Predictions

predictions_rf <- predict(rf_model, newdata = test_data)

# Step 4: Model Evaluation

evaluate <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))  # Mean Absolute Error
  mse <- mean((actual - predicted)^2)  # Mean Squared Error
  r2 <- cor(actual, predicted)^2       # Coefficient of Determination (R²)
  mae <- format(mae, scientific = FALSE, digits = 2)
  mse <- format(mse, scientific = FALSE, digits = 2)
  r2 <- format(r2, scientific = FALSE, digits = 3)
  return(c(MAE = mae, MSE = mse, R2 = r2))
}

# Actual values from test set
actual_values <- test_data$AskPrice

# Evaluate Random Forest Model
rf_metrics <- evaluate(actual_values, predictions_rf)

cat("\nRandom Forest Evaluation Metrics:\n")

cat("MAE: ", rf_metrics["MAE"], "\n")
cat("MSE: ", rf_metrics["MSE"], "\n")
cat("R²: ", rf_metrics["R2"], "\n")

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

# Step 6: Check Actual vs Predicted Values
comparison_df <- data.frame(Actual = actual_values, Predicted = predictions_rf)
cat("\nComparison of Actual vs Predicted Values:\n")
print(head(comparison_df))  # Show the first few rows of the comparison


