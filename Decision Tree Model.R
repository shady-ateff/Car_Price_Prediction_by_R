# Load libraries
library(rpart)
library(rpart.plot)
library(caret)

# Load feature-engineered data (already processed as per Feature Engineering script)
data <- read.csv("G:/org/gittest/Car_Price_Prediction_by_R/feature_extraction_data.csv", stringsAsFactors = TRUE)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Tune Decision Tree hyperparameters using caret
tuned_params <- train(
  LogAskPrice ~ ., 
  data = train_data, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = expand.grid(cp = seq(0.001, 0.02, by = 0.001))
)

# Use the best model parameters
best_cp <- tuned_params$bestTune$cp
dt_model <- rpart(LogAskPrice ~ ., 
                  data = train_data,
                  method = "anova",
                  control = rpart.control(
                    minsplit = 20,
                    minbucket = 7,
                    maxdepth = 10,
                    cp = best_cp
                  ))

# Visualize the decision tree
rpart.plot(dt_model, 
           box.palette = "RdBu",
           shadow.col = "gray",
           nn = TRUE)

# Make predictions on the test set
predictions <- predict(dt_model, newdata = test_data)

# Evaluate model performance
rmse <- sqrt(mean((test_data$LogAskPrice - predictions)^2))
r2 <- 1 - sum((test_data$LogAskPrice - predictions)^2) / 
  sum((test_data$LogAskPrice - mean(test_data$LogAskPrice))^2)

# Print results
cat("RMSE (log scale):", round(rmse, 4), "\n")
cat("R-squared:", round(r2, 4), "\n")

# Convert predictions and actual values back to the original scale
original_predictions <- exp(predictions)
original_actual <- exp(test_data$LogAskPrice)

# Calculate accuracy (percentage of predictions within 20% of actual values)
accuracy <- mean(abs((original_actual - original_predictions) / original_actual) <= 0.20) * 100
cat("Model Accuracy:", round(accuracy, 2), "%\n")

