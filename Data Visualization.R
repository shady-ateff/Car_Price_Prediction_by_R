install.packages("corrplot")
install.packages("pheatmap")
install.packages("caret")
library(caret)
dummies <- dummyVars(~., data = data_frame)

# Predict the dummy variables and bind them to the original dataset
data_frame_encoded <- predict(dummies, newdata = data_frame)

# Convert the result to a data frame
data_frame_encoded <- as.data.frame(data_frame_encoded)

plot(data_frame)

boxplot(data_frame$kmDriven ,data_frame$AskPrice ,data_frame$Age)

plot(data_frame$kmDriven ,data_frame$AskPrice ,data_frame$Age)

hist(data_frame$AskPrice/100000,breaks = 1550)

par(mfcol=c(2,3))


correlation_matrix <- cor(data_frame[-8], use = "complete.obs") # `use = "complete.obs"` ignores NA
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20), 
        scale = "none", 
        main = "Correlation Matrix")
