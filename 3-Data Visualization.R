#We visualize the numeric data in our data frame
par(mfrow=c(1,3))
boxplot(data_frame$kmDriven, main="Km Driven")
boxplot(data_frame$Age, main="Age")
boxplot(data_frame$AskPrice, main="Ask Price")

#We then visualize our newly created features
par(mfrow=c(1,3))
boxplot(data_frame$kmPerYear, main="Km per Year")
boxplot(data_frame$PostedMonths, main="Posted Months")
boxplot(data_frame$LogAskPrice, main="Log Ask Price")

par(mfrow=c(1,1))
hist(data_frame$kmDriven, main="Distribution of kmDriven", col="skyblue", xlab="kmDriven")
hist(data_frame$Age, main="Distribution of Age", col="lightgreen", xlab="Age (Years)")
hist(data_frame$LogAskPrice, main="Distribution of Asking Price", col="lightcoral", xlab="Price")
hist(data_frame$kmPerYear, main="km Driven Per Year", breaks=20,  col="lightpink", xlab="kmPerYear")










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
