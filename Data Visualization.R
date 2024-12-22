#We visualize the numeric data in our data frame
par(mfrow=c(1,3))
boxplot(data_frame$kmDriven, main="Km Driven")
boxplot(data_frame$Age, main="Age")
boxplot(data_frame$AskPrice, main="Ask Price")

#We then visualize our newly created features
par(mfrow=c(1,2))
boxplot(data_frame$kmPerYear, main="Km per Year")
boxplot(data_frame$PostedMonths, main="Posted Months")

#We now further inspect the non-normalized distributions
par(mfrow=c(1,1))
hist(data_frame$Age, main="Distribution of Age", col="lightgreen", xlab="Age (Years)")
hist(data_frame$AskPrice, main="Distribution of Asking Price", col="lightcoral", xlab="Price")
hist(data_frame$kmPerYear, main="km Driven Per Year", breaks=20,  col="lightpink", xlab="kmPerYear")

#We now view categorial data distributions
par(mar = c(6, 4, 4, 2)) 
barplot(table(data_frame$Brand), las=2, col="steelblue", main="Count of Cars by Brand", cex.names = 0.8)
par(mar = c(5, 4, 4, 2), mfrow=c(1,3))
barplot(table(data_frame$FuelType), col="orange", main="Fuel Type Distribution", cex.names = 0.94)
barplot(table(data_frame$Transmission), col="lightgreen", main="Transmission Type")
barplot(table(data_frame$Owner), col="purple", main="Owner Type")

#We then view some relations between numeric data
par(mfrow=c(1,2))
plot(data_frame$Age, data_frame$AskPrice, main="Price vs Age of Car", xlab="Age (Years)", ylab="Asking Price", col="blue", pch=16)
plot(data_frame$kmDriven, data_frame$AskPrice, main="Price vs km Driven", xlab="kmDriven", ylab="Asking Price", col="red", pch=16)

par(mfrow=c(1,2))
plot(data_frame$Age, data_frame$AskPrice, main="Price vs Km per Year", xlab="Km per Year", ylab="Asking Price", col="green", pch=16)
plot(data_frame$kmDriven, data_frame$AskPrice, main="Price vs Posted Months", xlab="Posted Months", ylab="Asking Price", col="orange", pch=16)

#After labal encoding, we now create the correlation matrix and view heat map
cor_matrix <- cor(encoded_data, use="complete.obs")
heatmap(cor_matrix, main="Correlation Heatmap", col=colorRampPalette(c("blue", "white", "red"))(100), scale="column")

#We lastly create bar plot for correlation with ask price
par(mfrow=c(1,1))
correlations <- sapply(encoded_data, function(col) cor(col, encoded_data$AskPrice, use="pairwise.complete.obs"))
correlations <- abs(correlations[names(correlations) != "AskPrice"])
barplot(correlations, main = "Absolute Correlation of Each Column with AskPrice",ylab = "Correlation Coefficient",col = "skyblue", las = 2, cex.names = 0.7)
