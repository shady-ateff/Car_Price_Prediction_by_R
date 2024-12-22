#We diffrentiate between luxurious and regular brands
luxury_brands <- c("BMW", "Mercedes-Benz", "Audi","Tesla","Porsche","Volvo","Lexus","Ferrari", "Lamborghini","Aston Martin","Bentley","Ford","Rolls-Royce" ,"Land Rover")
data_frame$isLuxury <- data_frame$Brand %in% luxury_brands

#We extract how many Km were driven per year
data_frame$kmPerYear <- data_frame$kmDriven / data_frame$Age
data_frame$kmPerYear[is.na(data_frame$kmPerYear)] <- 0
data_frame$kmPerYear[is.infinite(data_frame$kmPerYear)] <- data_frame$kmDriven[is.infinite(data_frame$kmPerYear)]

#We derive months since 1 month before earliest date from posted date
#to provide useful integer data for models
min(data_frame$PostedDate)
data_frame$PostedMonths <- (as.integer(format(data_frame$PostedDate, "%Y")) - 2023)*12 + (as.integer(format(data_frame$PostedDate, "%m")) - 11)

#We combine bith brand and model into 1 feature
data_frame$Brand.model <-paste0(data_frame$Brand,"-",data_frame$model)


#After some visualization, we label encode data
encoded_data <- data_frame
for (col in names(encoded_data)) if (!is.numeric(encoded_data[[col]])) encoded_data[[col]] <- as.numeric(as.factor(encoded_data[[col]]))
encoded_data <- subset(encoded_data, select = -c(Year, PostedDate, AdditionInfo, PostedYear))


#After Completing Visualization Step:
#We apply log transormation on km per Year and Ask Price
encoded_data$LogkmPerYear = ifelse(encoded_data$kmPerYear == 0,0,log(encoded_data$kmPerYear))
encoded_data$LogAskPrice = ifelse(encoded_data$AskPrice==0,0,log(encoded_data$AskPrice))

#We now look at the correlation with log ask price
encoded_data$kmPerYear <- NULL
encoded_data$AskPrice <- NULL
correlations <- sapply(encoded_data, function(col) cor(col, encoded_data$LogAskPrice, use="pairwise.complete.obs"))
correlations <- abs(correlations[names(correlations) != "LogAskPrice"])
barplot(correlations, main = "Absolute Correlation of Each Column with LogAskPrice",ylab = "Correlation Coefficient",col = "skyblue", las = 2, cex.names = 0.7)

#We filter out features with low correlation with log ask price
encoded_data <- subset(encoded_data, select = -c(Brand, model, PostedMonths, Brand.model))
