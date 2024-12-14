#Load data set and view its structure
data_frame <- read.csv("used_car_dataset.csv",na.strings="")
str(data_frame)

#Checking and removing Nulls
sum(is.na(data_frame))
data_frame <- na.omit(data_frame)

# Removing duplicates
data_frame <- unique(data_frame)

#Changing PostedDate column into date format for later analysis
data_frame$PostedDate <- paste0(data_frame$PostedDate, "-15")
data_frame$PostedDate <- as.Date(strptime(data_frame$PostedDate, format="%b-%y-%d"))

#Extracting year from PostedDate
data_frame$PostedYear <- as.integer(format(data_frame$PostedDate, "%Y"))

#Discovering and fixing Age inconsistency isssue
unique(data_frame$Year + data_frame$Age == data_frame$PostedYear)
data_frame$Age = (data_frame$PostedYear - data_frame$Year)

#Checking nominal values
unique(data_frame$Brand)
unique(data_frame$kmDriven)
unique(data_frame$model)
unique(data_frame$Transmission)
unique(data_frame$Owner)
unique(data_frame$FuelType)

#Changing numeric strings into numeric
data_frame$kmDriven = gsub(",", "", data_frame$kmDriven)
data_frame$kmDriven = sub("km", "", data_frame$kmDriven)
data_frame$kmDriven = as.integer(data_frame$kmDriven)

data_frame$AskPrice = gsub(",", "", data_frame$AskPrice)
data_frame$AskPrice = sub("₹", "", data_frame$AskPrice)
data_frame$AskPrice = as.integer(data_frame$AskPrice)

#Removing incorrect data
data_frame <- data_frame[!(data_frame$kmDriven == 0 & data_frame$Age > 0 & data_frame$Owner == "second"),]

#Fixing numeric data outliers problem
boxplot(data_frame$kmDriven)
kmDriven.Q1 <- quantile(data_frame$kmDriven, 0.25)
kmDriven.Q3 <- quantile(data_frame$kmDriven, 0.75)
kmDriven.IQR <- IQR(data_frame$kmDriven)
data_frame$kmDriven[data_frame$kmDriven < kmDriven.Q1 - 1.5*kmDriven.IQR] <- kmDriven.Q1 - 1.5*kmDriven.IQR
data_frame$kmDriven[data_frame$kmDriven > kmDriven.Q3 + 1.5*kmDriven.IQR] <- kmDriven.Q3 + 1.5*kmDriven.IQR

boxplot(data_frame$AskPrice)
AskPrice.Q1 <- quantile(data_frame$AskPrice, 0.25)
AskPrice.IQR <- IQR(data_frame$AskPrice)
data_frame$AskPrice[data_frame$AskPrice < AskPrice.Q1 - 1.5*AskPrice.IQR] = AskPrice.Q1 - 1.5*AskPrice.IQR

#Log transformation for ask price to reduce high outliers impact
data_frame$LogAskPrice = log(data_frame$AskPrice)
