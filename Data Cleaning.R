#Load data set
data_frame <- read.csv("used_car_dataset.csv")

#Checking data set structure and nulls
str(data_frame)
sum(is.na(data_frame))

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
unique(data_frame$model)
unique(data_frame$Transmission)
unique(data_frame$Owner)
unique(data_frame$FuelType)

#Changing numeric strings into numeric
data_frame$kmDriven = gsub(",", "", data_frame$kmDriven)
data_frame$kmDriven = sub("km", "", data_frame$kmDriven)
data_frame$kmDriven = as.numeric(data_frame$kmDriven)
data_frame$AskPrice = gsub(",", "", data_frame$AskPrice)
data_frame$AskPrice = sub("₹", "", data_frame$AskPrice)
data_frame$AskPrice = as.numeric(data_frame$AskPrice)

