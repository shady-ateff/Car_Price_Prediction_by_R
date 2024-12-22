Sys.setlocale("LC_TIME", "C")  # Set to English for date parsing"convert NOV -> 11"
#Load data set and view its structure
data_frame <- read.csv("used_car_dataset.csv",na.strings="")
str(data_frame)
library(dplyr)

# Data cleaning
data_frame <- na.omit(data_frame)  

# Removing duplicates
data_frame <- unique(data_frame)

# Remove unnecessary columns
data_frame <- data_frame %>%
  select(-c(AdditionInfo))  

#Changing PostedDate column into date format for later analysis
Sys.setlocale("LC_TIME", "C")  # Set to English for date parsing"convert NOV -> 11"
data_frame$PostedDate <- paste0(data_frame$PostedDate, "-15")
data_frame$PostedDate <- as.Date(strptime(data_frame$PostedDate, format="%b-%y-%d"))

# Fix numeric columns
data_frame$kmDriven <- as.numeric(gsub(",", "", sub(" km", "", data_frame$kmDriven)))
data_frame$AskPrice <- as.numeric(gsub(",", "", sub("₹", "", data_frame$AskPrice)))

# Fix inconsistencies in Age
data_frame$PostedDate <- as.Date(paste0(data_frame$PostedDate, "-15"), format="%b-%y-%d")
data_frame$PostedYear <- as.integer(format(data_frame$PostedDate, "%Y"))
data_frame$Age <- data_frame$PostedYear - data_frame$Year

# Filter out invalid data
data_frame <- data_frame %>%
  filter(kmDriven > 0, Age >= 0)



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

head(data_frame)
str(data_frame)
# Save cleaned data
write.csv(data_frame, "cleaned_data.csv", row.names=FALSE)

