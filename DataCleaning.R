# Load dataset
data_frame <- read.csv("used_car_dataset.csv")

# Checking data structure and nulls
str(data_frame)
sum(is.na(data_frame))

# Handle missing values (example: replacing with median or mode)
data_frame[is.na(data_frame)] <- NA  # Adjust per variable requirements

# Changing PostedDate column into date format
data_frame$PostedDate <- paste0(data_frame$PostedDate, "-15")
data_frame$PostedDate <- as.Date(strptime(data_frame$PostedDate, format="%b-%y-%d"))

# Extracting year from PostedDate
data_frame$PostedYear <- as.numeric(format(data_frame$PostedDate, "%Y"))

# Discovering and fixing Age inconsistency
unique(data_frame$Year + data_frame$Age == data_frame$PostedYear)
data_frame$Age = (data_frame$PostedYear - data_frame$Year)

# Standardizing nominal values
#library(stringr)
data_frame$Brand <- str_to_lower(str_trim(data_frame$Brand))
data_frame$Model <- str_to_lower(str_trim(data_frame$Model))
data_frame$Transmission <- str_to_lower(str_trim(data_frame$Transmission))
data_frame$Owner <- str_to_lower(str_trim(data_frame$Owner))
data_frame$FuelType <- str_to_lower(str_trim(data_frame$FuelType))

# Removing duplicates
data_frame <- unique(data_frame)

# Checking outliers (example for numeric columns)
boxplot(data_frame$Price, main = "Boxplot of Price")  # Replace with actual variable
