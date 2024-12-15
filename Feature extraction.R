
library(dplyr)

# Load cleaned dataset
data_frame <- read.csv("cleaned_data.csv")
head(data_frame)
str(data_frame)
# Define luxury brands
luxury_brands <- c("BMW", "Mercedes-Benz", "Audi", "Tesla", "Porsche", "Volvo", 
                   "Lexus", "Ferrari", "Lamborghini", "Aston Martin", "Bentley", 
                   "Ford", "Rolls-Royce", "Land Rover")

# Feature engineering
data_frame <- data_frame %>%
  mutate(
    isLuxury = as.numeric(Brand %in% luxury_brands),
    kmPerYear = kmDriven / Age,
    Brand_model = as.numeric(as.factor(paste(Brand, model))),
    Transmission = as.numeric(as.factor(Transmission)),
    Owner = as.numeric(as.factor(Owner)),
    FuelType = as.numeric(as.factor(FuelType))
  ) %>%
  select(-c(Brand, model, PostedDate, PostedYear, Year))
head(data_frame)
str(data_frame)
# Save data with features
write.csv(data_frame, "feature_extraction_data.csv", row.names=FALSE)
