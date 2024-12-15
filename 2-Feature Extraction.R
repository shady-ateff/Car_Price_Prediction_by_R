#We diffrentiate between luxurious and regular brands
luxury_brands <- c("BMW", "Mercedes-Benz", "Audi","Tesla","Porsche","Volvo","Lexus","Ferrari", "Lamborghini","Aston Martin","Bentley","Ford","Rolls-Royce" ,"Land Rover")
data_frame$isLuxury <- as.numeric(as.factor(data_frame$Brand %in% luxury_brands))

#We extract how many Km were driven per year
data_frame$kmPerYear <- data_frame$kmDriven / data_frame$Age
data_frame$kmPerYear[is.na(data_frame$kmPerYear)] <- 0
data_frame$kmPerYear[is.infinite(data_frame$kmPerYear)] <- data_frame$kmDriven[is.infinite(data_frame$kmPerYear)]

#We derive months since 1 month before earliest date from posted date
#to provide useful integer data for models
min(data_frame$PostedDate)
data_frame$PostedMonths <- (as.integer(format(data_frame$PostedDate, "%Y")) - 2023)*12 + (as.integer(format(data_frame$PostedDate, "%m")) - 11)

#We combine bith brand and model into 1 feature
data_frame$Brand.model <-as.numeric(as.factor(paste0(data_frame$Brand,"-",data_frame$model)))
