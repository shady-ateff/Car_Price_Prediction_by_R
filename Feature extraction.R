
luxury_brands <- c("BMW", "Mercedes-Benz", "Audi","Tesla","Porsche","Volvo","Lexus","Ferrari", "Lamborghini","Aston Martin","Bentley","Ford","Rolls-Royce" ,"Land Rover")
data_frame$isLuxury <- as.numeric(as.factor(data_frame$Brand %in% luxury_brands))

data_frame$kmPerYear <- data_frame$kmDriven / data_frame$Age

data_frame$AdditionInfo <- NULL
data_frame$PostedDate<-NULL
data_frame$PostedYear<-NULL
data_frame$Year<-NULL

data_frame$Brand.model <-as.numeric(as.factor(paste0(data_frame$Brand,"-",data_frame$model)))
data_frame$Brand<-NULL
data_frame$model<-NULL
data_frame$Transmission<-as.numeric(as.factor(data_frame$Transmission))
data_frame$Owner<-as.numeric(as.factor(data_frame$Owner))
data_frame$FuelType<-as.numeric(as.factor(data_frame$FuelType))
