# Load essential libraries
install.packages("MASS")
install.packages("car")
library(ggplot2)
library(lubridate)
library(tidyr)
library(MASS)
library(car)

#Load the car price data
cars_data <- read.csv("CarPrice_Assignment.csv")
##Data Cleaning And Preparation##
#Count Number of rows
nrow(cars_data)

#Checking for NA values in the file
which(is.na(cars_data) == TRUE)
#There is no NA value

#Checking for Duplicate value in the file
which(duplicated(cars_data) == TRUE)
#No rows are duplicate

#Removing not needed variable
cars_data$car_ID <- NULL

#Creating a new column for car company name
cars_data <-separate(cars_data,CarName, c("CarCompanyName"), sep = " ", remove=FALSE)
cars_data$CarName <- NULL

#Correcting the car names
cars_data[which(cars_data$CarCompanyName == "maxda"),"CarCompanyName"] <- "mazda"
cars_data[which(cars_data$CarCompanyName == "Nissan"),"CarCompanyName"] <- "nissan"
cars_data[which(cars_data$CarCompanyName == "porcshce"),"CarCompanyName"] <- "porsche"
cars_data[which(cars_data$CarCompanyName == "toyouta"),"CarCompanyName"] <- "toyota"
cars_data[which(cars_data$CarCompanyName == "vokswagen" | cars_data$CarCompanyName == "vw"),"CarCompanyName"] <- "volkswagen"


write.csv(cars_data,"cars.csv")

#Dummy variable creation

#Changing FuelType to numeric variable 
str(cars_data$fueltype)
summary(factor(cars_data$fueltype))
levels(cars_data$fueltype) <- c(0,1)
#Converting it to numeric
cars_data$fueltype <- as.numeric(levels(cars_data$fueltype))[cars_data$fueltype]

#Doing same for Aspiration
str(cars_data$aspiration)
summary(factor(cars_data$aspiration))
levels(cars_data$aspiration) <- c(1,0)
#Converting it to numeric
cars_data$aspiration <- as.numeric(levels(cars_data$aspiration))[cars_data$aspiration]

#Doing same for DoorNumber
str(cars_data$doornumber)
summary(factor(cars_data$doornumber))
levels(cars_data$doornumber) <- c(1,0)
#Converting it to numeric
cars_data$doornumber <- as.numeric(levels(cars_data$doornumber))[cars_data$doornumber]

#Doing same for EngineLocation
str(cars_data$enginelocation)
summary(factor(cars_data$enginelocation))
levels(cars_data$enginelocation) <- c(1,0)
#Converting it to numeric
cars_data$enginelocation <- as.numeric(levels(cars_data$enginelocation))[cars_data$enginelocation]

#Categorical variables with more than two levels

#Creating dummy variable for carbody
dummy_1 <- data.frame(model.matrix(~carbody, data = cars_data))
dummy_1 <- dummy_1[,-1]
cars_data$carbody <- NULL
cars_data <- cbind(cars_data, dummy_1)

#Creating dummy variable for drivewheel
dummy_2 <- data.frame(model.matrix(~drivewheel, data = cars_data))
dummy_2 <- dummy_2[,-1]
cars_data$drivewheel <- NULL
cars_data <- cbind(cars_data, dummy_2)

#Doing same for enginetype
summary(cars_data$enginetype)
dummy_3 <- data.frame(model.matrix(~enginetype, data = cars_data))
dummy_3 <- dummy_3[,-1]
cars_data$enginetype <- NULL
cars_data <- cbind(cars_data, dummy_3)

#Doing same for FuelSystem
summary(cars_data$fuelsystem)
dummy_4 <- data.frame(model.matrix(~fuelsystem, data = cars_data))
dummy_4 <- dummy_4[,-1]
cars_data$fuelsystem <- NULL
cars_data <- cbind(cars_data, dummy_4)


#Doing same for cylindernumber
summary(cars_data$cylindernumber)
dummy_5 <- data.frame(model.matrix(~cylindernumber, data = cars_data))
dummy_5 <- dummy_5[,-1]
cars_data$cylindernumber <- NULL
cars_data <- cbind(cars_data, dummy_5)

##Deriving Metrics
#Calculating car area from length , width and ht of the car
cars_data$CarArea <- cars_data$carlength * cars_data$carwidth * cars_data$carheight

#Derive new metric of averagempg
cars_data$AverageMPG <- (cars_data$citympg + cars_data$highwaympg)/2

# Scatter Plot (CarCompanyName vs price) To see average cost of cars of each company
ggplot(cars_data, aes(CarCompanyName,price)) + geom_bar(stat = "summary", position = "stack",fun.y = mean)

# Scatter Plot (CarCompanyName vs price vs fueltype) To see average cost of cars of each company
ggplot(cars_data, aes(CarCompanyName,price, col=fueltype)) + geom_bar(stat = "summary", position = "stack",fun.y = mean)
#This shows a very important trend ,only 8 company produces diesel variant, so in US mostly people prefer gas cars

# Scatter Plot (symboling vs price) To see average cost of cars of each company
ggplot(cars_data, aes(symboling,price)) + geom_bar(stat = "summary", position = "stack",fun.y = mean)

##Now let start building our model using the Step Wise Selection Method
cars_data$CarCompanyName <- NULL
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(cars_data), 0.7*nrow(cars_data))
train = cars_data[trainindices,]
test = cars_data[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~ .,data=train)
summary(model_1)


#Now lets do the step AIC method
step <- stepAIC(model_1, direction="both")


#Now lets make second model by removing all variable discarded in step aic step
model_2 <- lm(formula = price ~ boreratio + fuelsystemspdi + enginetypeohc + enginetypeohcv + carwidth
              +carbodysedan +carbodyhardtop +enginetypel+carheight + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation+enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_2)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_2)

#So boreratio has very high VIF and high p value  > 0.05 so we will remove it and build a new model
model_3 <- lm(formula = price ~  fuelsystemspdi + enginetypeohc + enginetypeohcv + carwidth
              +carbodysedan +carbodyhardtop +enginetypel+carheight + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation+enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_3)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_3)

#So enginetypeohc has  not very high VIF but high p value  > 0.05 so we will remove it and see adjusted r square
model_4 <- lm(formula = price ~  fuelsystemspdi   + enginetypeohcv + carwidth
              +carbodysedan +carbodyhardtop +enginetypel+carheight + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation+enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_4)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_4)
#Adjusted r square does not move much so we can remove enginetypeohc

#So enginetypeohcv has  not very high VIF but is one * so we will remove it and see adjusted r square
model_5 <- lm(formula = price ~  fuelsystemspdi    + carwidth
              +carbodysedan +carbodyhardtop +enginetypel+carheight + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation+enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_5)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_5)
#Adjusted r square does not move much so we can remove enginetypeohcv

#So carbodysedan has  not very high VIF but is one * so we will remove it and see adjusted r square
model_6 <- lm(formula = price ~  fuelsystemspdi    + carwidth
              +carbodyhardtop +enginetypel+carheight + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation+enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_6)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_6)
#Adjusted r square does not move much so we can remove carbodysedan


#So carwidth has  not very high VIF but have 2 * so we will remove it and see adjusted r square
model_7 <- lm(formula = price ~  fuelsystemspdi    
              +carbodyhardtop +enginetypel+carheight + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation
              +enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_7)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_7)

#So carheight has  high VIF but only one * so we will remove it and see adjusted r square
model_8 <- lm(formula = price ~  fuelsystemspdi    
              +carbodyhardtop +enginetypel + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+carlength+stroke+cylindernumbersix +cylindernumberfive+enginelocation
              +enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_8)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_8)

#So carlength has  not very high VIF but 2 * so we will remove it and see adjusted r square
model_9 <- lm(formula = price ~  fuelsystemspdi    
              +carbodyhardtop +enginetypel + peakrpm+curbweight+aspiration+carbodyhatchback
              +carbodywagon+CarArea+stroke+cylindernumbersix +cylindernumberfive+enginelocation
              +enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_9)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_9)


#So curbweight has  not very high VIF but one * so we will remove it and see adjusted r square
model_10 <- lm(formula = price ~  fuelsystemspdi    
              +carbodyhardtop +enginetypel + peakrpm+aspiration+carbodyhatchback
              +carbodywagon+CarArea+stroke+cylindernumbersix +cylindernumberfive+enginelocation
              +enginesize+cylindernumberfour,
              data = train)
# Let us look at the summary of the model
summary(model_10)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_10)

#Remove fuelsystemspdi  ,carbodyhardtop +enginetypel + carbodyhatchback
model_11 <- lm(formula = price ~ peakrpm+aspiration
               +carbodywagon+CarArea+stroke+cylindernumbersix +cylindernumberfive+enginelocation
               +enginesize+cylindernumberfour,
               data = train)
# Let us look at the summary of the model
summary(model_11)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_11)

#Remove carbodywagon  ,cylindernumberfive
model_12 <- lm(formula = price ~ peakrpm+aspiration
               +CarArea+stroke+cylindernumbersix +enginelocation
               +enginesize+cylindernumberfour,
               data = train)
# Let us look at the summary of the model
summary(model_12)
#Now calculate the VIF of the model and remove the variable with very high VIF values
vif(model_12)

# predicting the results in test dataset
Predict_1 <- predict(model_12,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#So we can say the predicted values from the model are able to explain 75.5% variation in the actual outcomes
#And the Parameter that the chinese company look for are given below
#1)PeakRPM
#2)Aspiration
#3)CarArea
#4)Stroke
#5)Number of cylinder - 6 or 4 are good
#6)Engine Location
#7)Engine Size




