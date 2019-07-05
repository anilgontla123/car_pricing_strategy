rm(list=ls())
setwd("G:/Upgrad/Course3/LinearRegression/Assignment")

#load libraries
library(ggplot2)
library(tidyr)
library(dbplyr)
library(caret)
library(MASS)
library(car)
library(stringr)


# Read the car dataset and check the structure
cars<- read.csv("CarPrice_Assignment.csv")
str(cars)


############################################################
#Data Understanding
##############################################################
# The given data set is related to car modeling.Below are factors we do have to consider:
# price represents the sale price of a car
#car_ID--Unique id given for each observation
#symboling--Insurance risk rating factor. +3 indicates  the auto is risky & -3 that it is probably pretty safe
#car_company--REpresents the name of the car company
#car_model--Represents the model of a car     
#fueltype--This represents whether the car is of type fuel or diesel
#aspiration--the type of aspirations of a car i.e.,"std"/"turbo"
#doornumber-- No.of door in a car
#carbody-- Represents the type of car body
#drivewheel--Drive wheel of a car whether "fwd"/"rwd"
#enginelocation --Where the engine is located i.e., at "front" or "rear"
# wheelbase-- Wheel base of a car 
#carlength  --Represents Length of a car
#carwidth -- Represents Width of a car
#carheight--Represents height of a car
#curbweight -- Represents weight of a car
#enginetype --Type of engine located in a car
#cylindernumber-- No of cylinders placed in a cr
#enginesize --The size of the engine
#fuelsystem    TYpe of fuel system in a car
#boreratio-- Bore ratio of car which gives the engine efficiency
#stroke-- the volumn inside the engine
#compressionratio: Compression ratio of car
#horsepower  --Horse POwer of a car
#peakrpm  --MAximum engine speed
#citympg --Mileage of a car in city traffic     
#highwaympg --Mileage of a car in highway



#############################################################
#Data Cleaning#
##############################################################

#identifying duplicate Car_ID's as Car_ID's must be unique
sum(duplicated(cars$car_ID))   # No duplicate car ID's

# checking for missing values in dataset
sapply(cars,function(x) length(which(is.na(x))))  # No NA values

# checking for blank values
sapply(cars,function(x) length(which(x==" ")))  # No blank values

# Check for Uppercase or Lowercase in individual columns 
summary(cars)

#Split the CarName column to 2 separate columns as per the requirement
cars<- separate(cars,CarName,into = c("car_company","car_model"),sep = " ")
str(cars)

# In car company there are some spell check issues...Hence changed into unique and correct formats
cars$car_company<- tolower(cars$car_company)
cars$car_company<-str_replace(cars$car_company,"maxda","mazda")
cars$car_company<-str_replace(cars$car_company,"alfa-romero","alfa-romeo")
cars$car_company<-str_replace(cars$car_company,"porcshce","porsche")
cars$car_company<-str_replace(cars$car_company,"toyouta","toyota")
cars$car_company<-str_replace(cars$car_company,"vokswagen","volkswagen")
cars$car_company<-str_replace(cars$car_company,"vw","volkswagen")
cars$symboling<- as.factor(cars$symboling)
str(cars)
########################################################

#Dummy Variables

########################################################
# We have to consider only company name as it is the independent variable for the model building(as per requirement)
#Hence Removing "car model"column 
#Also removed few other columns and created dummy variable and then appending these variable to main data frame

dummy_11 <- dummyVars("~ drivewheel+symboling +car_company+carbody+enginetype+cylindernumber+fuelsystem",data = cars,fullRank = T)
cars_dummy <- data.frame(predict(dummy_11,newdata = cars))
cars_main <- cbind(cars[,-c(1,2,3,4,8,9,16,17,19)],cars_dummy) 
str(cars_main)

#Replacing gas and diesel with 1 and 0 and converting it to numeric type
levels(cars_main$fueltype)<-c(0,1)
cars_main$fueltype <- as.numeric(levels(cars_main$fueltype))[cars_main$fueltype]

#Replacing Std with 1 and turbo with 0
levels(cars_main$aspiration) <- c(1,0) 
cars_main$aspiration <- as.numeric(levels(cars_main$aspiration))[cars_main$aspiration]

#Replacing four with 1 and two with 0
levels(cars_main$doornumber) <- c(1,0) 
cars_main$doornumber <- as.numeric(levels(cars_main$doornumber))[cars_main$doornumber]

levels(cars_main$enginelocation) <- c(1,0) #Replacing front with 1 and rear with 0
cars_main$enginelocation <- as.numeric(levels(cars_main$enginelocation))[cars_main$enginelocation]

###########################################
# understanding notions of dummy variables
##########################################

#carbody.hardtop -- 1 represents hardtop       #carbody.hatchback--  1 represents hatchback 
#carbody.sedan --  1 represents sedan          #carbody.wagon --  1 represents wagon

#enginelocation.rear--1 is enginelocation  rear     #enginetype.dohcv--1 represents enginetype dohcv  
#enginetype.l --1 represents engine type l          #enginetype.ohc--1 represents engine type ohc  
#enginetype.ohcf -- 1 represents engine type ohcf   #enginetype.ohcv -- 1 represents engine type ohcv 
#enginetype.rotor -- 1 represents engine type rotor 

#cylindernumber.five-- 1 represents cylinderno: 5      #cylindernumber.four -- 1 represents cylinderno: is 4
#cylindernumber.six-- 1 represents cylinderno:  6     #cylindernumber.three -- 1 represents cylinderno: is 3
#cylindernumber.twelve-- 1 represents cylinderno: 12   #cylindernumber.two -- 1 represents cylinderno: is 2

#fuelsystem.2bbl -- 1 represents fuelsystem is 2bbl    #fuelsystem.4bbl -- 1 represents fuelsystem is 4bbl   
#fuelsystem.idi --1 represents fuelsystem is idi       #fuelsystem.mfi-- 1 represents fuelsystem is mfi
#fuelsystem.mpfi -- 1 represents fuelsystem is mpfi    #fuelsystem.spdi-- 1 represents fuelsystem is spdi


#Removing  outliers
quantile(cars_main$wheelbase,seq(0,1,0.01))
cars_main$wheelbase[which(cars_main$wheelbase > 115.544)]<-115.544

quantile(cars_main$carlength,seq(0,1,0.01))
cars_main$carlength[which(cars_main$carlength > 202.480)]<-202.480

quantile(cars_main$carwidth,seq(0,1,0.01))
quantile(cars_main$carheight,seq(0,1,0.01))
quantile(cars_main$curbweight,seq(0,1,0.01))

quantile(cars_main$enginesize,seq(0,1,0.01))
cars_main$enginesize[which(cars_main$enginesize > 231.00)]<-231.00

quantile(cars_main$peakrpm,seq(0,1,0.01))
cars_main$peakrpm[which(cars_main$peakrpm > 6000)]<-6000

quantile(cars_main$citympg,seq(0,1,0.01))
cars_main$citympg[which(cars_main$citympg > 38.00)]<-38.00

quantile(cars_main$highwaympg,seq(0,1,0.01))
cars_main$highwaympg[which(cars_main$highwaympg > 46.92)]<-46.92

quantile(cars_main$horsepower,seq(0,1,0.01))
cars_main$horsepower[which(cars_main$horsepower > 184.00)]<-184.00

quantile(cars_main$boreratio,seq(0,1,0.01))
quantile(cars_main$stroke,seq(0,1,0.01))
cars_main$stroke[which(cars_main$stroke < 2.6400)]<-2.6400
cars_main$stroke[which(cars_main$stroke > 3.9000)]<-3.9000

#ggplot(cars_main,aes(cars_main$price,compressionratio))+geom_boxplot()
quantile(cars_main$compressionratio,seq(0,1,0.01))
cars_main$compressionratio[which(cars_main$compressionratio > 21.0000)]<-21.0000

# Deriving new Metrics
cars_main$EnginePower <- round((cars_main$horsepower/cars_main$curbweight),3)

str(cars_main)

###########################################################
#Model building and evaluation 
#############################################################
##Correlation Matrix
#cars_main <- round(cor(cars_main[,-c()]),3)
#cars_main[upper.tri(cars_main)] <- NA


set.seed(100)
# randomly generate row indices for train dataset & generate the train data set
trainindices= sample(1:nrow(cars_main), 0.7*nrow(cars_main))
cars_training_data = cars_main[trainindices,]

#Similarly store the rest of the observations into an object "test".
cars_test_data = cars_main[-trainindices,]


#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=cars_training_data)
summary(model_1)
#write.csv(cars_training_data,"cars_training_data.csv")

step <- stepAIC(model_1,direction = "both")
step
summary(step)

model_2 <-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyhonda + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot + 
               car_companyplymouth + car_companyrenault + car_companysaab + 
               car_companysubaru + car_companytoyota + car_companyvolkswagen + 
               car_companyvolvo + carbody.hardtop + carbody.hatchback + 
               carbody.sedan + carbody.wagon + enginetype.ohc + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_2)
sort(vif(model_2),decreasing = T)

#######################################################
# Rules followed in removing insignificant variables
########################################################

# Initially check for high VIF and validate p value:
# VIF high--- p- Value high---remove variable
# VIF high---p value is <0.05  variable is significant
# once all the VIF are approximately equal then high p value remove first
# and this process continues till all the variable comes to significant

#Removing enginetype.ohc 
model_3 <-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge + car_companyhonda + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot + 
               car_companyplymouth + car_companyrenault + car_companysaab + 
               car_companysubaru + car_companytoyota + car_companyvolkswagen + 
               car_companyvolvo + carbody.hardtop + carbody.hatchback + 
               carbody.sedan + carbody.wagon + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_3)
sort(vif(model_3),decreasing = T)
#Removing car_companyhonda
model_4 <-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge  + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot + 
               car_companyplymouth + car_companyrenault + car_companysaab + 
               car_companysubaru + car_companytoyota + car_companyvolkswagen + 
               car_companyvolvo + carbody.hardtop + carbody.hatchback + 
               carbody.sedan + carbody.wagon + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_4)
sort(vif(model_4),decreasing = T)

#Removing car_companyvolvo
model_5 <-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge  + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot + 
               car_companyplymouth + car_companyrenault + car_companysaab + 
               car_companysubaru + car_companytoyota + car_companyvolkswagen + 
               carbody.hardtop + carbody.hatchback + 
               carbody.sedan + carbody.wagon + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_5)
sort(vif(model_5),decreasing = T)
#Removing carbody.hardtop
model_6 <-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge  + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot + 
               car_companyplymouth + car_companyrenault + car_companysaab + 
               car_companysubaru + car_companytoyota + car_companyvolkswagen + 
               carbody.hatchback + 
               carbody.sedan + carbody.wagon + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_6)
sort(vif(model_6),decreasing = T)
#Removing wheelbase
model_7 <-lm(formula = price ~ aspiration + enginelocation + 
               carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + 
               car_companydodge  + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot + 
               car_companyplymouth + car_companyrenault + car_companysaab + 
               car_companysubaru + car_companytoyota + car_companyvolkswagen + 
               carbody.hatchback + 
               carbody.sedan + carbody.wagon + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_7)
sort(vif(model_7),decreasing = T)
#Removing carbody.sedan
model_8 <-lm(formula = price ~ aspiration + enginelocation + carbody.hatchback +carwidth + enginesize + stroke+drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + horsepower+
               car_companydodge  + car_companyisuzu + car_companyjaguar + car_companymazda + car_companymercury + 
               car_companymitsubishi + car_companynissan + car_companypeugeot +car_companyplymouth + car_companyrenault +
               car_companysaab +car_companysubaru + car_companytoyota + car_companyvolkswagen +cylindernumber.six
             carbody.wagon + cylindernumber.five +cylindernumber.four+ EnginePower, data = cars_training_data)

summary(model_8)
sort(vif(model_8),decreasing = T)
#Removing car_companypeugeot
model_9 <-lm(formula = price ~ aspiration + enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
               symboling.1 + car_companybmw + car_companybuick + car_companychevrolet +car_companydodge + car_companyisuzu + 
               car_companyjaguar + car_companymazda + car_companymercury + car_companymitsubishi + car_companynissan + 
               car_companyplymouth + car_companyrenault + car_companysaab +car_companysubaru + car_companytoyota +
               car_companyvolkswagen +carbody.hatchback +carbody.wagon + cylindernumber.five + 
               cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_9)
sort(vif(model_9),decreasing = T)
# Removing car_companyisuzu
model_10 <-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + car_companydodge  +  
                car_companyjaguar + car_companymazda + car_companymercury + car_companymitsubishi + car_companynissan + 
                car_companyplymouth + car_companyrenault + car_companysaab + car_companysubaru + car_companytoyota +
                car_companyvolkswagen +carbody.hatchback +carbody.wagon + cylindernumber.five + 
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_10)
sort(vif(model_10),decreasing = T)
#Removing car_companyrenaul
model_11 <-lm(formula = price ~ aspiration + enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd +  
                
                symboling.1 + car_companybmw + car_companybuick + car_companychevrolet +car_companydodge  +cylindernumber.six+
                car_companyjaguar + car_companymazda + car_companymercury +car_companymitsubishi + car_companynissan + 
                car_companyplymouth  + car_companysaab +car_companysubaru + car_companytoyota + car_companyvolkswagen + 
                carbody.hatchback + carbody.wagon + cylindernumber.five + cylindernumber.four +EnginePower, data = cars_training_data)

summary(model_11)
sort(vif(model_11),decreasing = T)
#Removing car_companysaab
model_12 <-lm(formula = price ~ aspiration + enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd +
                symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + car_companydodge  +
                car_companyjaguar + car_companymazda + car_companymercury +car_companymitsubishi +car_companyvolkswagen+
                car_companynissan + car_companyplymouth+ car_companysubaru + car_companytoyota +carbody.hatchback + 
                carbody.wagon + cylindernumber.five +cylindernumber.four + cylindernumber.six +
                EnginePower, data = cars_training_data)

summary(model_12)
sort(vif(model_12),decreasing = T)
#Removing car_companyvolkswagen
model_13 <-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd +
                symboling.1 + car_companybmw + car_companybuick + car_companychevrolet +car_companytoyota +carbody.wagon +  
                car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury + cylindernumber.five +
                car_companymitsubishi + car_companynissan + car_companyplymouth+ car_companysubaru +
                carbody.hatchback+cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_13)
sort(vif(model_13),decreasing = T)
#Removing carbody.hatchback
model_14 <-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd +
                symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + car_companytoyota + 
                car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury + car_companysubaru +  
                car_companymitsubishi + car_companynissan + car_companyplymouth+ cylindernumber.five +
                carbody.wagon +cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_14)
sort(vif(model_14),decreasing = T)
#Removing carbody.wagon
model_15 <-lm(formula = price ~ aspiration + enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                symboling.1 + car_companybmw + car_companybuick + car_companychevrolet + cylindernumber.five + 
                car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury + 
                car_companymitsubishi + car_companynissan + car_companyplymouth+car_companysubaru + car_companytoyota +
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_15)
sort(vif(model_15),decreasing = T)
#Removing car_companychevrolet
model_16 <-lm(formula = price ~ aspiration + enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                symboling.1 + car_companybmw + car_companybuick + car_companydodge  +car_companyjaguar + car_companymazda +
                car_companymercury +car_companymitsubishi + car_companynissan + car_companyplymouth+cylindernumber.six+ 
                car_companysubaru + car_companytoyota + cylindernumber.five +cylindernumber.four + 
                EnginePower, data = cars_training_data)

summary(model_16)
sort(vif(model_16),decreasing = T)
#Removing symboling.1
model_17 <-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd +
                car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury + 
                car_companymitsubishi + car_companynissan + car_companyplymouth+ cylindernumber.five + 
                car_companysubaru + car_companytoyota + car_companybmw + car_companybuick + 
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_17)
sort(vif(model_17),decreasing = T)
#Removing car_companynissan
model_18 <-lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + car_companysubaru + car_companytoyota + cylindernumber.five + 
                car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury +  car_companyplymouth+ 
                car_companymitsubishi +cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_18)
sort(vif(model_18),decreasing = T)
#Removing car_companyplymouth
model_19 <-lm(formula = price ~ aspiration + enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companytoyota +car_companysubaru +  
                car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury +car_companymitsubishi +
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_19)
sort(vif(model_19),decreasing = T)
#Removing aspiration
model_20 <-lm(formula = price ~  enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companydodge  +car_companyjaguar + car_companymazda + car_companymercury + 
                car_companymitsubishi +  car_companysubaru + car_companytoyota +cylindernumber.four + cylindernumber.six +
                EnginePower, data = cars_training_data)

summary(model_20)
sort(vif(model_20),decreasing = T)
#Removing car_companydodge
model_21 <-lm(formula = price ~  enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companyjaguar + car_companymazda +
                car_companymercury + car_companymitsubishi +  car_companysubaru + car_companytoyota +  
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_21)
sort(vif(model_21),decreasing = T)
#Removing car_companymercury
model_22 <-lm(formula = price ~  enginelocation + car_companyjaguar + car_companymazda + 
                carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+
                car_companymitsubishi +  car_companysubaru + car_companytoyota +  
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_22)
sort(vif(model_22),decreasing = T)
#Removing car_companymazda
model_23 <-lm(formula = price ~  enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companyjaguar +   
                car_companymitsubishi +  car_companysubaru + car_companytoyota +  
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_23)
sort(vif(model_23),decreasing = T)
#Removing car_companytoyota
model_24 <-lm(formula = price ~  enginelocation +carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companyjaguar +   
                car_companymitsubishi +  car_companysubaru +cylindernumber.four + cylindernumber.six + 
                EnginePower, data = cars_training_data)

summary(model_24)
sort(vif(model_24),decreasing = T)
#Removing car_companymitsubishi
model_25 <-lm(formula = price ~  enginelocation + carwidth + enginesize + stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companyjaguar + car_companysubaru +   
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_25)
sort(vif(model_25),decreasing = T)
#Removing enginesize
model_26 <-lm(formula = price ~  enginelocation +carwidth +  stroke + horsepower + drivewheel.rwd + 
                car_companybmw + car_companybuick + cylindernumber.five+car_companyjaguar + car_companysubaru +   
                cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_26)
sort(vif(model_26),decreasing = T)
#Removing drivewheel.rwd 
model_27 <-lm(formula = price ~  enginelocation + carwidth +  stroke + horsepower + car_companybmw + car_companybuick + cylindernumber.five+
                car_companyjaguar + car_companysubaru +cylindernumber.four + cylindernumber.six + 
                EnginePower, data = cars_training_data)

summary(model_27)
sort(vif(model_27),decreasing = T)
#Removing car_companysubaru
model_28 <-lm(formula = price ~  enginelocation + carwidth +  stroke + horsepower + car_companybmw + car_companybuick + cylindernumber.five+
                car_companyjaguar + cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_28)
sort(vif(model_28),decreasing = T)
#Removing stroke
model_29 <-lm(formula = price ~  enginelocation + carwidth + horsepower +car_companybmw + car_companybuick + cylindernumber.five+
                car_companyjaguar + cylindernumber.four + cylindernumber.six + EnginePower, data = cars_training_data)

summary(model_29)
sort(vif(model_29),decreasing = T)

#Engine power
#Due to high correlation between horse power and Engine power and also having high negative coefficient.
#Hence removing Enginer power inorder to make model significant
cor(cars_training_data$EnginePower,cars_training_data$horsepower)  #0.8520372
model_30 <-lm(formula = price ~  enginelocation + carwidth + horsepower +car_companybmw + car_companybuick + 
                cylindernumber.five+car_companyjaguar + cylindernumber.four + cylindernumber.six, data = cars_training_data)

summary(model_30)
sort(vif(model_30),decreasing = T)


#cor(cars_training_data$cylindernumber.five,cars_training_data$cylindernumber.four) #-0.4731602
#cor(cars_training_data$cylindernumber.five,cars_training_data$cylindernumber.six) #-0.09198711


###############################################################################################
#Due to high Correlation and  -ve sign coefficient for Cylinder.four removing Cylinder.four
#Inorder to make model significant

cor(cars_training_data$cylindernumber.four,cars_training_data$cylindernumber.six) #-0.6480336

#Removing cylindernumber.four
model_31 <-lm(formula = price ~  enginelocation + carwidth + horsepower +car_companybmw + car_companybuick + 
                cylindernumber.five+car_companyjaguar +  cylindernumber.six, data = cars_training_data)

summary(model_31)
sort(vif(model_31),decreasing = T)

#Removing cylindernumber.six 
model_32 <-lm(formula = price ~  enginelocation + carwidth + horsepower +car_companybmw + car_companybuick + 
                cylindernumber.five+car_companyjaguar , data = cars_training_data)

summary(model_32)
sort(vif(model_32),decreasing = T)

##Removing cylindernumber.five 
model_33 <-lm(formula = price ~  enginelocation + carwidth + horsepower + car_companybmw + car_companybuick + 
                car_companyjaguar , data = cars_training_data)

summary(model_33)
sort(vif(model_33),decreasing = T)

# Predict the car prices in the testing dataset
Predict_1 <- predict(model_33,cars_test_data[,-1])
cars_test_data$test_price <- Predict_1

# For Accuracy of the predictions, Calculate correlation
r <- cor(cars_test_data$price,cars_test_data$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(cars_test_data$price,cars_test_data$test_price)^2
rsquared    # check R-squared


############################################
#Results
###########################################
#On training dataset Multiple R-squared:  0.9343,	Adjusted R-squared:  0.9314 
#On test data set R-squared value :   0.8721394

######################
#Plot actual vs predicted price
##########################

ggplot(cars_test_data,aes(carwidth, price))+geom_line(aes(colour="blue")) +
geom_line(aes(x=cars_test_data$carwidth,y=cars_test_data$test_price,colour = "red"))

ggplot(cars_test_data,aes(horsepower, price))+geom_line(aes(colour="blue")) +
 geom_line(aes(x=cars_test_data$horsepower,y=cars_test_data$test_price,colour = "red"))

###############################################
# The Conclusion:
###############################################
# I have trained the model and then tested with testing data "cars_test_data".
#Here my linear equation would be of this pattern.Data set can be plotted as per ur wish

#cars_test_data$test_price=(-66304.916)+(-17935.184)*cars_test_data$enginelocation +
#  (1344.490)*cars_test_data$carwidth +(73.049)*cars_test_data$horsepower +
#  (9516.041)*cars_test_data$car_companybmw +(11797.856)*cars_test_data$car_companybuick+
#  (11706.921)*cars_test_data$car_companyjaguar
####
#Hence price factor is higly correlated with carwidth,horsepower,car_companybmw,car_companybuick,car_companyjaguar
#and negatively correlated with enginelocation
