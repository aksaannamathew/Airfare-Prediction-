library(readr)
library(DataExplorer)
library(imputeTS)
library(psych)
library(tidyverse)
library(caret)
library(forecast)
library(fpp)
library(tseries)
library(MLmetrics)
library(TSstudio)

##importing Domestic data 

domestic<-read.csv("C:\\Users\\God\\Desktop\\domesticfare.csv")
head(domestic)
str(domestic)
attach(domestic)
domestic$Date<-as.Date(domestic$Date,format = "%d-%m-%Y")

##replacing missing value with exponential moving average 
sapply(domestic, function(x) sum(is.na(x)))

domestic$Air<-na_ma(domestic$Air,k=3,weighting = "exponential")
domestic$Hotel=na_ma(domestic$Hotel,k=3,weighting = "simple") ## Fixed

##converting into time series data
str(dom.air)
class(dom.air)
dom.air<-ts(domestic$Air,start=c(2018,4,1),frequency=365)
dom.hotel<-ts(domestic$Hotel,start=c(2018,4,1),frequency=365)

## visualtion plotting 

ggplot(domestic, aes(x = Date, y = Air)) +        ##Domestic Air visualisation
  ggtitle("Domestic Air Fare")+
  geom_line(col="blue") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month") +
  theme_classic()+
  theme_bw()

ggplot(domestic, aes(x = Date, y = Hotel)) +      ## domestic hotel visualisation
  ggtitle("Domestic hotel Fare")+
  geom_line(col="blue") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month") +
  theme_classic()+
  theme_bw()

##final model building using neural network for Air and hotel
##Domestic air fare  prediction
set.seed(123)
fit.air<-nnetar(dom.air)
pred.air<-forecast(fit.air,h=30)
accuracy(fit.air)           
plot(pred.air)

##domestic hotel prediction
set.seed(123)
fit.hotel<-nnetar(dom.hotel)
pred.hotel<-forecast(fit.hotel,h=30)
accuracy(fit.hotel)           
plot(pred.hotel)


##---------------------------------INTERNATIONAL AIR & HOTEL PREDICTION---------------------##

inter<-read.csv("C:\\Users\\God\\Desktop\\Excelr Project\\INT_AIR_HOTLE.csv")
head(inter)
str(inter)
attach(inter)
inter$Date<-as.Date(inter$Date,format = "%d-%m-%Y")

##replacing missing value with exponential moving average 
sapply(inter, function(x) sum(is.na(x)))

inter$Air<-na_ma(inter$Air,k=2,weighting = "exponential")
inter$Hotel<-na_interpolation(inter$Hotel,option = "linear") ## Fixed
ts.plot(inter$Air)
##converting into time series data
str(dom.air)
class(dom.air)
inter.air<-ts(inter$Air,start=c(2018,4,1),frequency=365)
inter.hotel<-ts(inter$Hotel,start=c(2018,4,1),frequency=365)

## visualtion plotting 

ggplot(inter, aes(x = Date, y = Air)) +        ##International Air visualisation
  ggtitle("International Air Fare")+
  geom_line(col="blue") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month") +
  theme_classic()+
  theme_bw()

ggplot(inter, aes(x = Date, y = Hotel)) +      ## International hotel visualisation
  ggtitle("International Hotel Fare")+
  geom_line(col="blue") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month") +
  theme_classic()+
  theme_bw()


## final model building using neural network for INTERNATIONAL Air and Hotel
##Air 
set.seed(123)
fit.int.air<-nnetar(inter.air)
pred.int.air<-forecast(fit.int.air,h=30)
accuracy(fit.int.air)           
plot(pred.int.air)

##international hotel prediction
set.seed(123)
fit.int.hotel<-nnetar(inter.hotel)
pred.int.hotel<-forecast(fit.int.hotel,h=30)
accuracy(fit.int.hotel)           
plot(pred.int.hotel)
