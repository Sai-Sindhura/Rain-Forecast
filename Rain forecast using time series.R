
library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(forecast)

setwd("~/UWTacoma/TBANLT540/TimeSeriesRegression")
RainSeattleAll <- read.csv("1947028.csv")

#################### data quality check #############
b=RainSeattleAll$DATE
c = substring(b,1,4)
table(c)

################### data cleaning step ##############
RainSeattleAll$DATE<-as.POSIXct(RainSeattleAll$DATE, format="%Y-%m-%d")
RainSeattleAll <- RainSeattleAll[c(RainSeattleAll$DATE >= "1999-01-01"),]
view(RainSeattleAll)


dim(RainSeattleAll)    #returns the dimensions of an object
str(RainSeattleAll)    #returns the structure of an object


sum(is.na(RainSeattleAll)) #returns how many observations have "na"
RainSeattleAll[is.na(RainSeattleAll)] <- '0' #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it
sum(is.na(RainSeattleAll))
View(RainSeattleAll)

RainSeattleAll[is.na(RainSeattleAll)] <- 0
sum(is.na(RainSeattleAll))

#creating a time series dataset for decomposition#

View(RainSeattleAll)

#Create date variables, collapse to monthly averages, compare plots
RainSeattleAll$DATE<-as.POSIXct(RainSeattleAll$DATE, format="%Y-%m-%d")
RainSeattleAll$PRCP<-as.numeric(RainSeattleAll$PRCP)

MonthlyRain<-aggregate(list(rain = RainSeattleAll$PRCP), 
          list(month = cut(RainSeattleAll$DATE, "month")), 
          mean)
MonthlyRain

MonthlyRain2<-ts(MonthlyRain$rain, frequency = 12, start = c(1999,1))
MonthlyRain2

Rain<-ts(RainSeattleAll$PRCP, frequency = 365, start = c(1999,1))

#create a plot of the time series#
plot.ts(Rain)
plot.ts(MonthlyRain2)

#identify the trend/season/random components
RainParts<-decompose(Rain)
RainMonthParts<-decompose(MonthlyRain2)
plot(RainParts)
plot(RainMonthParts)


#Modeling using exponential smoothing - Full data

RainModel1<-HoltWinters(Rain)
RainModel1
RainModel1$SSE

plot(RainModel1, col=3, col.predicted=2)
residualsHolt1<-residuals(RainModel1)

plot(residualsHolt1, col=2)
acf(residualsHolt1)# 4 lags above the line
pacf(residualsHolt1)

#Modeling using exponential smoothing - Monthly data

RainModel2<-HoltWinters(MonthlyRain2)
RainModel2
RainModel2$SSE


plot(RainModel2, col=3, col.predicted=9)
residualsHolt2<-residuals(RainModel2)
plot(residualsHolt2)# better more evenly spaced
acf(residualsHolt2)# only one above line
pacf(residualsHolt2)# only one line

#Forecasting using exponential smooting - Full Data
RainForecast<-forecast(Rain, h=400)# 400 days in future,grey part is standard error, blue s forecast
plot(RainForecast)

#Forecasting using exponential smooting - Monthly Data

RainForecast2<-forecast(MonthlyRain2, h=13)# 13 months in future
plot(RainForecast2)

#modeling using an auto.arima model - Full Data 
#plot the acf and pacf
par(mfrow=c(1,2))
acf(Rain)
pacf(Rain)

RainArima<-auto.arima(Rain, seasonal=TRUE)
RainArima<-auto.arima(Rain, trace=TRUE)
RainArima
acf(ts(RainArima$residuals), main='ACF Residual - Full')
pacf(ts(RainArima$residuals), main='PACF Residual - Full')



#modeling using an auto.arima model - Monthly Data 
#plot the acf and pacf
acf(MonthlyRain2)
pacf(MonthlyRain2)

RainArima2<-auto.arima(MonthlyRain2)
RainArima2

acf(ts(RainArima2$residuals), main='ACF Residual - Monthly')
pacf(ts(RainArima2$residuals), main='PACF Residual- Monthly')

prediction=predict(RainArima,n.ahead=15)
prediction
plot(forecast(RainArima,h=100))

prediction=predict(RainArima2,n.ahead=6)
prediction
