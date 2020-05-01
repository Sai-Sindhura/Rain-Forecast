library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(car)
library(zoo)


RainSeattle2018 <- read_excel("1929697.xlsx")


dim(RainSeattle2018)    #returns the dimensions of an object
str(RainSeattle2018)    #returns the structure of an object
sum(is.na(RainSeattle2018)) #returns how many observations have "na"
RainSeattle2018[is.na(RainSeattle2018)] <- '0' #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it



sum(is.na(RainSeattle2018))
View(RainSeattle2018)

select (RainSeattle2018,-c(PGTM,SNOW,SNWD,WDF2))

View(RainSeattle2018)



#add a Season Variable#


yq <- as.yearqtr(as.yearmon(RainSeattle2018$DATE, "%Y/%m/%d") + 1/12)
RainSeattle2018$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))
table(RainSeattle2018$Season)

#create a Wind Direction factor variable#

RainSeattle2018$NewWindDir<-RainSeattle2018$WDF5-11.25
RainSeattle2018$WindCat<-cut(RainSeattle2018$NewWindDir, seq(-11.25, 348.75, 22.5), labels = FALSE)
labels = setNames(c("N", "NNE", "NE", "ENE", 
                   "E", "ESE", "SE", "SSE",
                   "S", "SSW", "SW", "WSW",
                   "W", "WNW", "NW", "NNW"), c(seq(1,16,1)))
RainSeattle2018$WindCat <- factor(RainSeattle2018$WindCat)
RainSeattle2018$NewWindCat<-revalue(RainSeattle2018$WindCat, labels) #c("1"="N","2"="NNE","3"="NE","4"="ENE", "5"="E","6"="ESE","7"="SE","8"="SSE", "9"="S","10"="SSW","11"="SW","12"="WSW", "13"="W","14"="WNW","15"="NW","16"="NNW"))


RainSeattle2018$NewWindDir8<-RainSeattle2018$WDF5-22.5
RainSeattle2018$WindCat8<-factor(cut(RainSeattle2018$NewWindDir8, seq(-22.5, 337.5, 45), labels = FALSE))
labels8 = setNames(c("N", "NE", "E", "SE", 
                    "S", "SW", "W", "NW"), c(seq(1,8,1)))
RainSeattle2018$NewWindCat8<-revalue(RainSeattle2018$WindCat8, labels8)

# RainSeattle2018$WindCat<-cut(RainSeattle2018$NewWindDir, c(-22,22,67,112,157,202,247,292,337))
# RainSeattle2018$NewWindCat<-revalue(RainSeattle2018$WindCat, c("(-22,22]"="N","(22,67]"="NE","(67,112]"="E","(112,157]"="SE","(157,202]"="S","(202,247]"="SW","(247,292]"="W","(292,337]"="NW" ))
list(RainSeattle2018$NewWindCat)
table(RainSeattle2018$NewWindCat8)

#Create Factor variable indicating presence of Rain
RainSeattle2018$RainFac <- ifelse(RainSeattle2018$PRCP > 0, 1, 0)
rain<-factor(RainSeattle2018$RainFac)
table(RainSeattle2018$RainFac)

#create sequential logit models

rainpredict<-glm(rain~RainSeattle2018$AWND, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG + RainSeattle2018$TMAX, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG + RainSeattle2018$TMAX + RainSeattle2018$TMIN, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG + RainSeattle2018$TMAX + RainSeattle2018$TMIN + RainSeattle2018$NewWindCat8, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG + RainSeattle2018$TMAX + RainSeattle2018$TMIN + RainSeattle2018$NewWindCat8 + RainSeattle2018$WSF5, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))
predictionlog=predict(rainpredict,n.ahead=15)
predictionlog
predictionlog1=predict(rainpredict,type="response")
predictionlog1
rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG + RainSeattle2018$TMAX + RainSeattle2018$TMIN + RainSeattle2018$NewWindCat8 + RainSeattle2018$WSF5 + RainSeattle2018$Season, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

##custom
rainpredict<-glm(rain~RainSeattle2018$AWND + RainSeattle2018$TAVG + RainSeattle2018$TMAX + RainSeattle2018$TMIN + RainSeattle2018$NewWindCat8 + RainSeattle2018$WSF5 + RainSeattle2018$Season, data=RainSeattle2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$NewWindCat8, y=rain.res))+geom_point()


rain.res<-residuals.glm(rainpredict)

ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$AWND, y=rain.res))+geom_point()
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$TAVG, y=rain.res))+geom_point()
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$TMAX, y=rain.res))+geom_point()
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$TMIN, y=rain.res))+geom_point()
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$WSF5, y=rain.res))+geom_point()
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$NewWindCat8, y=rain.res))+geom_point()
ggplot(data=RainSeattle2018, aes(x=RainSeattle2018$Season, y=rain.res))+geom_point()
