library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(pracma)
library(forecast)
ValueInvest_Global_A<-read_csv("ValueInvest_Global_A.csv")
data<-ValueInvest_Global_A
str(data)
data$Date<-as.Date(data$Date)
str(data)
colnames(data)<-c("Date","High.price","Low.price","Closing.price")
data


p<-ggplot(data,aes(x=Date,y=Closing.price)) + geom_line(color="#69b3a2") 
p
acf(data$Closing.price,lag.max=length(data$Closing.price),main="Plot of ACF against lag for closing price")

data$Closing.price <- detrend(data$Closing.price,tt="linear")

acf(data$Closing.price,lag.max=length(data$Closing.price),main="ACF against lag, after removing trends")

p<-ggplot(data,aes(x=Date,y=Closing.price)) + geom_line(color="blue") + ggtitle("Time series after removing trend")+
theme(plot.title = element_text(hjust = 0.5))
p

#Almost identically zero. Fit AR model
pacf(data$Closing.price,lag.max=length(data$Closing.price),main="Plot of PACF against lag for closing price")

data$Closing.price <-data$Closing.price - mean(data$Closing.price)

fit <- auto.arima(y=data$Closing.price, stationary=TRUE, seasonal=FALSE)

plot(forecast(fit,h=100))
plot(fit$residuals)




ma5pacf=ARMAacf(ma=fit$coef,lag.max=length(data$Closing.price),pacf=TRUE)
plot(ma5pacf,type="h",main="Theoretical PACF for the model",xlab="Lag",ylab="PACF")
ma5pac=ARMAacf(ma=fit$coef,lag.max=10)
plot(ma5pac,type="h",main="Theoretical ACF for the model",xlab="Lag",ylab="PACF")





  