library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(pracma)
library(forecast)
ValueInvest_Global_A<-read.csv("~/Desktop/KTH/SF2943/ProjektTS/ValueInvest_Global_A.csv")
data<-ValueInvest_Global_A
str(data)
data$Date<-as.Date(data$Date)
str(data)
colnames(data)<-c("Date","High.price","Low.price","Closing.price")
which(is.na(data))
data_ts=ts(data = data$Closing.price, frequency=252, start=c(2000,10))

plot.ts(diff(data_ts))

data_box <- log(data_ts)
box_diff <- diff(data_box)
boxplot(box_diff)
plot.ts(diff(box_diff,lag=365))

quantlies <- quantile(box_diff,p = c(0.25, 0.75))
i_q_r= IQR(box_diff)

fctr=5.5

upper_bound <- quantlies[2]+fctr*i_q_r
lower_bound <- quantlies[1]-fctr*i_q_r

cleaned <- subset(box_diff, box_diff > (lower_bound) & box_diff < upper_bound)
length(box_diff[box_diff > (lower_bound) & box_diff < upper_bound])
removed_up <- which(box_diff > upper_bound)
removed_down <- which(box_diff < lower_bound)
data$Date[removed_down]
data$Date[removed_up]

pacf(cleaned, lag.max=10) #2
acf(cleaned, lag.max=10) #1
plot.ts(cleaned)
adf.test(cleaned)

arma_two_three= arma(cleaned, 2,3)
M=c("log","diff",1)
forecast(data_ts,M, arma_two_three)



auto <- auto.arima(data_box, D=1)
auto_forc <- forecast::forecast(auto, h=200)
plot(auto_forc)
