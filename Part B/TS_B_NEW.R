library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(pracma)
library(forecast)
ValueInvest_Global_A <- read.csv("/afs/kth.se/home/m/i/miramadi/Download/ValueInvest_Global_A.csv")
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

cleaned <- box_diff

diffed_data <- diff(data_ts)
plot.ts(diffed_data)
fit_mod <- auto.arima(diffed_data)

# quantlies <- quantile(box_diff,p = c(0.25, 0.75))
# i_q_r= IQR(box_diff)
# 
# fctr=5.5
# 
# upper_bound <- quantlies[2]+fctr*i_q_r
# lower_bound <- quantlies[1]-fctr*i_q_r
# 
# cleaned <- subset(box_diff, box_diff > (lower_bound) & box_diff < upper_bound)
# length(box_diff[box_diff > (lower_bound) & box_diff < upper_bound])
# removed_up <- which(box_diff > upper_bound)
# removed_down <- which(box_diff < lower_bound)
# data$Date[removed_down]
# data$Date[removed_up]
# 
pacf(cleaned) #2
acf(cleaned) #1
plot.ts(cleaned)
adf.test(cleaned)

n <- length(cleaned)
train <- cleaned[1:(n-20-1)]
test <- data_ts[(n-20):n]

arma_two_three= arma(train, 1,0)
M=c("log","diff",1)
forc <- forecast(data_ts[1:(n-20-1)],M, arma_two_three)

plot(forc$pred, type = "l", ylim=c(160,190))
lines(test[1:10], type = "l", col="red")
lines(forc$l, type="l", col="green")
lines(forc$u, type="l", col="blue")

auto <- auto.arima(data_box, D=1)
auto_forc <- forecast::forecast(auto, h=200)
plot(auto_forc)
