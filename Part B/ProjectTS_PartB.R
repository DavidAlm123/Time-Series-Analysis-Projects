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

data_box <- log(data_ts)
plot.ts(data_box, main="Logged closing price of $VAIGLOA", ylab="Closing Price")
box_diff <- diff(data_box)
plot.ts(box_diff, main="Logged and differenced closing price of $VAIGLOA", ylab="Closing Price")

acf(box_diff, main="ACF of cleaned data", lag.max=100)
pacf(box_diff, main="PACF of cleaned data", lag.max=100)


plot(season(box_diff,d=252), type="h")

arma_two_three= arma(box_diff, 1,0)
M=c("log","diff",1)
forces <- forecast(data_ts,M, arma_two_three,h=252)

resids_AR_ONE=Resid(data_ts, M, arma_two_three)
ar_one_test <- test(resids_AR_ONE)
qqnorm(resids_AR_ONE, main = "Normal Q-Q Plot for AR(1)") 
qqline(resids_AR_ONE, col = "steelblue", lwd = 2)
ggplot(data = data.frame(resids_AR_ONE), aes(x = resids_AR_ONE)) +
   geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "white") +
   geom_density(alpha = .2, fill = "#FF6666") +
   labs(title = "Histogram of Residuals for AR(1)", x = "Residuals", y = "Density")+
  ggeasy::easy_center_title()


plot.ts(data_ts, xlim=c(2000,2030), main="AR(1) forecast", ylab="Closing Price")
lines(ts(forces$pred, start=c(2022,12), frequency=252), col="green")
lines(ts(forces$u, start=c(2022,12), frequency=252), col="red", lty="dashed")
lines(ts(forces$l, start=c(2022,12), frequency=252), col="red", lty="dashed")
legend("topright",
       legend=c("Original data","Forecast", "95% C.I"), col=c("black","green", "red"),
       lty=c("solid","solid","dashed"), text.font=2, cex=0.8)

ma_one= arma(box_diff, 0,1)
M=c("log","diff",1)
forces_ma_one <- forecast(data_ts,M, ma_one,h=252)

resids_MA_ONE <- Resid(data_ts, M, ma_one)
test(resids_MA_ONE)
qqnorm(resids_MA_ONE, main = "Normal Q-Q Plot for MA(1)") 
qqline(resids_MA_ONE, col = "steelblue", lwd = 2)

plot.ts(data_ts, xlim=c(2000,2030), main="MA(1) forecast", ylab="Closing price")
lines(ts(forces_ma_one$pred, start=c(2022,12), frequency=252), col="green")
lines(ts(forces_ma_one$u, start=c(2022,12), frequency=252), col="red", lty="dashed")
lines(ts(forces_ma_one$l, start=c(2022,12), frequency=252), col="red", lty="dashed")
legend("topright",
       legend=c("Original data","Forecast", "95% C.I"), col=c("black","green", "red"),
       lty=c("solid","solid","dashed"), text.font=2, cex=0.8)

arma_one_one=arma(box_diff, 1,1)




arma_four_zero <- arma(box_diff, 4,0)
M=c("log","diff",1)
forces_four_zero <- forecast(data_ts,M, arma_four_zero,h=252)

plot.ts(data_ts, xlim=c(2000,2030), main="AR(4) forecast", ylab="Price")
lines(ts(forces_four_zero$pred, start=c(2022,12), frequency=252), col="green")
lines(ts(forces_four_zero$u, start=c(2022,12), frequency=252), col="red", lty="dashed")
lines(ts(forces_four_zero$l, start=c(2022,12), frequency=252), col="red", lty="dashed")


#boxplot(box_diff)
#plot.ts(diff(box_diff,lag=365))

#ats <- auto.arima(box_diff)


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

plot.ts(cleaned, main="Closing price of $VAIGLOA after taking logarithming, differencing and removing outliers", ylab="Value",, cex.main=0.6)
pacf(cleaned, lag.max=10) #2
acf(cleaned, lag.max=10) #1
plot.ts(cleaned)
library(tseries)
adf.test(cleaned)

ar_four_cleaned= arma(cleaned, 4,0)
M=c("log","diff",1)
forc_clean <- forecast(data_ts,M, ar_four_cleaned, h=100)

plot.ts(data_ts, xlim=c(2000,2030), main="AR(4) forecast cleaned", ylab="Price")
lines(ts(forc_clean$pred, start=c(2022,12), frequency=252), col="green")
lines(ts(forc_clean$u, start=c(2022,12), frequency=252), col="red", lty="dashed")
lines(ts(forc_clean$l, start=c(2022,12), frequency=252), col="red", lty="dashed")




auto <- auto.arima(cleaned, stationary=TRUE, seasonal=FALSE)
auto_forc <- forecast::forecast(auto, h=500)




preds <-auto_forc$mean
preds_lower <- as.numeric(auto_forc$lower[,2])
preds_lower[1] <- preds_lower[1]+data_box[5567]
preds[1] <-auto_forc$mean[1]+data_box[5567]


for (i in 2:length(preds)){
  preds[i] <- preds[i]+preds[i-1]
}

for (i in 2:(length(auto_forc$lower)/2)){
  preds_lower[i] <- preds_lower[i]+preds_lower[i-1]
}


preds=exp(preds)
preds_lower=exp(preds_lower)

preds_ts=ts(preds,start=c(2022,04), frequency=252)
lower_ts=ts(preds_lower,start=c(2022,04), frequency=252)
plot(data_ts, xlim=c(2000,2025))
lines(preds_ts, col="red")
lines(lower_ts, col="green")

#########




#New model
plot.ts(data_ts)
decomp <- decompose(data_ts)
autoplot(decomp)
trn <- trend(data_ts, p=11)
plot(trn)
szn <- season(data_ts, 252)
plot(szn, type="h")
no_seas_trend <- data_ts - szn -trn
plot.ts(no_seas_trend, ylab="Closing price", main="Closing price of $VAIGLOA after trend and season removal", cex.main=0.8)
acf(no_seas_trend, lag.max=5000, main="ACF of deseasoned, detrended closing price")
pacf(no_seas_trend, lag.max=10,main="PACF of deseasoned, detrended closing price") #AR(2)
my_test_adf <- adf.test(no_seas_trend)
new_model_auto <- auto.arima(no_seas_trend, seasonal=FALSE, stationary=TRUE, allowmean = FALSE)

x <- arima.sim(model = list(order = c(3, 0, 3), ar = c(0.32981368, 0.88030628,-0.25164379
                                                       ), ma=c(0.51527396, -0.32332348, 0.05941454
                                                          )), n = 5000)
acf(x, lag.max=5000)
pacf(x, lag.max=10)

new_model_forecast <- forecast::forecast(new_model_auto, h=500)
forcasted_values <- new_model_forecast$mean + decomp$seasonal[(5568-500):(5567)] + trend(data_ts, p=7)[(5567-500+1):5567]
forcasted_values_up <- new_model_forecast$upper[,2] +  decomp$seasonal[(5568-500):(5567)] + trend(data_ts, p=7)[(5567-500+1):5567]
forcasted_values_down <- new_model_forecast$lower[,2] +  decomp$seasonal[(5568-500):(5567)] + trend(data_ts, p=7)[(5567-500+1):5567]

plot.ts(data_ts, xlim=c(2000,2025), main="Forecasted values from ARMA(3,3)", ylab="Closing price")
lines(ts(forcasted_values, start=c(2022), frequency = 252), col="green")
lines(ts(forcasted_values_up, start=c(2022), frequency = 252), col="red", lty="dashed")
lines(ts(forcasted_values_down, start=c(2022), frequency = 252), col="red", lty="dashed")

