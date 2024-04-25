library(forecast)
library(tseries)
library(ggplot2)
library(itsmr)

GlobalLandTemperaturesByCountry <- read.csv("~/Desktop/KTH/SF2943/ProjektTS/archive-2/GlobalLandTemperaturesByCountry.csv")
data <- GlobalLandTemperaturesByCountry
indix <- which(GlobalLandTemperaturesByCountry[,"Country" ]=="Sweden")
swe <- data$AverageTemperature[min(indix):max(indix)]
swe <- swe[108:3238]

swe_ts <- ts(swe, frequency=12, start=c(1752,10))
plot.ts(swe_ts, main="Average monthly temperature in Sweden", ylab="Average temperature (°C)", xlab="Time")

plot.ts(ts(swe_ts[1:(12*10)], frequency = 12, start=c(1752, 10)),main="Average monthly temperature in Sweden", ylab="Average temperature (°C)",
        xlab="Time", lwd=2)

acf(swe_ts, lag.max=36, main="ACF of avg. temperature in Sweden")
pacf(swe_ts, lag.max=36, main="PACF of avg. temperature in Sweden")


#decomp <- decompose(swe_ts)
#autoplot(decomp)
seas=season(swe_ts,d=12)
plot.ts(seas[1:36], main="Seasonal component of the time series", xlab="Months from 1752-10-01",
        ylab="Average temperature (°C)")
swe_noseas <- swe_ts-seas

#swe_noseas <- swe_ts - decomp$seasonal
plot.ts(swe_noseas, main="Deseasonalized Average temperature in Sweden",
        ylab="Average temperature (°C)")
#swe_noseas <- detrend(as.numeric(swe_noseas), tt="linear")
trnd <- trend(swe_noseas,p=1)
plot.ts(trnd)
swe_noseas <-swe_noseas-trnd
plot.ts(swe_noseas, main="Cleaned data",ylab="Average temperature (°C)")
acf(swe_noseas, lag.max=50, main="ACF plot of cleaned data") #MA(3)
pacf(swe_noseas, lag.max=500, main="PACF plot of cleaned data")

adf <- adf.test(swe_noseas)
#####
library(itsmr)
fit_ma <- arma(swe_noseas, p=0, q=3)
fit_ma2 <- arma(swe_noseas, p=0, q=2)


M=c("season",12, "trend", 1)
forc <- forecast(swe_ts, M, a=fit_ma)
forc_ts=ts(forc$pred, frequency = 12,start=c(2013,09))

plot(forc_ts, lty="dashed", ylim=c(-20,20),xlim=c(1980,2015), col="orange", lwd=1, main="MA(3) forecast",ylab="Average temperature (°C)")
short_ts <- ts(swe_ts[2771:3131], frequency = 12, end=c(2013,08))
lines(short_ts, col="blue",type='l')
upper_bound_ts <-ts(forc$u, frequency = 12,start=c(2013,09))
lines(upper_bound_ts, col="red", lwd=0.5)
lower_bound_ts <-ts(forc$l, frequency = 12,start=c(2013,09))
lines(lower_bound_ts, col="red", lwd=0.5)
legend("topleft", legend=c("Historic data", "Forecast"), col=c("blue","orange"), lty=1:2)


auto <- auto.arima(swe_noseas)
auto_forc <- forecast::forecast(auto, h=36)
auto_forc_ts_upper=auto_forc$upper[,2]+trnd[1:36]+seas[1:36]
plot(auto_forc_ts_upper, lty="solid", ylim=c(-20,20),xlim=c(1983,2015), col="red", lwd=1, main="ARMA(2,1) forecast", ylab="Average temperature (°C)")
lines(auto_forc$lower[,2]+trnd[1:36]+seas[1:36],ylim=c(-20,20),xlim=c(1980,2015),col="red", lty="solid")
lines(short_ts, col="blue",type='l')
legend("bottomleft", legend=c("Historic data", "95% C.I forecast"), col=c("blue","red"), lty="solid")


