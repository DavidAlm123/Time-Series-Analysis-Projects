#test
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(pracma)
library(forecast)
library(tseries)

url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_daily_mlo.txt"
data <- read.delim(url,
                   comment.char = "#",
                   header = FALSE,
                   sep = "",
                   col.names = c("YY", "MM", "DD", "Decimal", "CO2_PPM")
)
str(data)
data$Date <- as.Date(with(data, paste(YY, MM, DD, sep="-")), format="%Y-%m-%d")

ggplot(data, aes(x = Date, y = CO2_PPM))+geom_line(color="green")+ggtitle("CO2 in molar proportion")+
  theme(plot.title = element_text(hjust = 0.5))

co2=ts(data=data$CO2_PPM, frequency = 365, start=c(data$YY[1], data$MM[1]))

co2 <- co2[1:4000]
co2 %>% ggtsdisplay() #MA(3)


co2 <- diff(co2, lag=1)
plot.ts(co2)
co2 %>% ggtsdisplay() #AR(5), MA(2), differencing once (4000)


plot.ts(co2)
acf(as.numeric(co2), lag.max=10) #4 =/ 0
pacf(as.numeric(co2), lag.max=10) #->0

fit<- Arima(co2,order=c(3,0,5))
fit_auto <- auto.arima(co2)
checkresiduals(fit)

plot(forecast(fit,h=200))


library(itsmr)
plotc(co2, fit$fitted)
