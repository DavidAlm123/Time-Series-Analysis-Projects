GlobalLandTemperaturesByCountry <- read.csv("~/Desktop/KTH/SF2943/ProjektTS/archive-2/GlobalLandTemperaturesByCountry.csv")
data <- GlobalLandTemperaturesByCountry
indix <- which(GlobalLandTemperaturesByCountry[,"Country" ]=="Sweden")
swe <- data$AverageTemperature[min(indix):max(indix)]
swe <- swe[108:3238]

swe_ts <- ts(swe, frequency=12, start=c(1752,10))
#swe_ts <- diff(swe_ts, lag=1)

plot.ts(swe_ts)
decomp <- decompose(swe_ts)
autoplot(decomp)

swe_noseas <- swe_ts - decomp$seasonal
plot.ts(swe_noseas)
swe_noseas <- detrend(as.numeric(swe_noseas), tt="linear")
plot.ts(swe_noseas)
acf(swe_noseas, lag.max=50) #MA(3)
pacf(swe_noseas, lag.max=200)

adf <- adf.test(swe_noseas)
#####
fit_ma <- Arima(swe_noseas, order=c(0,0,3), include.mean = FALSE, method="ML")

auto <- auto.arima(swe_noseas)
