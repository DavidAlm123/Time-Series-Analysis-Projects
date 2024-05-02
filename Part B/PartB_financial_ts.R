library(forecast)
library(tseries)
library(ggplot2)
library(itsmr)
library(parallel)  # For parallel computing
cl <- makeCluster(detectCores() - 1)  # Leave one core free

setwd("/Users/micheledalto/Desktop/R/Time Series")

valueinvest <- read.csv("ValueInvest_Global_A.csv")
valueinvest$Date <- as.Date(valueinvest$Date, format = "%Y-%m-%d")  # Adjust the format as per your data

date <- valueinvest$Date
start_date <- min(date)
end_date <- max(date)

hp <- valueinvest$High.price
lp <- valueinvest$Low.price
cp <- valueinvest$Closing.price

cp_ts <- ts(cp, start=c(start_date), frequency=252)
plot.ts(cp_ts, main="Close Price", ylab="Price", xlab="Day")

cp_ts <- log(cp_ts)- mean(log(cp_ts))
plot.ts(cp_ts, main="Close log Price", ylab=" log Price", xlab="Day")

cp_ts <- diff(cp_ts, differences = 1)
plot.ts(cp_ts, main="Close Price", ylab="Price", xlab="Day")

# Plot the Autocorrelation Function (ACF) of the time series
acf(cp_ts, lag.max=365, main="ACF of closing price")

# Plot the Partial Autocorrelation Function (PACF) of the time series
pacf(cp_ts, lag.max=365, main="PACF of closing price")

# Decomposition of the time series
m <- decompose(cp_ts)

# Seasonal component
plot.ts(m$seasonal, main="Seasonal component of the time series")

# Remove seasonal from the deseasonalized data
cp_noseas <- cp_ts - m$seasonal
plot.ts(cp_noseas, main="Deseasonalized closing price", ylab="Closing price")

# Trend component
plot.ts(m$trend, main="Trend component of the time series")

# Remove trend from the deseasonalized data
cp_noseas <- cp_noseas - m$trend
cp_noseas <- na.omit(cp_noseas)

# Plot the cleaned data (after removing seasonality and trend)
plot.ts(cp_noseas, main="Cleaned data", ylab="Closing price")

# Plot the ACF of the cleaned data
acf(cp_noseas, lag.max=365, main="ACF plot of cleaned data")

# Plot the PACF of the cleaned data
pacf(cp_noseas, lag.max=365, main="PACF plot of cleaned data")

# Perform Augmented Dickey-Fuller test to check for stationarity
adf <- adf.test(cp_noseas)


###

trnd <- na.omit(m$trend)
seas <- na.omit(m$seasonal)


n = length(cp_noseas)
hh = 30 #test days

train <- cp_noseas[1:(n-hh)]
test <- cp_ts[(n-hh+1):n]
clusterExport(cl, list("train"))

auto <-auto.arima(train, max.order = 20,
                  max.p = 5,
                  max.q = 5,
                  max.P = 5,
                  max.Q = 5,
                  ic = c("bic"),
                  stationary = TRUE, seasonal = FALSE, approximation = FALSE, trace = TRUE, 
                  stepwise = FALSE,  num.cores = NULL)

summary(auto)
forecast::checkresiduals(auto)

auto_forc <- forecast::forecast(auto, h=hh)
auto_forc_ts_upper = as.vector(auto_forc$upper[,2]+trnd[(n-hh+1):n]+seas[(n-hh+1):n])
auto_forc_ts_lower = as.vector(auto_forc$lower[,2]+trnd[(n-hh+1):n]+seas[(n-hh+1):n])
dates <- date[(n-hh+1):n]

# Calculate the overall limits for the y-axis to include all data
all_values <- c(auto_forc_ts_upper,
                auto_forc_ts_lower,
                test)

y_lim <- range(all_values)

plot(dates, auto_forc_ts_upper, type = "l", lty = "solid", col = "red", lwd = 1,
     xlab = "Time", ylab = "Values", main = "Forecast with Confidence Intervals", ylim = y_lim)
lines(dates, auto_forc_ts_lower, col = "red", lty = "solid")
lines(dates, test, col = "blue", lty = "solid")

# Add a legend to the plot
legend("bottomleft", legend = c("95% C.I. Forecast", "Historic Data"), col = c("red", "blue"), lty = c("solid", "solid"), cex = 0.8)

