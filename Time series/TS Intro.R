############# Generate Non-stationary data Process

# Random Walk
set.seed(98234)                                    # Creating example series
n   <- 150
eps <- rnorm(n)
x0  <- rep(0, n)
for(i in seq.int(2, n)){
  x0[i] <- x0[i-1] + eps[i]
}
plot(ts(x0))

# Random Walk with Drift
drift <- 2
x1    <- rep(0, n)
for(i in seq.int(2, n)){
  x1[i] <- drift + x1[i-1] + eps[i]
}
plot(ts(x1))

# Deterministic Trend
trend <- seq_len(n)
x2    <- rep(0, n)
for(i in seq.int(2, n)){
  x2[i] <- trend[i] + x2[i-1] + eps[i]
}
plot(ts(x2))
start_date <- as.Date("2000/01/01")
t <- seq(start_date, by = "day", length.out = 150)

# Moving Average 
#install.packages("zoo")                            # Install zoo package
library("zoo")


my_series <- 1:100 + rnorm(100, 0, 10)
plot(my_series, type = "l")                                          # Printing series

my_moving_average_2 <- rollmean(my_series, k = 20)  # Apply rollmean function
plot(my_moving_average_2, type = "l")                                # Printing moving average
lines(my_series, col="red",lty=2)

# Exponential Smoothing
##SES
#install.packages("fpp2")
library(fpp2)
ses.ts <- ses(my_series, h = 100)
autoplot(ses.ts)


##Holt's Linear
library(forecast)
data("AirPassengers")
ts_data <- ts(AirPassengers, frequency = 12)

# install.packages("aTSA")
library(aTSA)
dataHolt <- Holt(ts_data)
plot(predict(dataHolt$estimate))


##HoltWinters
library(forecast)
fit <- HoltWinters(ts_data)
plot(ts_data)
plot(fit$fitted[,1])


# ARMA estimation & forecast
library(readxl)
data <- read_xlsx("Money Supply M1 USD mn Monthly Indonesia.xlsx", sheet = "data")

# Define as time-series variable
data <- ts(data[,2],start = c(1968,2),frequency = 12)

# plot data
plot(data)

# install.packages("tseries")
library(tseries)
library(lmtest)


# ARMA estimation
acf(diff(ts_data))
pacf(diff(ts_data))

armamod <- arima(ts_data, order = c(2,0,1))
# ARMA forecast
pred <- predict(armamod, n.ahead = 5)
pred$pred[1]
pred$se[1]

armamod
# Unit Root Tests
library(tseries)
adf <- adf.test(ts_data)
pp <- pp.test(ts_data)
kpss <- kpss.test(ts_data)


# Treating Non-stationary Processes
##Random Walk
plot(ts(x0))
x0_1 <- c()
for(i in 2:length(x0)){
  x0_1[i] <- x0[i] - x0[i-1]
}
plot(ts(x0_1))

##Random Walk with Drift
diff(x1)
ts.plot(x1)
plot(ts(diff(x1)))

##Deterministic Trend
set.seed(1312)
toy_data <- arima.sim(n = 100, model = list(order = c(0,0,0)))

# add a deterministic trend to the series
toy_data_trend <- toy_data + 0.2*1:length(toy_data)

par(mfrow=c(1,3))
plot.ts(toy_data, main = "Original series")
plot.ts(toy_data_trend, main = "Series with Trend")

dummy_trend <- 1:length(toy_data_trend)
lm_toydata <- lm(toy_data_trend ~ dummy_trend)
plot.ts(lm_toydata$residuals, main = "Residuals (detrended)")

