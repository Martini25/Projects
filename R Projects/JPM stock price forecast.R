# Load data
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

# Pull Data from yfinance

getSymbols('JPM', from = '2020-01-01', to = '2024-10-18')

# want the days close price for each trading day (4th column)
JPM_close_prices <- JPM[,4]
# plot the data
par(mfrow=c(1,1))
plot(JPM_close_prices)

# plot and get the initial, auto arima pdq values
par(mfrow=c(1,2))
Acf(JPM_close_prices, main = 'ACF for Differenced Series')
Pacf(JPM_close_prices, main = 'PACF for Differenced Series')
auto.arima(JPM_close_prices, seasonal = FALSE)

# Log Residuals to remove non-stationary properties:
# Compute the log returns for the stock - makes it more stable
logs = diff(log(JPM_close_prices), lag = 1)
logs = logs[!is.na(logs)]

# plot log returns for more accurate forecasting - eliminates
par(mfrow=c(1,1))
plot(logs, type='l', main='log returns plot')
print(adf.test(logs)) # p-value < 0.01

auto.arima(logs, seasonal = FALSE) 
str(logs)

# Data partition: 80/20 - train and valid
sample_size = floor(0.80*nrow(logs))
set.seed(2)
train_index <- sample(seq_len(nrow(logs)), size = sample_size)

train <- logs[train_index, ]
valid <- logs[-train_index, ]

par(mfrow=c(1,2))
Acf(train, main='ACF for Differenced Series')
Pacf(train, main='PACF for Differenced Series')
auto.arima(train, seasonal = FALSE)



# plot models, get accuracy and draw conclusions:
# Look at residuals for the autoarima and custom arima models based on the aboved determined pdq values
fit1 <- auto.arima(train, seasonal = FALSE)
tsdisplay(residuals(fit1), lag.max = 40, main = '(0,0,1) Model Residuals')

fit2 = arima(train, order = c(0,1,2)) # custom
tsdisplay(residuals(fit2), lag.max = 40, main = '(0,1,2) Model Residuals')

# original data without log of returns
fit3 = auto.arima(JPM_close_prices, seasonal = FALSE)
tsdisplay(residuals(fit3), lag.max=40,main = 'original, non-log returns model residuals')

# custom arima from fit 2 above applied to original dataset
fit4 = arima(JPM_close_prices, order = c(0,1,2))
tsdisplay(residuals(fit4), lag.max = 40, main = '(0,1,2) Modeul residuals of original data')


# plotof all arima models
par(mfrow=c(2,2))
period <- 10
fcast1 <- forecast(fit1, h = period)
plot(fcast1)

fcast2 <- forecast(fit2, h = period)
plot(fcast2)

fcast3 <- forecast(fit3, h = period)
plot(fcast3)

fcast4 <- forecast(fit4, h = period)
plot(fcast4)

# look closer on the orignal dataset
par(mfrow=c(1,2))
plot(fcast3)
plot(fcast4)

accuracy(fcast1) # inf 
accuracy(fcast2) # inf
accuracy(fcast3)
accuracy(fcast4)



### Conclusion 
# JPM stock price will go decrease within the next 100 days by 30%
