# Example of how we can estimate an autoregressive model in R

# Clear the Workspace
rm(list=ls())

# Load the relevant libraries
library('Quandl')
Quandl.api_key("UMo9bYuaDpyAy55mMfGS")
library(forecast)

# We are interested in predicting output growth, with an autoregressive model
data=Quandl(c('FRED/GDPC1'),collapse='quarterly', end_date="2019-12-31",type="xts")
Y = na.exclude(400*diff(log(data[,1])))


reg_ts = lm(Y~lag(Y,1))
summary(reg_ts)

reg_ts_alt = ar.ols(Y, aic = FALSE, 1,demean = FALSE, intercept = TRUE)
reg_ts_alt


# estimating with an ARIMA command
ar = arima(Y,c(1,0,0), method = 'CSS')
summary(ar)

ar1 = arima(Y,c(1,0,0))
summary(ar1)

ar2 = arima(Y,c(2,0,0))
summary(ar2)

ar4 = arima(Y,c(4,0,0))
summary(ar4)

ar12 = arima(Y,c(12,0,0))
summary(ar12)

fitted(ar)
forecast(ar,h=1)$mean
forecast(ar,h=12)$mean

dates = seq(as.Date("2020-1-31"), length= 12, by = "quarters")
forecast_date = xts(x = forecast(ar,h=12)$mean, order.by = dates)
plot(forecast_date)

