###########################################
# Get monthly unemployment data from FREDMD
############################################
uemploy_monthly = read.csv('data/unemployment_rate_monthly.csv')

# out of sample evaluation of 120 observations (10 years of data)
train = uemploy_monthly[1:360,]
test = uemploy_monthly[361:nrow(uemploy_monthly), ]

sum(is.na(train)) # check for na values



###############################
# Plot series
###############################
train$month = as.Date(train$month)
plot(train$month, train$unemployment_rate, main = "Monthly Unemployment Rate", 
     xlab = "Year", ylab = "Unemployment (%)")


####################################################################
# Check for stationarity using ADF test monthly unemployment rate
####################################################################
library(tseries)
adf.test(train$unemployment_rate)
# The p value from the ADF test is 0.2344, therefore we fail to reject the null hypothesis
# at 95% significance level and conclude that the series is non-stationary

# Difference the series 1 time (d = 1)
uemploy_diff <- diff(train$unemployment_rate)
adf.test(uemploy_diff)
# The p value from the ADF test is now 0.01, therefore we reject the null hypothesis
# at 95% significance level and conclude that the series is stationary. Therefore, d = 1

###########################################################
# Search for the best Arima model (p,1,q) using auto_arima
###########################################################
library(forecast)
train2 = ts(train$unemployment_rate, start = c(1980, 1), frequency = 12) # convert to time series object

arima_aic = auto.arima(train2, d = 1, ic = "aic", stepwise = FALSE, approximation = FALSE)
summary(arima_aic) 
# arima(1,1,2)(0,0,2)[12]
# AIC = -307.06
# BIC = -283.76

arima_bic = auto.arima(train2, d = 1, ic = "bic", stepwise = FALSE, approximation = FALSE)
summary(arima_bic)
# arima(1,1,2)(0,0,2)[12], same as arima_aic
# AIC = -307.06
# BIC = -283.76

# Allow stepwise and approximation = TRUE
arima_aic2 = auto.arima(train2, d=1, ic = "aic", stepwise = TRUE, approximation = TRUE)
summary(arima_aic2)
# arima(5,1,3)(2,0,1)[12] 
# AIC = -318.93
# BIC = -272.33

arima_bic2 = auto.arima(train2, d = 1, ic = "bic", stepwise = TRUE, approximation = TRUE)
summary(arima_bic2)
# arima(1,1,2)(2,0,1)[12] 
# AIC = -311.73
# BIC = -284.55

