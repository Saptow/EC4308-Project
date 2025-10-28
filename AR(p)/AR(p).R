# AR(p) benchmark model

# load FRED-MD data
load("data/fredmd.RData")

Y = md
nprev = 120
Ytrain = head(Y, nrow(Y) - nprev)
idx = which(colnames(Ytrain) == "UNRATE") # index for unemployment rate
yy = Y[, "UNRATE"]  
oosy = tail(yy, nprev)    # out-of-sample true values

# Checking for outliers in unrate
library(forecast)
unrate_ts <- ts(Ytrain[["UNRATE"]], frequency = 12)  
out <- forecast::tsoutliers(unrate_ts)
out$index            
out$replacements     
time(unrate_ts)[out$index]  


source("Ar(p)/func-ar.R")
# Test using rolling window 
bar1c=ar.rolling.window(Y,nprev,idx,1,type="bic") #1-step AR forecast
bar3c=ar.rolling.window(Y,nprev,idx,3,type="bic") #3-step AR forecast
bar6c=ar.rolling.window(Y,nprev,idx,6,type="bic") #6-step AR forecast
bar12c=ar.rolling.window(Y,nprev,idx,12,type="bic") #12-step AR forecast

get_p <- function(coef_row){
  nz <- which(abs(coef_row[-1]) > 1e-6) 
  if(length(nz) == 0) return(0)
  max(nz)
}

p1 = apply(bar1c$coef, 1, get_p)
p3 = apply(bar3c$coef, 1, get_p)
p6 = apply(bar6c$coef, 1, get_p)
p12 = apply(bar12c$coef, 1, get_p)

# Plotting
arcoef.ts=ts(bar1c$coef, start=c(2010,1), end=c(2019,12), freq=12)
colnames(arcoef.ts)=c("Constant","Phi1","Phi2","Phi3","Phi4") #name columns to distinguish plots

#Plot all the coefficients over time (plot.ts() same as plot, but for time series objects):
plot.ts(arcoef.ts, main="AR regression coefficients for h=1", cex.axis=1.5)

# Compare actual vs predicted for h=1
bench1.ts=ts(cbind(bar1c$pred,oosy), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench1.ts)=c("AR","True")
plot.ts(bench1.ts[,"True"], main="1-step Ahead Forecast (AR vs Actual)", cex.axis=1.2, lwd=2, col="black", 
        ylab="Change in Unemployment Rate")
lines(bench1.ts[,"AR"], col="red", lwd=1.8)
legend("topright", 
       legend=c("AR(p)", "Actual Change"),
       col=c("red","black"),
       lty=c(1,1), lwd=c(1.8,2), 
       bty="n")

# Compare actual vs predicted for h = 3
bench3.ts=ts(cbind(bar3c$pred,oosy), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench3.ts)=c("AR","True")
plot.ts(bench3.ts[,"True"], main="3-step Ahead Forecast (AR vs Actual)", cex.axis=1.2, lwd=2, col="black", 
        ylab="Change in Unemployment Rate")
lines(bench3.ts[,"AR"], col="red", lwd=1.8)
legend("topright", 
       legend=c("AR(p)", "Actual Change"),
       col=c("red","black"),
       lty=c(1,1), lwd=c(1.8,2), 
       bty="n")

# Compare actual vs predicted for h=6
bench6.ts=ts(cbind(bar6c$pred,oosy), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench6.ts)=c("AR","True")
plot.ts(bench6.ts[,"True"], main="6-step Ahead Forecast (AR vs Actual)", cex.axis=1.2, lwd=2, col="black", 
        ylab="Change in Unemployment Rate")
lines(bench6.ts[,"AR"], col="red", lwd=1.8)
legend("topright", 
       legend=c("AR(p)", "Actual Change"),
       col=c("red","black"),
       lty=c(1,1), lwd=c(1.8,2), 
       bty="n")

# Compare actual vs predicted for h=12
bench12.ts=ts(cbind(bar12c$pred,oosy), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench12.ts)=c("AR","True")
plot.ts(bench12.ts[,"True"], main="12-step Ahead Forecast (AR vs Actual)", cex.axis=1.2, lwd=2, col="black", 
        ylab="Change in Unemployment Rate")
lines(bench12.ts[,"AR"], col="red", lwd=1.8)
legend("topright", 
       legend=c("AR(p)", "Actual Change"),
       col=c("red","black"),
       lty=c(1,1), lwd=c(1.8,2), 
       bty="n")

#AR forecasts RMSE:
ar.rmse1=bar1c$errors[1]
ar.rmse3=bar3c$errors[1]
ar.rmse6=bar6c$errors[1]
ar.rmse12=bar12c$errors[1]






