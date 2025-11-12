load("data/fredmd_cleaned.RData") 
source("Lasso/func-lasso.R")

library(glmnet)
library(hdm)

Y = md
nprev=120

########################################
#Penalized regression: LASSO forecasts
########################################

#Run forecasts for LASSO
lasso1c=lasso.rolling.window(Y,nprev,h=1,target_name = "UNRATE")
lasso3c=lasso.rolling.window(Y,nprev,h=3,target_name = "UNRATE")
lasso6c=lasso.rolling.window(Y,nprev,h=6,target_name = "UNRATE")
lasso12c=lasso.rolling.window(Y,nprev,h=12,target_name = "UNRATE")

##rmse
lasso.rmse1=lasso1c$errors[1]
lasso.rmse3=lasso3c$errors[1]
lasso.rmse6=lasso6c$errors[1]
lasso.rmse12=lasso12c$errors[1]

print(lasso.rmse1)
print(lasso.rmse3)
print(lasso.rmse6)
print(lasso.rmse12)

#Create the time series object collecting 1-step best=performing ML forecasts
bench1.ts=ts(cbind(tail(Y[, "UNRATE"], 120),lasso1c$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench1.ts)=c("True Value","LASSO")

#Create the time series object collecting 1-step best=performing ML forecasts
bench3.ts=ts(cbind(tail(Y[, "UNRATE"], 120),lasso3c$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench3.ts)=c("True Value","LASSO")

#Create the time series object collecting 1-step best=performing ML forecasts
bench6.ts=ts(cbind(tail(Y[, "UNRATE"], 120),lasso6c$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench6.ts)=c("True Value","LASSO")

#Create the time series object collecting 1-step best=performing ML forecasts
bench12.ts=ts(cbind(tail(Y[, "UNRATE"], 120),lasso12c$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench12.ts)=c("True Value","LASSO")

par(mfrow = c(2,2))

#Plot the graph for 1-step forecasts
plot.ts(bench1.ts[,1], main="1-step LASSO forecast", cex.axis=1.5, lwd=1, ylab="UNRATE")
points(bench1.ts[,2], type="l", col="red",lwd=1)
#legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))

#Plot the graph for 3-step forecasts
plot.ts(bench3.ts[,1], main="3-step LASSO forecast", cex.axis=1.5, lwd=1, ylab="UNRATE")
points(bench3.ts[,2], type="l", col="red",lwd=1)
#legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))

#Plot the graph for 6-step forecasts
plot.ts(bench6.ts[,1], main="6-step LASSO forecast", cex.axis=1.5, lwd=1, ylab="UNRATE")
points(bench6.ts[,2], type="l", col="red",lwd=1)
#legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))

#Plot the graph for 12-step forecasts
plot.ts(bench12.ts[,1], main="12-step LASSO forecast", cex.axis=1.5, lwd=1, ylab="UNRATE")
points(bench12.ts[,2], type="l", col="red",lwd=1)
#legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))




####Sparsity Analysis###

#Get nonzero coefficient numbers for different horizons (LASSO(BIC))
thr <- 1e-8
c1c  <- sapply(lasso1c$save.coef,  function(cf) if (is.null(cf)) NA_integer_ else sum(abs(cf) > thr))
c3c  <- sapply(lasso3c$save.coef,  function(cf) if (is.null(cf)) NA_integer_ else sum(abs(cf) > thr))
c6c  <- sapply(lasso6c$save.coef,  function(cf) if (is.null(cf)) NA_integer_ else sum(abs(cf) > thr))
c12c <- sapply(lasso12c$save.coef, function(cf) if (is.null(cf)) NA_integer_ else sum(abs(cf) > thr))

#Create a ts object for the plot
lcoef.ts=ts(cbind(c1c,c3c,c6c,c12c), start=c(2010,1), end=c(2019,12), freq=12)
colnames(lcoef.ts)=c("1-step","3-step","6-step","12-step")
#Plot numbers of nonzero coefficients across the test window
plot.ts(lcoef.ts, main="Sparsity Analysis for LASSO",cex.axis=1.5)

avg_nonzero <- c(
  h1  = mean(c1c,  na.rm = TRUE),
  h3  = mean(c3c,  na.rm = TRUE),
  h6  = mean(c6c,  na.rm = TRUE),
  h12 = mean(c12c, na.rm = TRUE)
)
avg_nonzero

save(lasso1c, file="lasso_h1.RData")
save(lasso3c, file="lasso_h3.RData")
save(lasso6c, file="lasso_h6.RData")
save(lasso12c, file="lasso_h12.RData")
