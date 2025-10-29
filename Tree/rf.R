# Random forest forecast

# load FRED-MD data
load("data/fredmd.RData")

Y = md
Y = Y[,-59] #drop New Orders for Consumer Goods (ACOGNO) due to insufficient data
nprev = 120
idx = which(colnames(Y) == "UNRATE") # index for unemployment rate
oosy = tail(yy, nprev)    
#sum(is.na(Y))
#which(rowSums(is.na(Y)) > 0)
#Y[253:398, ]
#which(is.na(Y[253:398, ]), arr.ind = TRUE)

source("Tree/func-rf.R")
rf1c=rf.rolling.window(Y,nprev,idx,1)
rf3c=rf.rolling.window(Y,nprev,idx,3)
rf6c=rf.rolling.window(Y,nprev,idx,6)
rf12c=rf.rolling.window(Y,nprev,idx,12)

#See the RMSE:
rf.rmse1=rf1c$errors[1]
rf.rmse3=rf3c$errors[1]
rf.rmse6=rf6c$errors[1]
rf.rmse12=rf12c$errors[1]

# Plotting actual vs predicted values
dates = tail(Y$date, 120)
plot(dates, oosy, type = "l", col = "black", lwd = 2,
     ylab = "Unemployment rate", xlab = "Date",
     main = "Random Forest Forecast vs Actual (1-step ahead)")
lines(dates, rf1c$pred, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Predicted"),
       col = c("black", "red"),
       lty = 1, lwd = 2)

