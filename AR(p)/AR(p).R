# AR(p) benchmark model

# load FRED-MD data
load("data/fredmd.RData")

Y = md
yy = Y[, "UNRATE"] # get target variable: Unemployment rate
nprev = 120 #number of out-of-sample observations (test window )
oosy = tail(yy,nprev) #out-of-sample true values
trainyy = head(yy, nrow(Y) - nprev) #training sample

source("func-ar.R")
# select best ar(p) model on bic
bar1c = runAR(trainyy, lag = 1, type = "bic")
bar3c = runAR(trainyy, lag = 3, type = "bic")
bar6c = runAR(trainyy, lag = 6, type = "bic")
bar12c = runAR(trainyy, lag = 12, type = "bic")

# see selected lags
bar1c$coef
bar3c$coef
bar6c$coef
bar12c$coef



