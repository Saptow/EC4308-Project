# load FRED-MD data
load("data/fredmd.RData")
library(randomForest)

Y = md
Y = Y[,-59] #drop New Orders for Consumer Goods (ACOGNO) due to insufficient data
nprev = 120
idx = which(colnames(Y) == "UNRATE") # index for unemployment rate

source("Tree/func-marx_rf.R")
marxrf1c=marxrf.rolling.window(Y,nprev,idx,hstep=1, L_y = 4, P_marx = 4)
marxrf1c$errors[1] #rsme 1.027358
marxrf1c$errors[2] #mae 0.803269


