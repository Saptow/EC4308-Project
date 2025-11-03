# Random forest forecast

# load FRED-MD data
load("data/fredmd.RData")
library(randomForest)

Y = md
Y = Y[,-59] #drop New Orders for Consumer Goods (ACOGNO) due to insufficient data

#Create dummy variable after Nov 2010 to handle structural break
cutoff = as.Date("2010-11-01")
Y$DUM = ifelse(Y[, 1] > cutoff, 1, 0)
nprev = 120 #test size

####################################################################################

#Use random forest 2 (base)
source("Tree/func-rf2.R")
rf12c = rf2.rolling.window(Y,nprev,h=1, "UNRATE")
rf32c = rf2.rolling.window(Y,nprev,h=3, "UNRATE")
rf62c = rf2.rolling.window(Y,nprev,h=6, "UNRATE")
rf122c = rf2.rolling.window(Y,nprev,h=12, "UNRATE")

#Use MARX random forest
source("Tree/func-marx_rf.R")
source("data_transformation/marx_transform.R")
marx_rf1 = marx_rf.rolling.window(Y, nprev, h=1, "UNRATE")
marx_rf3 = marx_rf.rolling.window(Y, nprev, h=3, "UNRATE")
marx_rf6 = marx_rf.rolling.window(Y, nprev, h=6, "UNRATE")
marx_rf12 = marx_rf.rolling.window(Y, nprev, h=12, "UNRATE")

#Use MAF random forest
source("Tree/func-maf_rf.R")
source("data_transformation/maf_transform.R")  
maf_rf1 = maf_rf.rolling.window(Y, nprev, h=1, "UNRATE")
maf_rf3 = maf_rf.rolling.window(Y, nprev, h=3, "UNRATE")
maf_rf6 = maf_rf.rolling.window(Y, nprev, h=6, "UNRATE")
maf_rf12 = maf_rf.rolling.window(Y, nprev, h=12, "UNRATE")

