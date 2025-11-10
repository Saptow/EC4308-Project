# Random forest forecast

# load FRED-MD data
load("./data/fredmd_cleaned.RData")
library(ranger)
Y = md

#Create dummy variable after Nov 2010 to handle structural break
nprev = 120 #test size
# Rename aft_break to DUM for script consistency
names(Y)[names(Y) == "aft_break"] <- "DUM"
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

