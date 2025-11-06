# Import necessary libraries
library(ranger) # use instead of rf for faster random forest and better regularisation settings
library(glmnet)
library(hdm)
library(gbm)

############################################
####Hybrid method: alternating LASSO and RF
############################################

# Set seed for reproducibility
set.seed(12345)

# Read in data
load("data/fredmd_cleaned.RData")
Y=md
nprev=120

# Function to run random forest with PCA preprocessing and h-step ahead forecast
h_windows <- c(1,3,6,12)
Y <- as.matrix(Y)

# ============================================
# Run hybrid rolling window forecasts for different horizons (rLASSO + RF on residuals)
# ============================================
source("hybrid/func-hybrid.R")
for (h in h_windows){
    hybrid_fit = hybrid.rolling.window(Y, nprev, h, "UNRATE")
    save(hybrid_fit, file=paste0("./hybrid/hybrid_fit_h",h,".RData"))
}

# Evaluate RMSE and MAE (using table)
for (h in h_windows){
    load(paste0("./hybrid/hybrid_fit_h",h,".RData"))
    print(paste0("Horizon: ", h))
    print(hybrid_fit$errors)
}

# ============================================
# Run hybrid rolling window forecasts for different horizons (rLASSO + RF on residuals with MAF)
# ============================================
source("./hybrid/func-hybrid-maf.R")
for (h in h_windows){
    hybrid_maf_fit = hybrid_maf.rolling.window(Y, nprev, h, "UNRATE")
    save(hybrid_maf_fit, file=paste0("./hybrid/hybrid_maf_fit_h",h,".RData"))
}
hybrid_maf_fit = hybrid_maf.rolling.window(Y, nprev, 1, "UNRATE")
# Evaluate RMSE and MAE (using table)
for (h in h_windows){
    load(paste0("./hybrid/hybrid_maf_fit_h",h,".RData"))
    print(paste0("Horizon: ", h))
    print(hybrid_maf_fit$errors)
}

# ============================================
# Run hybrid rolling window forecasts for different horizons (rLASSO + RF on residuals with MARX)
# ============================================
source("./hybrid/func-hybrid-marx.R")
for (h in h_windows){
    hybrid_marx_fit = hybrid_marx.rolling.window(Y, nprev, h, "UNRATE")
    save(hybrid_marx_fit, file=paste0("./hybrid/hybrid_marx_fit_h",h,".RData"))
}
hybrid_marx_fit = hybrid_marx.rolling.window(Y, nprev, 1, "UNRATE")

# Evaluate RMSE and MAE (using table)
for (h in h_windows){
    load(paste0("./hybrid/hybrid_marx_fit_h",h,".RData"))
    print(paste0("Horizon: ", h))
    print(hybrid_marx_fit$errors)
}