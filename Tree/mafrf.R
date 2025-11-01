# load FRED-MD data
load("data/fredmd.RData")
library(randomForest)
library(ranger)

Y = md
Y = Y[,-59] #drop New Orders for Consumer Goods (ACOGNO) due to insufficient data
nprev = 120
idx = which(colnames(Y) == "UNRATE") # index for unemployment rate


#############################################################################
source("Tree/func-maf_rf.R")
tune_results <- tune_maf_rf(
  Y[1:360, ], 
  indice = idx, 
  hstep = 1, 
  mtry_grid = c(20, 40, 80),
  min.node.size_grid = c(2, 5, 10),
  sample.fraction_grid = c(0.6, 0.8, 1.0),
  p_lag_x = 4, q_maf = 2, L_y = 4,
  train_size = 300,
  verbose = TRUE
)

best_params <- tune_results[1, ]
best_params

mafrf1c <- mafrf.rolling.window(
  Y, nprev = nprev, indice = idx, hstep = 1,
  p_lag_x = 4, q_maf = 2, L_y = 4,
  scale_lags = TRUE,
  mtry = best_params$mtry,
  min.node.size = best_params$min.node.size,
  sample.fraction = best_params$sample.fraction,
  verbose = TRUE
)

########################################################
source("Tree/func-maf_rf+.R")

train_size <- 300
feats <- build_maf_rf_features(
  Y, indice = idx, hstep = 1,
  p_lag_x = 4, q_maf = 2, L_y = 4, scale_lags = TRUE,
  pca_train_rows = train_size
)


best <- tune_maf_rf_precomp(
  feats, train_size = train_size,
  mtry_grid = c(20, 40, 80),
  min.node.size_grid = c(2, 5, 10),
  sample.fraction_grid = c(0.6, 0.8, 1.0),
  num.trees = 500
)
best[1, ]

