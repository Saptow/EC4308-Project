# Diebold-Mariano (DM) tests 
# Comparing forecasting models against AR benchmark

rm(list=ls())

# Required libraries
library(sandwich)   # for NeweyWest HAC estimator
library(lmtest)     # for coeftest, regression testing tools
library(stats)      # for lm(), acf(), ts(), plot.ts()
library(graphics)   # for plotting functions (plot.ts)

# Load all files
# Data
load("data/fredmd_cleaned.RData")  

# AR
load("AR(p)/ar_h1.RData")
load("AR(p)/ar_h3.RData")
load("AR(p)/ar_h6.RData")
load("AR(p)/ar_h12.RData")

# ADL rolling fixed (w/o PCA)
load("ADL/adl_rolling_fixed_h1.RData")
adl_bench_1c <- res_fixed
rm(res_fixed)

load("ADL/adl_rolling_fixed_h3.RData")
adl_bench_3c <- res_fixed
rm(res_fixed)

load("ADL/adl_rolling_fixed_h6.RData")
adl_bench_6c <- res_fixed
rm(res_fixed)

load("ADL/adl_rolling_fixed_h12.RData")
adl_bench_12c <- res_fixed
rm(res_fixed)

# ADL PCA Benchmark 
load("ADL/pca_adl_rolling_fixed_h1.RData")
adl_benchpca_1c <- res_fixed
rm(res_fixed)

load("ADL/pca_adl_rolling_fixed_h3.RData")
adl_benchpca_3c <- res_fixed
rm(res_fixed)

load("ADL/pca_adl_rolling_fixed_h6.RData")
adl_benchpca_6c <- res_fixed
rm(res_fixed)

load("ADL/pca_adl_rolling_fixed_h12.RData")
adl_benchpca_12c <- res_fixed
rm(res_fixed)

# ADL PCA MAF 
load("ADL/pca_adl_rolling_maf_h1.RData")
adl_mafpca_1c <- res_maf 
rm(res_maf)

load("ADL/pca_adl_rolling_maf_h3.RData")
adl_mafpca_3c <- res_maf 
rm(res_maf)

load("ADL/pca_adl_rolling_maf_h6.RData")
adl_mafpca_6c <- res_maf 
rm(res_maf)

load("ADL/pca_adl_rolling_maf_h12.RData")
adl_mafpca_12c <- res_maf 
rm(res_maf)

# ADL PCA MARX 
load("ADL/pca_adl_rolling_marx_h1.RData")
adl_marxpca_1c <- res_marx
rm(res_marx)

load("ADL/pca_adl_rolling_marx_h3.RData")
adl_marxpca_3c <- res_marx
rm(res_marx)

load("ADL/pca_adl_rolling_marx_h6.RData")
adl_marxpca_6c <- res_marx
rm(res_marx)

load("ADL/pca_adl_rolling_marx_h12.RData")
adl_marxpca_12c <- res_marx
rm(res_marx)

# LASSO     
load("Lasso/lasso_h1.RData")   
load("Lasso/lasso_h3.RData")
load("Lasso/lasso_h6.RData")
load("Lasso/lasso_h12.RData")
load ("Lasso/maf_lasso_h1.RData") 
load ("Lasso/maf_lasso_h3.RData")
load ("Lasso/maf_lasso_h6.RData")
load ("Lasso/maf_lasso_h12.RData")
load ("Lasso/marx_lasso_h1.RData")
load ("Lasso/marx_lasso_h3.RData")
load ("Lasso/marx_lasso_h6.RData")
load ("Lasso/marx_lasso_h12.RData")

# RF
load("Tree/rf_h1.RData")   
load("Tree/rf_h3.RData")
load("Tree/rf_h6.RData")
load("Tree/rf_h12.RData")
load("Tree/maf_rf_h1.RData")   
load("Tree/maf_rf_h3.RData")
load("Tree/maf_rf_h6.RData")
load("Tree/maf_rf_h12.RData")
load("Tree/marx_rf_h1.RData")
load("Tree/marx_rf_h3.RData")
load("Tree/marx_rf_h6.RData")
load("Tree/marx_rf_h12.RData")

# Hybrid
load("hybrid/hybrid_fit_h1.RData") 
hybrid_bench_1c <- hybrid_fit
rm(hybrid_fit)

load("hybrid/hybrid_fit_h3.RData")
hybrid_bench_3c <- hybrid_fit
rm(hybrid_fit)

load("hybrid/hybrid_fit_h6.RData")
hybrid_bench_6c <- hybrid_fit
rm(hybrid_fit)

load("hybrid/hybrid_fit_h12.RData")
hybrid_bench_12c <- hybrid_fit
rm(hybrid_fit)

load("hybrid/hybrid_maf_fit_h1.RData")
hybrid_maf_1c <- hybrid_maf_fit
rm(hybrid_maf_fit)

load("hybrid/hybrid_maf_fit_h3.RData")
hybrid_maf_3c <- hybrid_maf_fit
rm(hybrid_maf_fit)

load("hybrid/hybrid_maf_fit_h6.RData")
hybrid_maf_6c <- hybrid_maf_fit
rm(hybrid_maf_fit)

load("hybrid/hybrid_maf_fit_h12.RData")
hybrid_maf_12c <- hybrid_maf_fit
rm(hybrid_maf_fit)

load("hybrid/hybrid_marx_fit_h1.RData")
hybrid_marx_1c <- hybrid_marx_fit
rm(hybrid_marx_fit)

load("hybrid/hybrid_marx_fit_h3.RData")
hybrid_marx_3c <- hybrid_marx_fit
rm(hybrid_marx_fit)

load("hybrid/hybrid_marx_fit_h6.RData")
hybrid_marx_6c <- hybrid_marx_fit
rm(hybrid_marx_fit)

load("hybrid/hybrid_marx_fit_h12.RData")
hybrid_marx_12c <- hybrid_marx_fit
rm(hybrid_marx_fit)

# Wrapper Function 1: Run full DM test for model vs AR
run_dm_test_vs_ar <- function(model_name, loss_ar, loss_model, plot_results = TRUE) {
  
  # Horizons
  horizons <- c("1c", "3c", "6c", "12c")
  horizon_names <- c("1-step", "3-step", "6-step", "12-step")
  
  # Storage for results
  dm_stats <- numeric(4)
  dm_objects <- list()
  loss_diffs <- list()
  
  # Loop through horizons
  for (i in 1:4) {
    h <- horizons[i]
    
    # Compute loss differential (AR - Model)
    loss_diff <- loss_ar[[h]] - loss_model[[h]]
    loss_diffs[[h]] <- loss_diff
    
    # DM regression
    dm_reg <- lm(loss_diff ~ 1)
    dm_objects[[h]] <- dm_reg
    
    # Check ACF
    acf(dm_reg$residuals, main = paste(model_name, horizon_names[i]))
    
    # Compute DM statistic
    dm_stat <- as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = 5)))
    dm_stats[i] <- dm_stat
    
    # Print result
    cat(sprintf("%s vs AR - %s: DM = %.3f\n", model_name, horizon_names[i], dm_stat))
  }
  
  # Create plot 
  if (plot_results) {
    max_len <- max(sapply(loss_diffs, length))
    
    # Pad each series to max length
    loss_diff_list <- lapply(loss_diffs, function(x) {
      c(rep(NA, max_len - length(x)), x)  # Pad at the beginning with NA
    })
    
    loss_diff_matrix <- do.call(cbind, loss_diff_list)
    colnames(loss_diff_matrix) <- horizon_names
    
    # Convert to time series object with dates
    loss_diff_ts <- ts(loss_diff_matrix, start = c(2010, 1), end = c(2019, 12), frequency = 12)
    
    plot.ts(loss_diff_ts, main = paste("Loss differential: AR -", model_name), 
            cex.axis = 1.2, nc = 2, xlab = "Year") 
  }
  
  # Return results
  return(list(
    dm_stats = dm_stats,
    dm_objects = dm_objects,
    loss_diffs = loss_diffs
  ))
}

# Wrapper Function 2: Run full DM test for model vs ADL without PCA 
run_dm_test_vs_adl <- function(model_name, loss_adl, loss_model, plot_results = TRUE) {
  
  horizons <- c("1c", "3c", "6c", "12c")
  horizon_names <- c("1-step", "3-step", "6-step", "12-step")
  
  dm_stats <- numeric(4)
  dm_objects <- list()
  loss_diffs <- list()
  
  for (i in 1:4) {
    h <- horizons[i]
    
    loss_diff <- loss_adl[[h]] - loss_model[[h]]
    loss_diffs[[h]] <- loss_diff
    
    dm_reg <- lm(loss_diff ~ 1)
    dm_objects[[h]] <- dm_reg
    
    acf(dm_reg$residuals, main = paste(model_name, horizon_names[i]))
    
    dm_stat <- as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = 5)))
    dm_stats[i] <- dm_stat
    
    cat(sprintf("%s vs ADL - %s: DM = %.3f\n", model_name, horizon_names[i], dm_stat))
  }
  
  if (plot_results) {
    max_len <- max(sapply(loss_diffs, length))
    
    loss_diff_list <- lapply(loss_diffs, function(x) {
      c(rep(NA, max_len - length(x)), x)
    })
    
    loss_diff_matrix <- do.call(cbind, loss_diff_list)
    colnames(loss_diff_matrix) <- horizon_names
    
    loss_diff_ts <- ts(loss_diff_matrix, start = c(2010, 1), end = c(2019, 12), frequency = 12)
    
    plot.ts(loss_diff_ts, main = paste("Loss differential: ADL -", model_name), 
            cex.axis = 1.2, nc = 2, xlab = "Year") 
  }
  
  return(list(
    dm_stats = dm_stats,
    dm_objects = dm_objects,
    loss_diffs = loss_diffs
  ))
}

# Wrapper Function 3: Run full DM test for model vs any benchmark
run_dm_test <- function(model_name1, model_name2, loss_model1, loss_model2, plot_results = TRUE) {
  
  # Horizons
  horizons <- c("1c", "3c", "6c", "12c")
  horizon_names <- c("1-step", "3-step", "6-step", "12-step")
  
  # Storage for results
  dm_stats <- numeric(4)
  dm_objects <- list()
  loss_diffs <- list()
  
  # Loop through horizons
  for (i in 1:4) {
    h <- horizons[i]
    
    # Compute loss differential (AR - Model)
    loss_diff <- loss_model1[[h]] - loss_model2[[h]]
    loss_diffs[[h]] <- loss_diff
    
    # DM regression
    dm_reg <- lm(loss_diff ~ 1)
    dm_objects[[h]] <- dm_reg
    
    # Compute DM statistic
    dm_stat <- as.numeric(dm_reg$coefficients / sqrt(NeweyWest(dm_reg, lag = 5)))
    dm_stats[i] <- dm_stat
    
    # Print result
    cat(sprintf("%s vs %s - %s: DM = %.3f\n", model_name1, model_name2, horizon_names[i], dm_stat))
  }
  
  # Return results
  return(list(
    dm_stats = dm_stats,
    dm_objects = dm_objects,
    loss_diffs = loss_diffs
  ))
}

# STEP 1: Prepare loss differentials for all models
Y = md
yy = Y[, "UNRATE"]
n_total <- length(yy)
nprev <- 120
oos_start <- n_total - nprev + 1

# True values for each horizon
oosy_1  <- yy[oos_start:n_total]
oosy_3  <- yy[(oos_start+2):n_total]
oosy_6  <- yy[(oos_start+5):n_total]
oosy_12 <- yy[(oos_start+11):n_total]

# Function to clean NA values
clean_pred <- function(pred_vector) {
  first_valid <- which(!is.na(pred_vector))[1]
  return(pred_vector[first_valid:length(pred_vector)])
}

# AR
loss_ar <- list(
  "1c"  = (oosy_1 - clean_pred(bar1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(bar3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(bar6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(bar12c$pred))^2
)

# ADL (no PCA)
loss_adl_bench <- list(
  "1c"  = (oosy_1 - adl_bench_1c$pred)^2,
  "3c"  = (oosy_3 - adl_bench_3c$pred)^2,
  "6c"  = (oosy_6 - adl_bench_6c$pred)^2,
  "12c" = (oosy_12 - adl_bench_12c$pred)^2
)

# ADL PCA Benchmark
loss_adl_benchpca <- list(
  "1c"  = (oosy_1 - adl_benchpca_1c$pred)^2,
  "3c"  = (oosy_3 - adl_benchpca_3c$pred)^2,
  "6c"  = (oosy_6 - adl_benchpca_6c$pred)^2,
  "12c" = (oosy_12 - adl_benchpca_12c$pred)^2
)

# ADL MAF PCA
loss_adl_mafpca <- list(
  "1c"  = (oosy_1 - adl_mafpca_1c$pred)^2,
  "3c"  = (oosy_3 - adl_mafpca_3c$pred)^2,
  "6c"  = (oosy_6 - adl_mafpca_6c$pred)^2,
  "12c" = (oosy_12 - adl_mafpca_12c$pred)^2
)

# ADL MARX PCA
loss_adl_marxpca <- list(
  "1c"  = (oosy_1 - adl_marxpca_1c$pred)^2,
  "3c"  = (oosy_3 - adl_marxpca_3c$pred)^2,
  "6c"  = (oosy_6 - adl_marxpca_6c$pred)^2,
  "12c" = (oosy_12 - adl_marxpca_12c$pred)^2
)

# LASSO
loss_lasso_bench <- list(
  "1c"  = (oosy_1 - clean_pred(lasso1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(lasso3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(lasso6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(lasso12c$pred))^2
)

# LASSO MAF
loss_lasso_maf <- list(
  "1c"  = (oosy_1 - clean_pred(maf_lasso1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(maf_lasso3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(maf_lasso6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(maf_lasso12c$pred))^2
)

# LASSO MARX
loss_lasso_marx <- list(
  "1c"  = (oosy_1 - clean_pred(marx_lasso1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(marx_lasso3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(marx_lasso6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(marx_lasso12c$pred))^2
)

# RF
loss_rf_bench <- list(
  "1c"  = (oosy_1 - clean_pred(rf12c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(rf32c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(rf62c$pred))^2,
  "12c" = (oosy_12 - clean_pred(rf122c$pred))^2
)

# RF MAF
loss_rf_maf <- list(
  "1c"  = (oosy_1 - clean_pred(maf_rf1$pred))^2,
  "3c"  = (oosy_3 - clean_pred(maf_rf3$pred))^2,
  "6c"  = (oosy_6 - clean_pred(maf_rf6$pred))^2,
  "12c" = (oosy_12 - clean_pred(maf_rf12$pred))^2
)

# RF MARX
loss_rf_marx <- list(
  "1c"  = (oosy_1 - clean_pred(marx_rf1$pred))^2,
  "3c"  = (oosy_3 - clean_pred(marx_rf3$pred))^2,
  "6c"  = (oosy_6 - clean_pred(marx_rf6$pred))^2,
  "12c" = (oosy_12 - clean_pred(marx_rf12$pred))^2
)

# Hybrid
loss_hybrid_bench <- list(
  "1c"  = (oosy_1 - clean_pred(hybrid_bench_1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(hybrid_bench_3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(hybrid_bench_6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(hybrid_bench_12c$pred))^2
)

# Hybrid MAF
loss_hybrid_maf <- list(
  "1c"  = (oosy_1 - clean_pred(hybrid_maf_1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(hybrid_maf_3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(hybrid_maf_6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(hybrid_maf_12c$pred))^2
)

# Hybrid MARX
loss_hybrid_marx <- list(
  "1c"  = (oosy_1 - clean_pred(hybrid_marx_1c$pred))^2,
  "3c"  = (oosy_3 - clean_pred(hybrid_marx_3c$pred))^2,
  "6c"  = (oosy_6 - clean_pred(hybrid_marx_6c$pred))^2,
  "12c" = (oosy_12 - clean_pred(hybrid_marx_12c$pred))^2
)

# STEP 2A: Run DM tests for all models vs AR
# Run tests
cat("\n--- ADL (no PCA) ---\n")
dm_adl_bench_vs_ar <- run_dm_test_vs_ar("ADL", loss_ar, loss_adl_bench)

cat("\n--- ADL PCA Models ---\n")
dm_adl_benchpca_vs_ar <- run_dm_test_vs_ar("ADL PCA", loss_ar, loss_adl_benchpca)
dm_adl_mafpca_vs_ar   <- run_dm_test_vs_ar("ADL MAF PCA", loss_ar, loss_adl_mafpca)
dm_adl_marxpca_vs_ar  <- run_dm_test_vs_ar("ADL MARX PCA", loss_ar, loss_adl_marxpca)

cat("\n--- LASSO Models ---\n")
dm_lasso_bench_vs_ar <- run_dm_test_vs_ar("LASSO", loss_ar, loss_lasso_bench)
dm_lasso_maf_vs_ar   <- run_dm_test_vs_ar("LASSO MAF", loss_ar, loss_lasso_maf)
dm_lasso_marx_vs_ar  <- run_dm_test_vs_ar("LASSO MARX", loss_ar, loss_lasso_marx)

cat("\n--- RF Models ---\n")
dm_rf_bench_vs_ar <- run_dm_test_vs_ar("RF", loss_ar, loss_rf_bench)
dm_rf_maf_vs_ar   <- run_dm_test_vs_ar("RF MAF", loss_ar, loss_rf_maf)
dm_rf_marx_vs_ar  <- run_dm_test_vs_ar("RF MARX", loss_ar, loss_rf_marx)

cat("\n--- Hybrid Models ---\n")
dm_hybrid_bench_vs_ar <- run_dm_test_vs_ar("Hybrid", loss_ar, loss_hybrid_bench)
dm_hybrid_maf_vs_ar   <- run_dm_test_vs_ar("Hybrid MAF", loss_ar, loss_hybrid_maf)
dm_hybrid_marx_vs_ar  <- run_dm_test_vs_ar("Hybrid MARX", loss_ar, loss_hybrid_marx)

# STEP 2B: Run DM tests for all models vs ADL without PCA
cat("\n--- AR Model ---\n")
dm_ar_vs_adl <- run_dm_test_vs_adl("AR", loss_adl_bench, loss_ar)

cat("\n--- ADL PCA Models ---\n")
dm_adl_benchpca_vs_adl <- run_dm_test_vs_adl("ADL PCA", loss_adl_bench, loss_adl_benchpca)
dm_adl_mafpca_vs_adl   <- run_dm_test_vs_adl("ADL MAF PCA", loss_adl_bench, loss_adl_mafpca)
dm_adl_marxpca_vs_adl  <- run_dm_test_vs_adl("ADL MARX PCA", loss_adl_bench, loss_adl_marxpca)

cat("\n--- LASSO Models ---\n")
dm_lasso_bench_vs_adl <- run_dm_test_vs_adl("LASSO", loss_adl_bench, loss_lasso_bench)
dm_lasso_maf_vs_adl   <- run_dm_test_vs_adl("LASSO MAF", loss_adl_bench, loss_lasso_maf)
dm_lasso_marx_vs_adl  <- run_dm_test_vs_adl("LASSO MARX", loss_adl_bench, loss_lasso_marx)

cat("\n--- RF Models ---\n")
dm_rf_bench_vs_adl <- run_dm_test_vs_adl("RF", loss_adl_bench, loss_rf_bench)
dm_rf_maf_vs_adl   <- run_dm_test_vs_adl("RF MAF", loss_adl_bench, loss_rf_maf)
dm_rf_marx_vs_adl  <- run_dm_test_vs_adl("RF MARX", loss_adl_bench, loss_rf_marx)

cat("\n--- Hybrid Models ---\n")
dm_hybrid_bench_vs_adl <- run_dm_test_vs_adl("Hybrid", loss_adl_bench, loss_hybrid_bench)
dm_hybrid_maf_vs_adl   <- run_dm_test_vs_adl("Hybrid MAF", loss_adl_bench, loss_hybrid_maf)
dm_hybrid_marx_vs_adl  <- run_dm_test_vs_adl("Hybrid MARX", loss_adl_bench, loss_hybrid_marx)


# STEP 3A: Collect results into 4×13 matrix for AR benchmark
dm_results_vs_ar <- matrix(NA, nrow = 4, ncol = 13)
colnames(dm_results_vs_ar) <- c("ADL", "ADL_PCA", "ADL_MAF_PCA", "ADL_MARX_PCA",
                                "LASSO", "LASSO_MAF", "LASSO_MARX",
                                "RF", "RF_MAF", "RF_MARX", 
                                "Hybrid", "Hybrid_MAF", "Hybrid_MARX")
rownames(dm_results_vs_ar) <- c("1-step", "3-step", "6-step", "12-step")

dm_results_vs_ar[, 1]  <- dm_adl_bench_vs_ar$dm_stats
dm_results_vs_ar[, 2]  <- dm_adl_benchpca_vs_ar$dm_stats
dm_results_vs_ar[, 3]  <- dm_adl_mafpca_vs_ar$dm_stats
dm_results_vs_ar[, 4]  <- dm_adl_marxpca_vs_ar$dm_stats
dm_results_vs_ar[, 5]  <- dm_lasso_bench_vs_ar$dm_stats
dm_results_vs_ar[, 6]  <- dm_lasso_maf_vs_ar$dm_stats
dm_results_vs_ar[, 7]  <- dm_lasso_marx_vs_ar$dm_stats
dm_results_vs_ar[, 8]  <- dm_rf_bench_vs_ar$dm_stats
dm_results_vs_ar[, 9]  <- dm_rf_maf_vs_ar$dm_stats
dm_results_vs_ar[, 10] <- dm_rf_marx_vs_ar$dm_stats
dm_results_vs_ar[, 11] <- dm_hybrid_bench_vs_ar$dm_stats
dm_results_vs_ar[, 12] <- dm_hybrid_maf_vs_ar$dm_stats
dm_results_vs_ar[, 13] <- dm_hybrid_marx_vs_ar$dm_stats

# STEP 3B: Collect results into 4×13 matrix for ADL benchmark
dm_results_vs_adl <- matrix(NA, nrow = 4, ncol = 13)
colnames(dm_results_vs_adl) <- c("AR", "ADL_PCA", "ADL_MAF_PCA", "ADL_MARX_PCA",
                                 "LASSO", "LASSO_MAF", "LASSO_MARX",
                                 "RF", "RF_MAF", "RF_MARX", 
                                 "Hybrid", "Hybrid_MAF", "Hybrid_MARX")
rownames(dm_results_vs_adl) <- c("1-step", "3-step", "6-step", "12-step")

dm_results_vs_adl[, 1]  <- dm_ar_vs_adl$dm_stats
dm_results_vs_adl[, 2]  <- dm_adl_benchpca_vs_adl$dm_stats
dm_results_vs_adl[, 3]  <- dm_adl_mafpca_vs_adl$dm_stats
dm_results_vs_adl[, 4]  <- dm_adl_marxpca_vs_adl$dm_stats
dm_results_vs_adl[, 5]  <- dm_lasso_bench_vs_adl$dm_stats
dm_results_vs_adl[, 6]  <- dm_lasso_maf_vs_adl$dm_stats
dm_results_vs_adl[, 7]  <- dm_lasso_marx_vs_adl$dm_stats
dm_results_vs_adl[, 8]  <- dm_rf_bench_vs_adl$dm_stats
dm_results_vs_adl[, 9]  <- dm_rf_maf_vs_adl$dm_stats
dm_results_vs_adl[, 10] <- dm_rf_marx_vs_adl$dm_stats
dm_results_vs_adl[, 11] <- dm_hybrid_bench_vs_adl$dm_stats
dm_results_vs_adl[, 12] <- dm_hybrid_maf_vs_adl$dm_stats
dm_results_vs_adl[, 13] <- dm_hybrid_marx_vs_adl$dm_stats

# STEP 4: Create RMSE table
# Initialize RMSE table with AR and all 13 comparison models
rmse_table <- matrix(NA, nrow = 4, ncol = 14)
colnames(rmse_table) <- c("AR", "ADL", "ADL_PCA", "ADL_MAF_PCA", "ADL_MARX_PCA",
                          "LASSO", "LASSO_MAF", "LASSO_MARX",
                          "RF", "RF_MAF", "RF_MARX", 
                          "Hybrid", "Hybrid_MAF", "Hybrid_MARX")
rownames(rmse_table) <- c("1-step", "3-step", "6-step", "12-step")

# Fill in RMSE values using correct syntax [["errors"]][["rmse"]]
rmse_table[1, "AR"]           <- bar1c[["errors"]][["rmse"]]
rmse_table[2, "AR"]           <- bar3c[["errors"]][["rmse"]]
rmse_table[3, "AR"]           <- bar6c[["errors"]][["rmse"]]
rmse_table[4, "AR"]           <- bar12c[["errors"]][["rmse"]]

rmse_table[1, "ADL"]          <- adl_bench_1c[["errors"]][["rmse"]]
rmse_table[2, "ADL"]          <- adl_bench_3c[["errors"]][["rmse"]]
rmse_table[3, "ADL"]          <- adl_bench_6c[["errors"]][["rmse"]]
rmse_table[4, "ADL"]          <- adl_bench_12c[["errors"]][["rmse"]]

rmse_table[1, "ADL_PCA"]      <- adl_benchpca_1c[["errors"]][["rmse"]]
rmse_table[2, "ADL_PCA"]      <- adl_benchpca_3c[["errors"]][["rmse"]]
rmse_table[3, "ADL_PCA"]      <- adl_benchpca_6c[["errors"]][["rmse"]]
rmse_table[4, "ADL_PCA"]      <- adl_benchpca_12c[["errors"]][["rmse"]]

rmse_table[1, "ADL_MAF_PCA"]  <- adl_mafpca_1c[["errors"]][["rmse"]]
rmse_table[2, "ADL_MAF_PCA"]  <- adl_mafpca_3c[["errors"]][["rmse"]]
rmse_table[3, "ADL_MAF_PCA"]  <- adl_mafpca_6c[["errors"]][["rmse"]]
rmse_table[4, "ADL_MAF_PCA"]  <- adl_mafpca_12c[["errors"]][["rmse"]]

rmse_table[1, "ADL_MARX_PCA"] <- adl_marxpca_1c[["errors"]][["rmse"]]
rmse_table[2, "ADL_MARX_PCA"] <- adl_marxpca_3c[["errors"]][["rmse"]]
rmse_table[3, "ADL_MARX_PCA"] <- adl_marxpca_6c[["errors"]][["rmse"]]
rmse_table[4, "ADL_MARX_PCA"] <- adl_marxpca_12c[["errors"]][["rmse"]]

rmse_table[1, "LASSO"]        <- lasso1c[["errors"]][["rmse"]]
rmse_table[2, "LASSO"]        <- lasso3c[["errors"]][["rmse"]]
rmse_table[3, "LASSO"]        <- lasso6c[["errors"]][["rmse"]]
rmse_table[4, "LASSO"]        <- lasso12c[["errors"]][["rmse"]]

rmse_table[1, "LASSO_MAF"]    <- maf_lasso1c[["errors"]][["rmse"]]
rmse_table[2, "LASSO_MAF"]    <- maf_lasso3c[["errors"]][["rmse"]]
rmse_table[3, "LASSO_MAF"]    <- maf_lasso6c[["errors"]][["rmse"]]
rmse_table[4, "LASSO_MAF"]    <- maf_lasso12c[["errors"]][["rmse"]]

rmse_table[1, "LASSO_MARX"]   <- marx_lasso1c[["errors"]][["rmse"]]
rmse_table[2, "LASSO_MARX"]   <- marx_lasso3c[["errors"]][["rmse"]]
rmse_table[3, "LASSO_MARX"]   <- marx_lasso6c[["errors"]][["rmse"]]
rmse_table[4, "LASSO_MARX"]   <- marx_lasso12c[["errors"]][["rmse"]]

rmse_table[1, "RF"]           <- rf12c[["errors"]][["rmse"]]
rmse_table[2, "RF"]           <- rf32c[["errors"]][["rmse"]]
rmse_table[3, "RF"]           <- rf62c[["errors"]][["rmse"]]
rmse_table[4, "RF"]           <- rf122c[["errors"]][["rmse"]]

rmse_table[1, "RF_MAF"]       <- maf_rf1[["errors"]][["rmse"]]
rmse_table[2, "RF_MAF"]       <- maf_rf3[["errors"]][["rmse"]]
rmse_table[3, "RF_MAF"]       <- maf_rf6[["errors"]][["rmse"]]
rmse_table[4, "RF_MAF"]       <- maf_rf12[["errors"]][["rmse"]]

rmse_table[1, "RF_MARX"]      <- marx_rf1[["errors"]][["rmse"]]
rmse_table[2, "RF_MARX"]      <- marx_rf3[["errors"]][["rmse"]]
rmse_table[3, "RF_MARX"]      <- marx_rf6[["errors"]][["rmse"]]
rmse_table[4, "RF_MARX"]      <- marx_rf12[["errors"]][["rmse"]]

rmse_table[1, "Hybrid"]       <- hybrid_bench_1c[["errors"]][["rmse"]]
rmse_table[2, "Hybrid"]       <- hybrid_bench_3c[["errors"]][["rmse"]]
rmse_table[3, "Hybrid"]       <- hybrid_bench_6c[["errors"]][["rmse"]]
rmse_table[4, "Hybrid"]       <- hybrid_bench_12c[["errors"]][["rmse"]]

rmse_table[1, "Hybrid_MAF"]   <- hybrid_maf_1c[["errors"]][["rmse"]]
rmse_table[2, "Hybrid_MAF"]   <- hybrid_maf_3c[["errors"]][["rmse"]]
rmse_table[3, "Hybrid_MAF"]   <- hybrid_maf_6c[["errors"]][["rmse"]]
rmse_table[4, "Hybrid_MAF"]   <- hybrid_maf_12c[["errors"]][["rmse"]]

rmse_table[1, "Hybrid_MARX"]  <- hybrid_marx_1c[["errors"]][["rmse"]]
rmse_table[2, "Hybrid_MARX"]  <- hybrid_marx_3c[["errors"]][["rmse"]]
rmse_table[3, "Hybrid_MARX"]  <- hybrid_marx_6c[["errors"]][["rmse"]]
rmse_table[4, "Hybrid_MARX"]  <- hybrid_marx_12c[["errors"]][["rmse"]]

# STEP 5: Display Results
cat("   RMSE TABLE (All Models)\n")
print(round(rmse_table, 3))

cat("   DM TEST RESULTS vs AR BENCHMARK\n")
print(round(dm_results_vs_ar, 3))

cat("   DM TEST RESULTS vs ADL BENCHMARK\n")
print(round(dm_results_vs_adl, 3))

# STEP 6: Identify best models by horizon and compute averages

# Best model by horizon (vs AR) - highest DM statistic
cat("   BEST MODEL BY HORIZON (vs AR Benchmark)\n")
for (i in 1:4) {
  best_idx <- which.max(dm_results_vs_ar[i, ])
  best_model <- colnames(dm_results_vs_ar)[best_idx]
  best_dm <- dm_results_vs_ar[i, best_idx]
  cat(sprintf("%s: %s (DM = %.3f)\n", 
              rownames(dm_results_vs_ar)[i], best_model, best_dm))
}

# Best model by horizon (vs ADL) - highest DM statistic
cat("   BEST MODEL BY HORIZON (vs ADL Benchmark)\n")
for (i in 1:4) {
  best_idx <- which.max(dm_results_vs_adl[i, ])
  best_model <- colnames(dm_results_vs_adl)[best_idx]
  best_dm <- dm_results_vs_adl[i, best_idx]
  cat(sprintf("%s: %s (DM = %.3f)\n", 
              rownames(dm_results_vs_adl)[i], best_model, best_dm))
}
# Add average DM statistic row for vs AR
dm_avg_vs_ar <- colMeans(dm_results_vs_ar)
dm_with_avg_vs_ar <- rbind(dm_results_vs_ar, Average = dm_avg_vs_ar)

# Add average DM statistic row for vs ADL
dm_avg_vs_adl <- colMeans(dm_results_vs_adl)
dm_with_avg_vs_adl <- rbind(dm_results_vs_adl, Average = dm_avg_vs_adl)

# Best model overall by average DM (vs AR)
cat("   BEST MODEL BY AVERAGE DM STATISTIC (vs AR)\n")
best_overall_idx_ar <- which.max(dm_avg_vs_ar)
best_overall_model_ar <- names(dm_avg_vs_ar)[best_overall_idx_ar]
best_overall_dm_ar <- dm_avg_vs_ar[best_overall_idx_ar]
cat(sprintf("Best Model: %s (Average DM = %.3f)\n", 
            best_overall_model_ar, best_overall_dm_ar))


# Best model overall by average DM (vs ADL)
cat("   BEST MODEL BY AVERAGE DM STATISTIC (vs ADL)\n")
best_overall_idx_adl <- which.max(dm_avg_vs_adl)
best_overall_model_adl <- names(dm_avg_vs_adl)[best_overall_idx_adl]
best_overall_dm_adl <- dm_avg_vs_adl[best_overall_idx_adl]
cat(sprintf("Best Model: %s (Average DM = %.3f)\n", 
            best_overall_model_adl, best_overall_dm_adl))

## Add this after your STEP 5: Display Results section

# STEP 5.5: Add critical value comparison at 5% significance level
# Critical value for two-tailed test at 5% significance level
critical_value <- 1.96

cat("DM TEST RESULTS vs AR BENCHMARK\n")
cat("Critical value (5% level): ±1.96\n")
print(round(dm_results_vs_ar, 3))

cat("\nSignificant at 5% level (|DM| > 1.96):\n")
sig_matrix_ar <- ifelse(abs(dm_results_vs_ar) > critical_value, "YES", "NO")
print(sig_matrix_ar)

cat("DM TEST RESULTS vs ADL BENCHMARK\n")
cat("Critical value (5% level): ±1.96\n")
print(round(dm_results_vs_adl, 3))

cat("\nSignificant at 5% level (|DM| > 1.96):\n")
sig_matrix_adl <- ifelse(abs(dm_results_vs_adl) > critical_value, "YES", "NO")
print(sig_matrix_adl)

# Summary count
cat("\n========================================\n")
cat("SUMMARY: Significant Results at 5% Level\n")
cat("========================================\n")
cat(sprintf("vs AR:  %d out of %d tests are significant\n", 
            sum(abs(dm_results_vs_ar) > critical_value),
            length(dm_results_vs_ar)))

cat(sprintf("vs ADL: %d out of %d tests are significant\n", 
            sum(abs(dm_results_vs_adl) > critical_value),
            length(dm_results_vs_adl)))


cat("\n--- Vanilla Models Comparison ---\n")
dm_lasso_bench_vs_rf_bench <- run_dm_test("LASSO", "RF", loss_lasso_bench, loss_rf_bench)
dm_lasso_bench_vs_hybrid_bench   <- run_dm_test("LASSO", "HYBRID",loss_lasso_bench, loss_hybrid_bench)
dm_rf_bench_vs_hybrid_bench  <- run_dm_test("RF", "HYBRID", loss_rf_bench, loss_hybrid_bench)

cat("\n--- Variable Transformations Comparison ---\n")
dm_lasso_bench_vs_lasso_maf <- run_dm_test("LASSO", "LASSO MAF", loss_lasso_bench, loss_lasso_maf)
dm_lasso_bench_vs_lasso_marx <- run_dm_test("LASSO", "LASSO MARX", loss_lasso_bench, loss_lasso_marx)

dm_rf_bench_vs_rf_maf  <- run_dm_test("RF", "RF MAF",loss_rf_bench, loss_rf_maf)
dm_rf_bench_vs_rf_marx <- run_dm_test("RF", "RF MARX", loss_rf_bench, loss_rf_marx)

dm_hybrid_bench_vs_hybrid_maf  <- run_dm_test("HYBRID", "HYBRID MAF",loss_hybrid_bench, loss_hybrid_maf)
dm_hybrid_bench_vs_hybrid_marx <- run_dm_test("HYBRID", "HYBRID MARX", loss_hybrid_bench, loss_hybrid_marx)