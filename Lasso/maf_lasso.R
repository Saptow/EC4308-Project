#setwd()
load("data/fredmd_cleaned.RData") 
source("data_transformation/maf_transform.R") 

library(glmnet)
library(HDeconometrics)
library(sandwich) #library to estimate variance for DM test regression using NeweyWest()
library(hdm)

Y = md
nprev = 120 #test size

########################################
#LASSO-MAF AND ROLLING WINDOW FUNCTION
########################################

run_maflasso <- function(Y, h = 1, target_name = "UNRATE",
                         L_y = 4, P_maf = 4,
                         alpha = 1, IC = "bic") {
  
  # Drop date; split train (1..T-1) and last row T for prediction
  Y <- subset(Y, select = -date)
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # Identify target & dummy (dummy = last column)
  idx_y   <- which(colnames(Y_in) == target_name)
  dum_idx <- which(colnames(Y_in) == "aft_break") # dummy var index
  if (length(idx_y) != 1) stop("target_name not found in Y.")
  
  # 1) MAF on TRAIN X only (exclude target & dummy)
  X_train_raw <- as.matrix(Y_in[, setdiff(seq_len(ncol(Y_in)), c(idx_y, dum_idx)), drop = FALSE])
  maf_train <- maf_transform(X_train_raw, P_maf = P_maf, scale_data = TRUE)  # expect (T_in - P_maf) rows
  
  # 2) Horizon-consistent alignment
  y_in <- as.numeric(Y_in[, idx_y, drop = TRUE])
  T_in <- nrow(Y_in)
  
  t_start <- max(P_maf + 1, L_y + 1)
  t_end   <- T_in - h
  if (t_end < t_start) stop("Window too short for chosen h/L_y/P_maf.")
  t_idx <- t_start:t_end
  
  maf_rows <- t_idx - P_maf
  
  if (L_y > 0) {
    y_embed <- embed(y_in, L_y + 1)              # [y_t, y_{t-1}, ..., y_{t-L_y}]
    y_lags  <- y_embed[, -1, drop = FALSE]
    y_rows  <- t_idx - L_y
    y_lags_aligned <- y_lags[y_rows, , drop = FALSE]
    colnames(y_lags_aligned) <- paste0("y_L", 1:L_y)
  } else {
    y_lags_aligned <- NULL
  }
  
  dum_t    <- as.numeric(Y_in[t_idx, dum_idx, drop = TRUE])  # contemporaneous dummy
  y_target <- y_in[t_idx + h]                                # target y_{t+h}
  
  # 3) Assemble training design (no NA imputation; we will drop incomplete rows)
  X_train_df <- cbind(
    if (!is.null(y_lags_aligned)) as.data.frame(y_lags_aligned) else NULL,
    as.data.frame(maf_train[maf_rows, , drop = FALSE], check.names = FALSE),
    DUM = dum_t
  )
  
  # ---- Coulombe-style row TRIM: drop any row with NA in X or y ----
  non_dummy_cols <- setdiff(colnames(X_train_df), "DUM")
  X_all_mat <- as.matrix(X_train_df[, c(non_dummy_cols, "DUM"), drop = FALSE])
  good_rows <- complete.cases(X_all_mat) & is.finite(y_target)
  if (!any(good_rows)) stop("No complete rows after MAF/lag alignment.")
  X_train_df <- X_train_df[good_rows, , drop = FALSE]
  y_target   <- y_target[good_rows]
  
  # 4) Build X_new at t = T_in (must be fully observed; if not, stop)
  if ((T_in - P_maf) < 1 || (T_in - P_maf) > nrow(maf_train)) {
    stop("Cannot form X_new: window too short relative to P_maf.")
  }
  X_new_maf <- maf_train[T_in - P_maf, , drop = FALSE]
  if (L_y > 0) {
    y_lags_new <- rev(y_in[(T_in - L_y):(T_in - 1)])
    names(y_lags_new) <- paste0("y_L", 1:L_y)
  } else {
    y_lags_new <- NULL
  }
  DUM_new <- as.numeric(Y_out[, dum_idx, drop = TRUE])
  
  X_new_df <- as.data.frame(cbind(
    if (!is.null(y_lags_new)) t(y_lags_new) else NULL,
    X_new_maf,
    DUM = DUM_new
  ), check.names = FALSE)
  
  if (any(!complete.cases(X_new_df))) {
    stop("X_new has NA after MAF/lag alignment—per Coulombe, do not impute; fix inputs or reduce P_maf/L_y.")
  }
  
  # 5) Standardize NON-dummy features  
  dum_name <- colnames(Y_in)[dum_idx]
  non_dummy_cols <- setdiff(colnames(X_train_df), dum_name)
  
  X_non_train <- as.matrix(X_train_df[, non_dummy_cols, drop = FALSE])
  X_non_new   <- as.matrix(X_new_df[,   non_dummy_cols, drop = FALSE])
  
  means <- colMeans(X_non_train)
  sds   <- apply(X_non_train, 2, sd)
  sds[!is.finite(sds) | sds == 0] <- 1  # guard zero-variance
  
  X_train_mat <- cbind(
    sweep(sweep(X_non_train, 2, means, "-"), 2, sds, "/"),
    DUM = as.numeric(X_train_df[[dum_name]])
  )
  colnames(X_train_mat)[ncol(X_train_mat)] <- dum_name
  
  newx <- cbind(
    sweep(sweep(X_non_new, 2, means, "-"), 2, sds, "/"),
    DUM = as.numeric(X_new_df[[dum_name]])
  )
  colnames(newx)[ncol(newx)] <- dum_name
  
  # Drop all-constant columns (after scaling they’re zeros)
  keep <- which(colSums(abs(X_train_mat)) > 0)
  X_train_mat <- X_train_mat[, keep, drop = FALSE]
  newx        <- newx[,        keep, drop = FALSE]
  
  # 6) Fit LASSO by information criterion & predict
  fit <- HDeconometrics::ic.glmnet(
    x     = as.matrix(X_train_mat),
    y     = y_target,
    crit  = IC,
    alpha = alpha
  )
  pred_raw <- predict(fit, newx = as.matrix(newx), s = fit$lambda)
  pred <- drop(pred_raw)
  if (length(pred) != 1L) pred <- pred[1L]
  
  # Sparse coefs as named vector
  cf <- as.numeric(fit$coef); names(cf) <- rownames(fit$coef)
  
  list(model = fit, pred = pred, coef = cf, X_new = X_new_df)
}


maf_lasso.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE",
                                     L_y = 4, P_maf = 4,
                                     alpha = 1, IC = "bic",
                                     verbose = TRUE) {
  
  save.pred <- matrix(NA_real_, nprev, 1)
  save.coef <- vector("list", nprev)
  
  target_idx <- which(colnames(Y) == target_name)
  if (length(target_idx) != 1) stop("target_name not found in Y.")
  
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    
    fit <- run_maflasso(
      Y = Y.window, h = h, target_name = target_name,
      L_y = L_y, P_maf = P_maf, alpha = alpha, IC = IC
    )
    
    t  <- nrow(Y) - i
    u  <- t + h
    pos <- u - (nrow(Y) - nprev)
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos, 1] <- as.numeric(fit$pred)
      save.coef[[pos]]  <- fit$coef
    }
    
    if (verbose) cat("iteration", pos, "\n")
  }
  
  # OOS errors over the last nprev points (keep only stored forecasts)
  real <- Y[, target_idx]
  y_test_full <- tail(real, nprev)
  pred_full   <- save.pred[, 1]
  
  valid <- !is.na(pred_full)
  y_test <- y_test_full[valid]
  pred   <- pred_full[valid]
  
  rmse <- sqrt(mean((y_test - pred)^2))
  mae  <- mean(abs(y_test - pred))
  errors <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
  
  list(
    pred = save.pred,
    errors = errors,
    save.coef = save.coef
  )
}

############################################################################
#Penalized regression: LASSO-MAF forecasts (BIC, AIC, AICc)
##########################################################################

alpha=1 #set alpha=1 for LASSO

#Run forecasts for MAF-LASSO (BIC)
maf_lasso1c=maf_lasso.rolling.window(Y,nprev,h=1,target_name = "UNRATE",
                                       L_y = 4, P_maf = 4, alpha,IC="bic")
maf_lasso3c=maf_lasso.rolling.window(Y,nprev,h=3,target_name = "UNRATE",
                                     L_y = 4, P_maf = 4, alpha,IC="bic")
maf_lasso6c=maf_lasso.rolling.window(Y,nprev,h=6,target_name = "UNRATE",
                                     L_y = 4, P_maf = 4, alpha,IC="bic")
maf_lasso12c=maf_lasso.rolling.window(Y,nprev,h=12,target_name = "UNRATE",
                                     L_y = 4, P_maf = 4, alpha,IC="bic")

