#setwd()
load("data/fredmd_cleaned.RData") 
source("data_transformation/marx_transform.R") 

library(glmnet)
library(HDeconometrics)
library(sandwich) #library to estimate variance for DM test regression using NeweyWest()
library(hdm)

Y = md
nprev = 120 #test size

########################################
#LASSO-MARX AND ROLLING WINDOW FUNCTION
########################################
run_marxlasso <- function(Y, h, target_name = 'UNRATE',
                          L_y = 4, P_marx = 4,
                          alpha = 1, IC = "bic") {
  
  # 0) Drop date; split into train (1..T-1) and last row T for prediction
  Y <- subset(Y, select = -date)
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # Identify target & dummy (dummy = last col)
  idx_y   <- which(colnames(Y_in) == target_name)
  dum_idx <- which(colnames(Y_in) == "aft_break") # dummy var index
  if (length(idx_y) != 1) stop("target_name not found in Y.")
  
  # 1) MARX on TRAIN X only (exclude target & dummy)
  X_train_raw <- as.matrix(Y_in[, setdiff(seq_len(ncol(Y_in)), c(idx_y, idx_dum)), drop = FALSE])
  mx <- marx_transform(X_train_raw, n_lag = P_marx, scale_data = FALSE)
  X_marx <- mx$mat_x_marx   # should have T_in - P_marx rows if coded à la Coulombe
  
  # 2) Horizon-consistent alignment
  y_in <- as.numeric(Y_in[, idx_y, drop = TRUE])
  T_in <- nrow(Y_in)
  
  # Valid times t (need P_marx and L_y available; and target at t+h must exist)
  t_start <- max(P_marx + 1, L_y + 1)
  t_end   <- T_in - h
  if (t_end < t_start) stop("Window too short for chosen h/L_y/P_marx.")
  t_idx <- t_start:t_end
  
  # Map to MARX rows (Coulombe-style trimming: X_marx row for time t is t - P_marx)
  marx_rows <- t_idx - P_marx
  
  # y lags aligned with time t
  if (L_y > 0) {
    y_embed <- embed(y_in, L_y + 1)              # [y_t, y_{t-1}, ..., y_{t-L_y}]
    y_lags  <- y_embed[, -1, drop = FALSE]
    y_rows  <- t_idx - L_y
    y_lags_aligned <- y_lags[y_rows, , drop = FALSE]
    colnames(y_lags_aligned) <- paste0("y_L", 1:L_y)
  } else {
    y_lags_aligned <- NULL
  }
  
  # contemporaneous dummy and target
  dum_t    <- as.numeric(Y_in[t_idx, idx_dum, drop = TRUE])
  y_target <- y_in[t_idx + h]
  
  # 3) Assemble training design (no NA imputation)
  X_train_df <- cbind(
    if (!is.null(y_lags_aligned)) as.data.frame(y_lags_aligned) else NULL,
    as.data.frame(X_marx[marx_rows, , drop = FALSE], check.names = FALSE),
    DUM = dum_t
  )
  
  # ---- Coulombe-style row TRIM: drop any row with NA in X or y ----
  non_dummy_cols <- setdiff(colnames(X_train_df), "DUM")
  X_all_mat <- as.matrix(X_train_df[, c(non_dummy_cols, "DUM"), drop = FALSE])
  good_rows <- complete.cases(X_all_mat) & is.finite(y_target)
  
  if (!any(good_rows)) stop("No complete rows after MARX/lag alignment.")
  X_train_df <- X_train_df[good_rows, , drop = FALSE]
  y_target   <- y_target[good_rows]
  
  # 4) Build X_new at t = T_in (must be fully observed; if not, stop)
  if ((T_in - P_marx) < 1 || (T_in - P_marx) > nrow(X_marx)) {
    stop("Cannot form X_new: window too short relative to P_marx.")
  }
  X_new_marx <- X_marx[T_in - P_marx, , drop = FALSE]
  if (L_y > 0) {
    y_lags_new <- rev(y_in[(T_in - L_y):(T_in - 1)])
    names(y_lags_new) <- paste0("y_L", 1:L_y)
  } else {
    y_lags_new <- NULL
  }
  DUM_new <- as.numeric(Y_out[, idx_dum, drop = TRUE])
  
  X_new_df <- as.data.frame(cbind(
    if (!is.null(y_lags_new)) t(y_lags_new) else NULL,
    X_new_marx,
    DUM = DUM_new
  ), check.names = FALSE)
  
  # If the last-row features have NA (rare if upstream data are clean), stop—don’t impute
  if (any(!complete.cases(X_new_df))) {
    stop("X_new has NA after MARX/lag alignment—per Coulombe, do not impute; fix inputs or reduce P_marx/L_y.")
  }
  
  # 5) Standardize NON-dummy features only (trees don’t need this; LASSO does)
  dum_name <- "DUM"
  non_dummy_cols <- setdiff(colnames(X_train_df), dum_name)
  
  # Ensure numeric matrices
  X_non_train <- as.matrix(X_train_df[, non_dummy_cols, drop = FALSE])
  X_non_new   <- as.matrix(X_new_df[,   non_dummy_cols, drop = FALSE])
  
  # Means/SDs (no NA left by construction)
  means <- colMeans(X_non_train)
  sds   <- apply(X_non_train, 2, sd)
  # guard zero-variance
  sds[!is.finite(sds) | sds == 0] <- 1
  
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
  pred <- as.numeric(predict(fit, newx = as.matrix(newx)))
  
  # Sparse coefs as named vector
  cf <- as.numeric(fit$coef); names(cf) <- rownames(fit$coef)
  
  list(model = fit, pred = pred, coef = cf, X_new = X_new_df)
}



marx_lasso.rolling.window <- function(Y, nprev, h = 1,
                                      target_name = "UNRATE",
                                      L_y = 4, P_marx = 4,
                                      alpha = 1, IC = "bic",
                                      verbose = TRUE) {
  save.pred <- rep(NA_real_, nprev)
  save.coef <- vector("list", nprev)
  
  target_idx <- which(colnames(Y) == target_name)
  if (length(target_idx) != 1) stop("target_name not found in Y.")
  
  for (i in nprev:max(h, 1)) {
    # rolling window
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    
    fit <- run_marxlasso(
      Y            = Y.window,
      h            = h,
      target_name  = target_name,
      L_y          = L_y,
      P_marx       = P_marx,
      alpha        = alpha,
      IC           = IC
    )
    
    # book-keeping indices: match your RF code
    t   <- nrow(Y) - i      # last in-sample index
    u   <- t + h            # forecasted index
    pos <- u - (nrow(Y) - nprev)
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos] <- as.numeric(fit$pred)
      save.coef[[pos]] <- fit$coef
    }
    
    if (verbose) cat("iteration", pos, "\n")
  }
  
  # OOS errors against last nprev points
  real <- Y[, which(colnames(Y) == target_name)]
  y_test_full <- tail(real, nprev)
  pred_full   <- save.pred
  
  valid <- !is.na(pred_full)
  y_test <- y_test_full[valid]
  pred   <- pred_full[valid]
  
  rmse <- sqrt(mean((y_test - pred)^2))
  mae  <- mean(abs(y_test - pred))
  errors <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
  
  list(
    pred       = save.pred,
    errors     = errors,
    save.coef  = save.coef
  )
}

############################################################################
#Penalized regression: LASSO-MAF forecasts (BIC, AIC, AICc)
############################################################################

alpha=1 #set alpha=1 for LASSO

#Run forecasts for MARX-LASSO (BIC)
marx_lasso1c=marx_lasso.rolling.window(Y,nprev,h=1,target_name = "UNRATE",
                                  L_y = 4, P_marx = 4, alpha,IC="bic")
marx_lasso3c=marx_lasso.rolling.window(Y,nprev,h=3,target_name = "UNRATE",
                                       L_y = 4, P_marx = 4, alpha,IC="bic")
marx_lasso6c=marx_lasso.rolling.window(Y,nprev,h=6,target_name = "UNRATE",
                                       L_y = 4, P_marx = 4, alpha,IC="bic")
marx_lasso12c=marx_lasso.rolling.window(Y,nprev,h=12,target_name = "UNRATE",
                                       L_y = 4, P_marx = 4, alpha,IC="bic")



