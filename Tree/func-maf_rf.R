# Moving Average Factor Random Forest
# We apply MAF transformation on the X variables proposed by Coulombe (2021)
# Design matrix is lags of y, PCs for each variable using 4 lags 

run_mafrf <- function(Y, h = 1, target_name = "UNRATE") {
  L_y   = 4   # number of lags of y to keep
  P_maf = 4  # number of lags for each X in MAF
  
  
  # Drop date; split into train (1..T-1) and last row T for prediction
  Y <- Y[, -1, drop = FALSE]  # drop date column
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # Identify target & dummy (dummy = last column)
  indice  <- which(colnames(Y_in) == target_name)
  dum_idx <- ncol(Y_in)
  
  # Apply MAF transformation on TRAIN X only
  X_train_raw <- as.matrix(Y_in[, setdiff(seq_len(ncol(Y_in)), c(indice, dum_idx)), drop = FALSE])
  maf_train <- maf_transform(X_train_raw, P_maf = P_maf, scale_data = TRUE)
  
  # Align features/target for h-step learning
  y_in <- as.numeric(Y_in[, indice, drop = TRUE])
  T_in <- nrow(Y_in)
  t_start <- max(P_maf + 1, L_y + 1)
  t_end   <- T_in - h
  if (t_end < t_start) stop("Window too short for chosen h/L_y/P_maf.")
  t_idx <- t_start:t_end
  
  maf_rows <- t_idx - P_maf
  if (L_y > 0) {
    y_embed <- embed(y_in, L_y + 1)
    y_lags  <- y_embed[, -1, drop = FALSE]  
    y_rows  <- t_idx - L_y
    y_lags_aligned <- y_lags[y_rows, , drop = FALSE]
    colnames(y_lags_aligned) <- paste0("y_L", 1:L_y)
  } else {
    y_lags_aligned <- NULL
  }
  
  # Dummy at t
  dum_t <- as.numeric(Y_in[t_idx, dum_idx, drop = TRUE])
  
  # Target variable 
  y_target <- y_in[t_idx + h]
  
  # Final design matrix for training
  X_train <- cbind(
    if (!is.null(y_lags_aligned)) as.data.frame(y_lags_aligned) else NULL,
    as.data.frame(maf_train[maf_rows, , drop = FALSE], check.names = FALSE),
    DUM = dum_t
  )
  
  # Build X_new for forecasting y_{T_in + h}
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
  
  X_new <- as.data.frame(cbind(
    if (!is.null(y_lags_new)) t(y_lags_new) else NULL,
    X_new_maf,
    DUM = DUM_new
  ), check.names = FALSE)
  
  # Fit Random Forest and predict
  rf <- ranger(x = X_train, y = y_target, importance = "permutation", max.depth = 5, mtry = floor(ncol(X_train) / 3))
  pred <- predict(rf, X_new)
  
  list(model = rf, pred = pred$predictions, importance = importance(rf), X_new = X_new)
}


maf_rf.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE", verbose = TRUE) {
  
  save.pred <- matrix(NA_real_, nprev, 1)
  save.importance <- vector("list", nprev)
  
  target_idx <- which(colnames(Y) == target_name)
  if (length(target_idx) != 1) stop("target_name not found in Y.")
  
  set.seed(123)
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    fit <- run_mafrf(Y.window, h = h, target_name = target_name)
    
    t  <- nrow(Y) - i          
    u  <- t + h                
    pos <- u - (nrow(Y) - nprev)
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos, 1] <- as.numeric(fit$pred)
      save.importance[[pos]] <- fit$importance 
    }
      
    if (verbose) cat("iteration", pos, "\n")
  }
  
  real <- Y[, target_idx]
  y_test_full <- tail(real, nprev)
  pred_full   <- save.pred[, 1]
  
  idx <- h:nprev  
  valid <- !is.na(pred_full)
  y_test <- y_test_full[idx]
  pred   <- pred_full[idx]
  
  rmse <- sqrt(mean((y_test - pred)^2))
  mae  <- mean(abs(y_test - pred))
  
  list(
    pred = save.pred,
    errors = c(rmse = rmse, mae = mae, n_effective = sum(valid)),
    save.importance = save.importance
  )
}
  

