# Moving Average Rotation of X Random Forest

# We apply MARX transformation on the X variables proposed by Coulombe (2021)
# Design matrix is lags of y, MARX lags of X. To keep model consistency across all models, we choose lags of y and lags of x be 4. 

run_marxrf = function(Y, h, target_name = 'UNRATE') {
  L_y  = 4 # lags of y to keep
  P_marx = 4 # lags of marx
  
  # drop date and leave last row for prediction
  Y <- Y[, -1, drop = FALSE] # drop date column
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # set target and dummy indices
  indice  <- which(colnames(Y_in) == target_name)
  dum_idx <- ncol(Y_in)
  
  # Apply MARX on train X only
  X_train_raw <- as.matrix(Y_in[, setdiff(seq_len(ncol(Y_in)), c(indice, dum_idx)), drop = FALSE])
  mx <- marx_transform(X_train_raw, n_lag = P_marx, scale_data = FALSE)
  X_marx <- mx$mat_x_marx 
  
  # Align features/target for h-step learning
  y_in <- as.numeric(Y_in[, indice, drop = TRUE])
  T_in <- nrow(Y_in)
  # validate time indices
  t_start <- max(P_marx + 1, L_y + 1)
  t_end   <- T_in - h
  if (t_end < t_start) stop("Window too short for chosen h/L_y/P_marx.")
  t_idx <- t_start:t_end
  
  # Map to matrix rows
  marx_rows <- t_idx - P_marx       
  if (L_y > 0) {
    y_embed <- embed(y_in, L_y + 1)
    y_lags  <- y_embed[, -1, drop = FALSE]       
    y_rows  <- t_idx - L_y
    y_lags_aligned <- y_lags[y_rows, , drop = FALSE]
    colnames(y_lags_aligned) <- paste0("y_L", 1:L_y)
  } else {
    y_lags_aligned <- NULL
  }
  
  # Contemporaneous dummy at time t
  dum_t <- as.numeric(Y_in[t_idx, dum_idx, drop = TRUE])
  
  y_target <- y_in[t_idx + h]
  
  # Final training design matrix
  X_train <- cbind(
    if (!is.null(y_lags_aligned)) as.data.frame(y_lags_aligned) else NULL,
    as.data.frame(X_marx[marx_rows, , drop = FALSE], check.names = FALSE),
    DUM = dum_t
  )
  # Drop rows with any NA in X or y
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
  DUM_new <- as.numeric(Y_out[, dum_idx, drop = TRUE])
  X_new <- as.data.frame(cbind(
    if (!is.null(y_lags_new)) t(y_lags_new) else NULL,
    X_new_marx,
    DUM = DUM_new
  ), check.names = FALSE)
  
  # Fit RF and predict
  rf <- ranger(x = X_train, y = y_target, importance = "permutation", max.depth = 5, mtry = floor(ncol(X_train) / 3))
  pred <- predict(rf, X_new)
  
  list(model = rf, pred  = pred$predictions, importance = importance(rf),X_new = X_new)
  
}

# Rolling window 
marx_rf.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE", verbose = TRUE) {
  
  save.pred <- matrix(NA_real_, nprev, 1)
  save.importance <- vector("list", nprev)
  
  target_idx <- which(colnames(Y) == target_name)
  if (length(target_idx) != 1) stop("target_name not found in Y.")
  
  set.seed(123)
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    fit <- run_marxrf(Y.window, h = h, target_name = target_name)
    
    t  <- nrow(Y) - i          
    u  <- t + h                
    pos <- u - (nrow(Y) - nprev)
    
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos, 1]    <- as.numeric(fit$pred)
      save.importance[[pos]] <- fit$importance
    }
    
    if (verbose) cat("iteration", pos, "\n")
  }
  
  # OOS errors 
  real <- Y[, which(colnames(Y) == target_name)]
  y_test_full <- tail(real, nprev)      
  pred_full   <- save.pred[, 1]
  
  # filter valid (non-NA) forecasts
  valid <- !is.na(pred_full)
  y_test <- y_test_full[valid]
  pred   <- pred_full[valid]
  rmse <- sqrt(mean((y_test - pred)^2))
  mae  <- mean(abs(y_test - pred))
  errors <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
  
  list(
    pred = save.pred,
    errors = c(rmse = rmse, mae = mae, n_effective = sum(valid)),
    save.importance = save.importance
  )
}

 
  

