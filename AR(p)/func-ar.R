# ============================================
# AR(p) direct h-step with contemporaneous dummy
# ============================================

runAR <- function(Y, h = 1, target_name = "UNRATE", type = "fixed") {
  L_max <- 4  # max lag of y
  
  # 0) Drop date; split into train (1..T-1) and last row T (forecast origin)
  Y     <- Y[, -1, drop = FALSE]
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # Identify target and dummy at last column
  idx_y   <- which(colnames(Y_in) == target_name)
  idx_dum <- ncol(Y_in)
  
  # Pull series as numeric vectors
  y_train <- as.numeric(Y_in[, idx_y])
  d_train <- as.numeric(Y_in[, idx_dum])  # contemporaneous dummy
  d_new   <- as.numeric(Y_out[, idx_dum]) # dummy at forecast origin t
  
  # 1) Build lag panel for y 
  k_max <- h + L_max + 1
  if (length(y_train) < k_max) stop("Window too short for h + L_max + 1 lags.")
  aux <- embed(y_train, k_max)                    
  colnames(aux) <- paste0("y_L", 0:(k_max - 1))
  
  # Align dummy 
  d_aligned <- tail(d_train, nrow(aux))
  
  # Target is y_{t+h}
  y_vec <- aux[, paste0("y_L", h)]           
  
  # 2) Choose lag order p
  if (identical(type, "fixed")) {
    p <- L_max
  } else if (identical(type, "bic")) {
    best_bic <- Inf; best_p <- 1
    for (pp in 1:L_max) {
      lag_names <- paste0("y_L", (h + 1):(h + pp))
      X_pp <- as.data.frame(cbind(aux[, lag_names, drop = FALSE], DUM = d_aligned),
                            check.names = FALSE)
      df_pp <- data.frame(y = y_vec, X_pp, check.names = FALSE)
      m_pp  <- lm(y ~ ., data = df_pp)
      b_pp  <- BIC(m_pp)
      if (b_pp < best_bic) { best_bic <- b_pp; best_p <- pp }
    }
    p <- best_p
  } else stop('type must be "fixed" or "bic"')
  
  # 3) Final regressors: AR p lags of y + contemporaneous dummy
  lag_names <- paste0("y_L", (h + 1):(h + p))
  X_df <- as.data.frame(cbind(aux[, lag_names, drop = FALSE], DUM = d_aligned),
                        check.names = FALSE)
  # ensure proper column names (no recycling surprises)
  colnames(X_df) <- c(lag_names, "DUM")
  df <- data.frame(y = y_vec, X_df, check.names = FALSE)
  
  # Horizon-safe prediction row — build with a named list so dims never drop
  last_row_vals <- as.numeric(aux[nrow(aux), lag_names, drop = FALSE])
  names(last_row_vals) <- lag_names
  X_new <- as.data.frame(as.list(c(last_row_vals, DUM = d_new)), check.names = FALSE)
  # guarantee the same column order as X_df
  X_new <- X_new[, colnames(X_df), drop = FALSE]
  
  # 4) Fit and predict
  model <- lm(y ~ ., data = df)
  pred  <- as.numeric(predict(model, newdata = X_new))
  
  # Pad coefficients for consistent storage: 1 (intcpt) + 4 lags + 1 dummy = 6
  ar_coef  <- coef(model)
  coef_pad <- rep(NA_real_, 1 + L_max + 1)
  coef_pad[1:length(ar_coef)] <- ar_coef
  
  list(model = model, pred = pred, coef = coef_pad, p = p)
}


# =====================================================
# Rolling window (same mapping as your RF template)
# =====================================================

ar.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE", type = "fixed") {
  L_max <- 4
  save.coef <- matrix(NA_real_, nprev, 1 + L_max + 1)  # (Intercept) + up to 4 lags + DUM
  save.pred <- matrix(NA_real_, nprev, 1)
  save.p    <- integer(nprev)
  
  set.seed(12455)
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    fit <- runAR(Y.window, h = h, target_name = target_name, type = type)
    
    t   <- nrow(Y) - i          
    u   <- t + h                
    pos <- u - (nrow(Y) - nprev) 
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos, ] <- fit$pred
      save.coef[pos, ] <- fit$coef
      save.p[pos]      <- fit$p
    }
    cat("iteration", pos, "\n")
  }
  
  
  real       <- Y[, which(colnames(Y) == target_name)]
  y_test_all <- tail(real, nprev)
  pred_all   <- save.pred[, 1]
  valid      <- !is.na(pred_all)
  
  rmse <- sqrt(mean((y_test_all[valid] - pred_all[valid])^2))
  mae  <- mean(abs(y_test_all[valid] - pred_all[valid]))
  errors <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
  
  list(pred = save.pred, coef = save.coef, p_used = save.p, errors = errors)
}


