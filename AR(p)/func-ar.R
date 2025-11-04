# ============================================
# AR(p) direct h-step with contemporaneous dummy
# ============================================
runAR <- function(Y, h = 1, target_name = "UNRATE", type = "fixed", L_max = 4) {
  Y     <- Y[, -1, drop = FALSE]
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  idx_y   <- which(colnames(Y_in) == target_name)
  idx_dum <- ncol(Y_in)
  
  y_train <- as.numeric(Y_in[, idx_y])
  d_train <- as.numeric(Y_in[, idx_dum])
  d_new   <- as.numeric(Y_out[, idx_dum])
  
  k_max <- h + L_max + 1
  if (length(y_train) < k_max) stop("Window too short for h + L_max + 1 lags.")
  aux <- embed(y_train, k_max)
  colnames(aux) <- paste0("y_L", 0:(k_max - 1))
  
  d_aligned <- tail(d_train, nrow(aux))
  y_vec <- aux[, paste0("y_L", h)]
  
  # choose p
  if (identical(type, "fixed")) {
    p <- L_max
  } else if (identical(type, "bic")) {
    best_bic <- Inf; best_p <- 1
    for (pp in 1:L_max) {
      lag_names <- paste0("y_L", (h + 1):(h + pp))
      X_pp <- as.data.frame(cbind(aux[, lag_names, drop = FALSE], DUM = d_aligned))
      # drop zero-variance columns early
      nzv <- vapply(X_pp, function(z) var(z) > 0, logical(1))
      X_pp <- X_pp[, nzv, drop = FALSE]
      m_pp  <- lm(y_vec ~ ., data = as.data.frame(X_pp))
      b_pp  <- BIC(m_pp)
      if (b_pp < best_bic) { best_bic <- b_pp; best_p <- pp }
    }
    p <- best_p
  } else stop('type must be "fixed" or "bic"')
  
  # build final X
  lag_names <- paste0("y_L", (h + 1):(h + p))
  X_df <- as.data.frame(cbind(aux[, lag_names, drop = FALSE], DUM = d_aligned))
  
  # 1) drop zero-variance columns (incl. constant dummy)
  nzv <- vapply(X_df, function(z) var(z) > 0, logical(1))
  X_df <- X_df[, nzv, drop = FALSE]
  
  # 2) preliminary fit, then remove aliased (non-estimable) terms
  m0 <- lm(y_vec ~ ., data = X_df)
  ali <- alias(m0)$Complete
  if (!is.null(ali)) {
    drop_cols <- rownames(ali)
    keep <- setdiff(colnames(X_df), drop_cols)
    X_df <- X_df[, keep, drop = FALSE]
  }
  
  # final fit
  df <- data.frame(y = y_vec, X_df, check.names = FALSE)
  model <- lm(y ~ ., data = df)
  
  # build X_new using only estimable columns (names in coef(model) minus intercept)
  keep_terms <- setdiff(names(coef(model)), "(Intercept)")
  # construct a full row first
  base_row <- as.list(c(as.numeric(aux[nrow(aux), lag_names, drop = FALSE]), DUM = d_new))
  names(base_row)[seq_along(lag_names)] <- lag_names
  # subset to estimable terms
  X_new <- as.data.frame(as.list(base_row[keep_terms]), check.names = FALSE)
  # ensure same order
  X_new <- X_new[, keep_terms, drop = FALSE]
  
  pred <- as.numeric(predict(model, newdata = X_new))
  
  # pad coefficients to 1 + L_max + 1 for storage
  ar_coef  <- coef(model)
  coef_pad <- rep(NA_real_, 1 + L_max + 1)
  coef_pad[1:length(ar_coef)] <- ar_coef
  
  list(model = model, pred = pred, coef = coef_pad, p = p)
}

# =====================================================
# Rolling window
# =====================================================
ar.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE", type = "fixed", L_max = 4) {
  ncoef     <- 1 + L_max + 1
  save.coef <- matrix(NA_real_, nprev, ncoef)  # (Intercept) + up to L_max lags + DUM
  save.pred <- matrix(NA_real_, nprev, 1)
  save.p    <- integer(nprev)
  
  set.seed(12455)
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    fit <- runAR(Y.window, h = h, target_name = target_name, type = type, L_max = L_max)
    
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
