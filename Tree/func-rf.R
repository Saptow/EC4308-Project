# Random forest functions

runrf = function(Y, indice, lag) {
  # Drop date column 
  Y = Y[, -1, drop = FALSE]
  
  # 1) Split window into training rows and the last row used as forecast input
  Y_in  = Y[-nrow(Y), , drop = FALSE]  # up to time t-1
  Y_out = Y[nrow(Y),  , drop = FALSE]  # time t (inputs to forecast y_{t+lag})
  
  # 2) Extract target and predictors
  y_train     = Y_in[, indice, drop = FALSE]
  X_train_raw = Y_in[, -indice, drop = FALSE]
  
  # 3) PCA on TRAIN ONLY, X ONLY; keep enough PCs for 90% variance
  X_train_sc = scale(X_train_raw, center = TRUE, scale = TRUE)
  pca = princomp(X_train_sc)
  #var_cum = cumsum(pca$sdev^2 / sum(pca$sdev^2))
  #n_keep = min(which(var_cum >= 0.90))
  #if (!is.finite(n_keep)) n_keep = min(ncol(X_train_raw), 4)  # fallback
  n_keep = 4
  pcs_train = pca$scores[, 1:n_keep, drop = FALSE]
  colnames(pcs_train) = paste0("PC", seq_len(n_keep))
  
  # 4) Project the held-out last row (time t) onto training PCA loadings
  x_t_raw = Y_out[, -indice, drop = FALSE]
  x_t_sc  = scale(
    x_t_raw,
    center = attr(X_train_sc, "scaled:center"),
    scale  = attr(X_train_sc, "scaled:scale")
  )
  pcs_t = as.matrix(x_t_sc) %*% pca$loadings[, 1:n_keep, drop = FALSE]
  colnames(pcs_t) = paste0("PC", seq_len(n_keep))
  
  # 5) Build augmented matrices: [ y | X_raw | PCs ]
  # --- CHANGE 1: keep original names; do NOT rename to X1.. ---
  X_train_named <- X_train_raw                   # <-- keep real colnames here
  x_t_named     <- x_t_raw                       # <-- and here
  
  Y2_train   <- cbind(y = y_train[, 1], X_train_named, pcs_train)
  Y2_predrow <- cbind(y = Y_out[, indice, drop = FALSE][, 1], x_t_named, pcs_t)
  
  # Stack so we can form a consistent embed; last row is the forecast-input row
  Y2_all <- rbind(as.matrix(Y2_train), as.matrix(Y2_predrow))
  
  # 6) Create lagged design for a direct h-step (lag) forecast with 4 lags
  k <- 4 + lag
  aux <- embed(as.matrix(Y2_all), k)
  
  # --- CHANGE 2: add informative lag-aware column names after embed() ---
  base_names <- colnames(Y2_train)               # names for L0 block
  colnames(aux) <- unlist(lapply(0:(k-1), function(L) paste0(base_names, "_L", L)))
  
  # Target for training rows (drop last row, which is reserved for X_out)
  y <- aux[1:(nrow(aux) - 1), "y_L0"]
  
  # Training / forecast design matrices (already sliced from aux) 
  X     <- aux[1:(nrow(aux) - 1), -seq_len(ncol(Y2_all) * lag), drop = FALSE]
  X_new <- aux[nrow(aux),          -seq_len(ncol(Y2_all) * lag), drop = FALSE]
  
  # 1) Make a single, UNIQUE, RF-safe set of names (no duplicates, no spaces)
  cn <- make.names(colnames(X), unique = TRUE)
  colnames(X) <- cn
  colnames(X_new) <- cn  # align names before we coerce to data.frame
  
  # 2) Coerce to data.frame without altering names
  X <- as.data.frame(X, check.names = FALSE)
  
  # 3) Build X_out with the SAME columns and order as X, guaranteed
  X_out <- X[0, , drop = FALSE]     # empty row with identical cols
  X_out[1, ] <- as.numeric(X_new)   # fill values
  
  
  # 8) Fit Random Forest (on training only) and predict one h-step ahead
  model = randomForest(X, y, importance = TRUE)
  pred  = predict(model, X_out)
  
  list(model = model, pred = pred)
}



# Rolling window for random forest
rf.rolling.window = function(Y, nprev, indice = 1, lag = 1) {
  save.importance = vector("list", nprev)
  save.pred = matrix(NA_real_, nprev, 1)
  
  for (i in nprev:1) {
    # Estimation window slides forward one step each iteration
    Y.window = Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    
    rf_fit = runrf(Y.window, indice, lag)
    
    pos = 1 + nprev - i
    save.pred[pos, ] = rf_fit$pred
    save.importance[[pos]] = importance(rf_fit$model)
    cat("iteration", pos, "\n")
  }
  
  
  real = Y[, indice]
  rmse = sqrt(mean((tail(real, nprev) - save.pred[, 1])^2))
  mae  = mean(abs(tail(real, nprev) - save.pred[, 1]))
  errors = c(rmse = rmse, mae = mae)
  
  list(pred = save.pred, errors = errors, save.importance = save.importance)
}











