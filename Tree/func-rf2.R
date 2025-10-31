# Random forest 2: 
#We run PCA on X variables, keep 4 PCs, and augment on lags of y to predict
#Design matrix is pcs, lags of pcs and lags of y

runrf = function(Y, indice, lag) {
  # ---------------- SETTINGS ----------------
  n_keep <- 4   # number of PCs to keep (max)
  L_y    <- 4   # lags of y to use
  L_pc   <- 4   # lags of each PC to use
  # ------------------------------------------
  
  # 0) Drop date; split into train (1..T-1) and last row T for prediction
  Y <- Y[, -1, drop = FALSE]
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # 1) Extract y and X (train only)
  y_train     <- Y_in[, indice, drop = FALSE]
  X_train_raw <- Y_in[, -indice, drop = FALSE]
  
  # 2) PCA on TRAIN ONLY (X only) — use prcomp correctly
  X_train_sc <- scale(X_train_raw, center = TRUE, scale = TRUE)
  max_pc <- min(ncol(X_train_sc), nrow(X_train_sc) - 1)
  n_pc <- min(n_keep, max_pc)
  
  pca <- prcomp(X_train_sc, center = FALSE, scale. = FALSE, rank. = n_pc)
  n_pc <- min(n_pc, ncol(pca$rotation))  # guard rank deficiency
  
  pcs_train <- predict(pca, X_train_sc)[, seq_len(n_pc), drop = FALSE]
  pcs_train <- as.matrix(pcs_train)
  colnames(pcs_train) <- paste0("PC", seq_len(n_pc))
  
  # 3) Compute PC score at time T using train scalers/loadings
  x_t_sc <- scale(
    as.matrix(Y_out[, -indice, drop = FALSE]),
    center = attr(X_train_sc, "scaled:center"),
    scale  = attr(X_train_sc, "scaled:scale")
  )
  pcs_t <- x_t_sc %*% pca$rotation[, seq_len(n_pc), drop = FALSE]
  pcs_t <- as.matrix(pcs_t)  # 1 x n_pc
  colnames(pcs_t) <- paste0("PC", seq_len(n_pc))
  
  # 4) Keep ONLY y and PCs; stack with final row
  Y2_train   <- cbind(y = y_train[, 1], pcs_train)
  Y2_predrow <- cbind(y = Y_out[, indice, drop = FALSE][, 1], pcs_t)
  Y2_all <- rbind(as.matrix(Y2_train), as.matrix(Y2_predrow))
  
  # 5) Create lagged design matrix
  k <- max(L_y, L_pc) + lag
  aux <- embed(as.matrix(Y2_all), k)
  
  # Label blocks L0..L(k-1)
  base_names <- colnames(Y2_train)  # c("y", "PC1", "PC2", ...)
  colnames(aux) <- unlist(lapply(0:(k-1), function(L) paste0(base_names, "_L", L)))
  
  # 6) Construct lagged features and target variable (FIXED)
  lags_y  <- lag + seq_len(L_y)  - 1      # e.g., lag=6 & L_y=4 -> 6,7,8,9
  lags_pc <- lag + seq_len(L_pc) - 1      # same idea for PCs
  
  want_y  <- paste0("y_L",  lags_y)
  want_pc <- as.vector(outer(paste0("PC", 1:n_pc), paste0("_L", lags_pc), paste0))
  keep_names <- c(want_y, want_pc)
  if (length(keep_names) == 0) {
    stop("No predictors selected: check lag/L_y/L_pc and sample size.")
  }

  y     <- as.numeric(aux[1:(nrow(aux) - 1), "y_L0"])
  X     <- aux[1:(nrow(aux) - 1), keep_names, drop = FALSE]
  X_new <- aux[nrow(aux),          keep_names, drop = FALSE]
  
  # 7) Prepare X_out = latest feature vector for forecasting
  cn <- make.names(colnames(X), unique = TRUE)
  colnames(X) <- cn; colnames(X_new) <- cn
  X <- as.data.frame(X, check.names = FALSE)
  X_out <- X[0, , drop = FALSE]; X_out[1, ] <- as.numeric(X_new)
  
  # 8) Fit RF and predict
  set.seed(123)
  model <- randomForest::randomForest(x = X, y = y, importance = TRUE)
  pred  <- predict(model, X_out)
  
  list(model = model, pred = pred)
}



# Rolling window for random forest
rf2.rolling.window = function(Y, nprev, indice = 1, lag = 1) {
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
