# Moving Average Rotation of X Random Forest


runmarxrf <- function(Y, indice, hstep, L_y = 4, P_marx = 12) {
  stopifnot(hstep >= 1L)
  
  # 0) Prep
  Y_no_date <- Y[, -1, drop = FALSE]
  Y_mat <- data.matrix(Y_no_date)
  if (indice < 1 || indice > ncol(Y_mat)) {
    stop(sprintf("indice=%d out of bounds after dropping date (1..%d)", indice, ncol(Y_mat)))
  }
  if (nrow(Y_mat) < (hstep + L_y + 2)) {
    stop("Too few rows for requested hstep/L_y. Add data or reduce hstep/L_y.")
  }
  
  # 1) Train vs last row
  Y_in  <- Y_mat[-nrow(Y_mat), , drop = FALSE]
  Y_out <- Y_mat[nrow(Y_mat),  , drop = FALSE]
  
  # 2) Split y / X
  y_train     <- Y_in[, indice, drop = FALSE]
  X_train_raw <- Y_in[, -indice, drop = FALSE]
  x_t         <- matrix(Y_out[, -indice, drop = FALSE], nrow = 1)
  
  # 3) Lag depth (ensure enough for MARX and y-lags)
  P <- max(P_marx, hstep + L_y - 1L)
  
  # 4) MARX on X (train + last row)
  if (!exists("marx_transform")) stop("marx_transform() not found; source it first.")
  X_aug <- rbind(X_train_raw, x_t)
  if (nrow(X_aug) <= P) stop("Not enough rows for MARX with lag depth P.")
  mt <- marx_transform(X_aug, n_lag = P, scale_data = FALSE)
  X_marx_all <- as.matrix(mt$mat_x_marx)
  n_rows_all <- nrow(X_marx_all)
  if (n_rows_all < 2) stop("Too few observations after MARX embedding.")
  
  # 5) y-lag block aligned with MARX rows
  y_full <- c(y_train[, 1], Y_out[, indice])
  y_emb  <- embed(y_full, P + 1L)
  y_emb  <- tail(y_emb, n_rows_all)
  colnames(y_emb) <- paste0("y_L", 0:P)
  
  # Use L_y consecutive lags starting at hstep: {hstep, ..., hstep+L_y-1}
  lags_y  <- hstep + seq_len(L_y) - 1L
  want_yc <- paste0("y_L", lags_y)
  if (!all(want_yc %in% colnames(y_emb))) {
    stop("Requested y-lags exceed available range; increase P_marx/L_y or reduce hstep.")
  }
  
  # Targets y_{t+hstep} for rows 1..(n_rows_all-1)
  y_target <- y_full[(P + 1L + hstep):(P + hstep + (n_rows_all - 1L))]
  if (!is.numeric(y_target) || anyNA(y_target)) stop("y_target has NA/non-numeric.")
  
  # 6) Design matrices
  X_train <- cbind(
    y_emb[1:(n_rows_all - 1L), want_yc, drop = FALSE],
    X_marx_all[1:(n_rows_all - 1L), , drop = FALSE]
  )
  X_out <- cbind(
    y_emb[n_rows_all, want_yc, drop = FALSE],
    X_marx_all[n_rows_all, , drop = FALSE]
  )
  cn <- make.names(colnames(X_train), unique = TRUE)
  colnames(X_train) <- cn
  colnames(X_out)   <- cn
  
  # 7) RF (defaults)
  set.seed(123)
  model <- randomForest::randomForest(
    x = as.data.frame(X_train),
    y = as.numeric(y_target),
    importance = TRUE
  )
  pred <- predict(model, as.data.frame(X_out))
  
  list(model = model, pred = pred)
}


marxrf.rolling.window <- function(Y, nprev, indice = 1, hstep = 1, L_y = 4, P_marx = 12) {
  if (!exists("marx_transform")) {
    source("data_transformation/marx_transform.R")
  }
  
  save.importance <- vector("list", nprev)
  save.pred <- matrix(NA_real_, nprev, 1)
  
  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    rf_fit <- runmarxrf(Y.window, indice, hstep, L_y = L_y, P_marx = P_marx)
    pos <- 1 + nprev - i
    save.pred[pos, ]       <- rf_fit$pred
    save.importance[[pos]] <- randomForest::importance(rf_fit$model)
    cat("iteration", pos)
  }
  
  real <- data.matrix(Y[, -1, drop = FALSE])[, indice]
  rmse <- sqrt(mean((tail(real, nprev) - save.pred[, 1])^2))
  mae  <- mean(abs(tail(real, nprev) - save.pred[, 1]))
  list(pred = save.pred, errors = c(rmse = rmse, mae = mae), save.importance = save.importance)
}