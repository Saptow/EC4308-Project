# Moving Average Rotation of X Random Forest

suppressPackageStartupMessages({
  library(randomForest)
})

runmarxrf <- function(Y, indice, lag, L_y = 4, P_marx = 12) {
  stopifnot(lag >= 1)
  
  # 0) Drop date; coerce to a numeric matrix right away (avoids list/factor cols)
  Y_no_date <- Y[, -1, drop = FALSE]
  Y_mat <- data.matrix(Y_no_date)  # converts factors/integers to numeric; errors if non-numeric list cols
  
  # Guard: indice must refer to Y WITHOUT the date column
  if (indice < 1 || indice > ncol(Y_mat)) {
    stop(sprintf("indice=%d is out of bounds after dropping date (1..%d).",
                 indice, ncol(Y_mat)))
  }
  
  if (nrow(Y_mat) < (lag + L_y + 2)) {
    stop("Too few rows for the requested lag/L_y. Add data or reduce lag/L_y.")
  }
  
  # 1) Split train vs last row for prediction
  Y_in  <- Y_mat[-nrow(Y_mat), , drop = FALSE]
  Y_out <- Y_mat[nrow(Y_mat),  , drop = FALSE]
  
  # 2) Extract y and X (TRAIN ONLY)
  y_train     <- Y_in[, indice, drop = FALSE]       # numeric matrix (col)
  X_train_raw <- Y_in[, -indice, drop = FALSE]      # predictors matrix
  x_t         <- matrix(Y_out[, -indice, drop = FALSE], nrow = 1)
  
  # 3) Choose lag depth sufficient for MARX and y-lag features
  P <- max(P_marx, lag + L_y)
  
  # 4) MARX on predictors ONLY using TRAIN + last row
  #    Expect marx_transform() already sourced elsewhere once.
  if (!exists("marx_transform")) {
    stop("marx_transform() not found. Source it before calling runmarxrf().")
  }
  X_aug <- rbind(X_train_raw, x_t)                   # numeric matrix
  if (nrow(X_aug) <= P) stop("Not enough rows for MARX with lag depth P.")
  mt <- marx_transform(X_aug, n_lag = P, scale_data = FALSE)
  
  X_marx_all <- mt$mat_x_marx
  if (!is.matrix(X_marx_all)) X_marx_all <- as.matrix(X_marx_all)
  n_rows_all <- nrow(X_marx_all)
  if (n_rows_all < 2) stop("Not enough observations after MARX embedding.")
  
  # 5) Build y-lag features aligned with MARX rows
  y_full <- c(y_train[, 1], Y_out[, indice])         # pure numeric vector
  y_emb  <- embed(y_full, P + 1)                     # matrix (rows = length(y_full) - P)
  y_emb  <- tail(y_emb, n_rows_all)
  colnames(y_emb) <- paste0("y_L", 0:P)
  
  # y-lag predictors: {lag, ..., lag+L_y-1}
  lags_y  <- lag + seq_len(L_y) - 1
  want_yc <- paste0("y_L", lags_y)
  if (!all(want_yc %in% colnames(y_emb))) {
    stop("Requested y lags exceed available range; increase P or reduce L_y/lag.")
  }
  
  # Targets for rows 1..(n_rows_all-1): y_{t+lag}
  # Index math matches construction of y_full and y_emb
  y_target <- y_full[(P + 1 + lag):(P + lag + (n_rows_all - 1))]
  if (!is.numeric(y_target) || anyNA(y_target)) {
    stop("y_target is not a clean numeric vector (check for NAs or types).")
  }
  
  # 6) Design matrices
  X_train <- cbind(
    y_emb[1:(n_rows_all - 1), want_yc, drop = FALSE],
    X_marx_all[1:(n_rows_all - 1), , drop = FALSE]
  )
  X_out <- cbind(
    y_emb[n_rows_all, want_yc, drop = FALSE],
    X_marx_all[n_rows_all, , drop = FALSE]
  )
  
  colnames(X_train) <- make.names(colnames(X_train), unique = TRUE)
  colnames(X_out)   <- colnames(X_train)
  
  # 7) Fit RF and predict
  set.seed(123)
  model <- randomForest::randomForest(
    x = as.data.frame(X_train),
    y = as.numeric(y_target),
    importance = TRUE
  )
  pred <- predict(model, as.data.frame(X_out))
  
  list(model = model, pred = pred)
}

marxrf.rolling.window <- function(Y, nprev, indice = 1, lag = 1) {
  if (!exists("marx_transform")) {
    source("data_transformation/marx_transform.R")
  }
  
  save.importance <- vector("list", nprev)
  save.pred <- matrix(NA_real_, nprev, 1)
  
  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    rf_fit <- runmarxrf(Y.window, indice, lag)
    pos <- 1 + nprev - i
    save.pred[pos, ]       <- rf_fit$pred
    save.importance[[pos]] <- randomForest::importance(rf_fit$model)
    cat("iteration", pos, "\n")
  }
  
  real <- data.matrix(Y[, -1, drop = FALSE])[, indice]
  rmse <- sqrt(mean((tail(real, nprev) - save.pred[, 1])^2))
  mae  <- mean(abs(tail(real, nprev) - save.pred[, 1]))
  list(pred = save.pred, errors = c(rmse = rmse, mae = mae), save.importance = save.importance)
}
