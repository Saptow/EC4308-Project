# Moving Average Factor proposed by Coulombe (2021)

tune_maf_rf <- function(
    Y, indice, hstep = 1,
    mtry_grid = c(20, 40, 80),
    min.node.size_grid = c(2, 5, 10),
    sample.fraction_grid = c(0.6, 0.8, 1.0),
    p_lag_x = 4, q_maf = 2, L_y = 4,
    scale_lags = TRUE,
    train_size = 240, verbose = TRUE
) {
  if (!exists("maf_transform")) {
    source("data_transformation/maf_transform.R")
  }
  
  combos <- expand.grid(
    mtry = mtry_grid,
    min.node.size = min.node.size_grid,
    sample.fraction = sample.fraction_grid
  )
  
  n_combos <- nrow(combos)
  n_obs <- nrow(Y)
  n_valid <- n_obs - train_size
  rmse_vec <- numeric(n_combos)
  
  if (verbose) cat("Starting recursive CV with", n_combos, "parameter combos...\n")
  
  # Loop over parameter combinations
  for (g in seq_len(n_combos)) {
    pars <- combos[g, ]
    preds <- numeric(n_valid)
    reals <- numeric(n_valid)
    
    # Recursive expanding window cross-validation
    for (i in seq_len(n_valid)) {
      Y.window <- Y[1:(train_size + i), , drop = FALSE]
      
      fit <- runmaf_rf(
        Y.window, indice, hstep,
        p_lag_x = p_lag_x, q_maf = q_maf, L_y = L_y,
        scale_lags = scale_lags,
        mtry = pars$mtry,
        min.node.size = pars$min.node.size,
        sample.fraction = pars$sample.fraction
      )
      
      preds[i] <- as.numeric(fit$pred)
      reals[i] <- as.numeric(Y[train_size + i, -1, drop = FALSE][, indice])
    }
    
    rmse <- sqrt(mean((reals - preds)^2, na.rm = TRUE))
    rmse_vec[g] <- rmse
    if (verbose) cat(
      sprintf("Combo %d/%d | RMSE = %.4f | mtry=%d | node=%d | samp.frac=%.1f\n",
              g, n_combos, rmse, pars$mtry, pars$min.node.size, pars$sample.fraction)
    )
  }
  
  results <- cbind(combos, rmse = rmse_vec)
  results <- results[order(results$rmse), ]
  if (verbose) cat("Best combo:\n"); print(head(results, 1))
  return(results)
}



runmaf_rf <- function(Y, indice, hstep,
                      p_lag_x = 4, q_maf = 2, L_y = 4, scale_lags = TRUE,
                      mtry = NULL, min.node.size = NULL, sample.fraction = NULL) {
  stopifnot(hstep >= 1L)
  Y_no_date <- Y[, -1, drop = FALSE]
  Y_mat <- data.matrix(Y_no_date)
  if (indice < 1 || indice > ncol(Y_mat)) stop("indice out of bounds.")
  
  # Split last row as forecast input
  Y_in  <- Y_mat[-nrow(Y_mat), , drop = FALSE]
  Y_out <- Y_mat[nrow(Y_mat),  , drop = FALSE]
  
  # Build MAFs on TRAIN + last row (so we can get features at T)
  X_train_raw <- Y_in[, -indice, drop = FALSE]
  x_t         <- matrix(Y_out[, -indice, drop = FALSE], nrow = 1)
  X_aug <- rbind(X_train_raw, x_t)
  
  mt <- maf_transform(X_aug, p_lag = p_lag_x, q_maf = q_maf, scale_lags = scale_lags)
  maf_all <- mt$maf                 # rows = T_eff_all
  n_rows_all <- nrow(maf_all)
  
  # y embedding aligned with maf rows
  y_full <- c(Y_in[, indice], Y_out[indice])       # length T_train+1
  P <- max(p_lag_x, hstep + L_y - 1L)              # ensure enough y-lags
  y_emb <- embed(y_full, P + 1L)
  y_emb <- tail(y_emb, n_rows_all)                 # align to maf rows
  colnames(y_emb) <- paste0("y_L", 0:P)
  
  lags_y <- hstep + seq_len(L_y) - 1L              # {hstep,...,hstep+L_y-1}
  want_yc <- paste0("y_L", lags_y)
  if (!all(want_yc %in% colnames(y_emb))) stop("Increase p_lag_x or reduce L_y.")
  
  # Targets: y_{t+h} for each training row
  y_target <- y_full[(P + 1L + hstep):(P + hstep + (n_rows_all - 1L))]
  if (!is.numeric(y_target) || anyNA(y_target)) stop("y_target invalid.")
  
  # Design matrices
  X_train <- cbind(
    y_emb[1:(n_rows_all - 1L), want_yc, drop = FALSE],
    maf_all[1:(n_rows_all - 1L), , drop = FALSE]
  )
  X_out <- cbind(
    y_emb[n_rows_all, want_yc, drop = FALSE],
    maf_all[n_rows_all, , drop = FALSE]
  )
  cn <- make.names(colnames(X_train), unique = TRUE)
  colnames(X_train) <- cn; colnames(X_out) <- cn
  
  set.seed(123)
  model <- randomForest::randomForest(
    x = as.data.frame(X_train),
    y = as.numeric(y_target),
    mtry = if (!is.null(mtry)) mtry else floor(sqrt(ncol(X_train))),
    nodesize = if (!is.null(min.node.size)) min.node.size else 5,
    sampsize = if (!is.null(sample.fraction)) 
      floor(sample.fraction * nrow(X_train)) else nrow(X_train),
    importance = TRUE
  )
  pred <- predict(model, as.data.frame(X_out))
  list(model = model, pred = pred)
}


mafrf.rolling.window <- function(Y, nprev, indice = 1, hstep = 1,
                                  p_lag_x = 4, q_maf = 2, L_y = 4,
                                  scale_lags = TRUE, verbose = TRUE,
                                  mtry = NULL, min.node.size = NULL, 
                                  sample.fraction = NULL) {
  
  if (!exists("maf_transform")) {
    source("data_transformation/maf_transform.R")
  }
  save.importance <- vector("list", nprev)
  save.pred <- matrix(NA_real_, nprev, 1)
  
  for (i in nprev:1) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    fit <- runmaf_rf(Y.window, indice, hstep,
                     p_lag_x = p_lag_x, q_maf = q_maf, L_y = L_y,
                     scale_lags = scale_lags, mtry = mtry, 
                     min.node.size = min.node.size, sample.fraction = sample.fraction)
    pos <- 1 + nprev - i
    save.pred[pos, ]       <- fit$pred
    save.importance[[pos]] <- randomForest::importance(fit$model)
    if (verbose) cat("iter", pos)
  }
  
  real <- data.matrix(Y[, -1, drop = FALSE])[, indice]
  oosy <- tail(real, nprev)
  rmse <- sqrt(mean((oosy - save.pred[, 1])^2))
  mae  <- mean(abs(oosy - save.pred[, 1]))
  list(pred = save.pred, errors = c(rmse = rmse, mae = mae),
       save.importance = save.importance)
}

