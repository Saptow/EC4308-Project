# Random forest: 
#We run PCA on X variables, keep 4 PCs, and augment on lags of y to predict
#Design matrix is pcs, lags of pcs, lags of y and dummy 



# Random forest (h-step), PCA on X-only (excl. target & dummy), horizon fix,
# features = lags of y, lags of PCs, and contemporaneous dummy.

runrf = function(Y, h, target_name = "UNRATE") {
  L_y  = 4 # lags of y to keep
  L_pc = L_y # lags of pcs to keep, same as lags of y 
  
  # drop date and leave last row for prediction
  Y <- Y[, -1, drop = FALSE] 
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # set target and dummy indices
  indice  <- which(colnames(Y_in) == target_name)
  dum_idx <- ncol(Y_in)
  
  # separate y and X
  y_train     <- Y_in[, indice, drop = FALSE]
  pca_cols    <- setdiff(seq_len(ncol(Y_in)), c(indice, dum_idx))
  X_train_raw <- Y_in[, pca_cols, drop = FALSE]
  pca_vars <- colnames(X_train_raw)
  
  # pca on X_train
  X_train_sc <- scale(X_train_raw, center = TRUE, scale = TRUE)
  max_pc <- min(ncol(X_train_sc), nrow(X_train_sc) - 1)
  pca <- prcomp(X_train_sc, center = FALSE, scale. = FALSE, rank. = max_pc)
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  n_pc <- which(cumsum(var_exp) >= 0.90)[1]; if (is.na(n_pc)) n_pc <- 1
  
  pcs_train <- predict(pca, X_train_sc)[, seq_len(n_pc), drop = FALSE]
  colnames(pcs_train) <- paste0("PC", seq_len(n_pc))
  
  # project last row T to PCs with train scalers/loadings
  x_t_raw <- Y_out[, pca_cols, drop = FALSE]
  x_t_sc  <- scale(x_t_raw,
                   center = attr(X_train_sc, "scaled:center"),
                   scale  = attr(X_train_sc, "scaled:scale"))
  pcs_t <- as.matrix(x_t_sc) %*% pca$rotation[, 1:n_pc, drop = FALSE]
  colnames(pcs_t) <- paste0("PC", seq_len(n_pc))
  
  # dummy contemporaneous
  dum_train <- Y_in[, dum_idx, drop = FALSE]; colnames(dum_train) <- "DUM"
  dum_t     <- Y_out[, dum_idx, drop = FALSE]; colnames(dum_t)     <- "DUM"
  
  # Keep y, PCs, and dummy; stack with final row
  Y2_train   <- cbind(y = y_train[, 1], pcs_train, DUM = dum_train[, 1])
  Y2_predrow <- cbind(y = Y_out[, indice, drop = FALSE][, 1], pcs_t, DUM = dum_t[, 1])
  Y2_all <- rbind(as.matrix(Y2_train), as.matrix(Y2_predrow))
  
  # create lagged design matrix
  k <- max(L_y, L_pc) + h
  aux <- embed(as.matrix(Y2_all), k)
  
  base_names <- colnames(Y2_train)  
  colnames(aux) <- unlist(lapply(0:(k-1), function(L) paste0(base_names, "_L", L)))
  
  # Drop first h blocks
  block   <- ncol(Y2_all)
  aux2    <- aux[, -(seq_len(block * h)), drop = FALSE]
  y_col   <- paste0("y_L", h)
  if (!y_col %in% colnames(aux2)) stop("Target column not found after horizon fix.")
  
  # keep ONLY contemporaneous dummy
  all_feat_cols <- setdiff(colnames(aux2), y_col)
  dum_cols_all  <- grep("^DUM_L", colnames(aux2), value = TRUE)
  dum_keep      <- paste0("DUM_L", h)
  dum_drop      <- setdiff(dum_cols_all, dum_keep)
  feat_cols     <- setdiff(all_feat_cols, dum_drop)
  
  y     <- as.numeric(aux2[1:(nrow(aux2) - 1), y_col])
  X     <- aux2[1:(nrow(aux2) - 1), feat_cols, drop = FALSE]
  X_new <- aux2[nrow(aux2),          feat_cols, drop = FALSE]
  
  # Fit RF and predict
  X <- as.data.frame(X, check.names = FALSE)
  X_out <- X[0, , drop = FALSE]; X_out[1, ] <- as.numeric(X_new)
  model <- ranger(x = X,y = y,importance = "permutation", max.depth=5, mtry=floor(ncol(X)/3))
  pred  <- predict(model, X_out)
  
  list(model = model, pred = pred$predictions, pca = pca, n_pc = n_pc, 
       var_exp = var_exp, pca_vars = pca_vars)
}

# Rolling window using h-step
rf2.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE") {
  save.importance <- vector("list", nprev)
  save.pred <- matrix(NA_real_, nprev, 1)
  save.pca      <- vector("list", nprev)  
  save.n_pc     <- integer(nprev)         
  save.pca_vars <- vector("list", nprev)
  
  set.seed(12455)
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    rf_fit <- runrf(Y.window, h = h, target_name = target_name)
    
    t  <- nrow(Y) - i          
    u  <- t + h                
    pos <- u - (nrow(Y) - nprev)
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos, ] <- rf_fit$pred
      save.importance[[pos]] <- importance(rf_fit$model)
      save.pca[[pos]]      <- rf_fit$pca     
      save.n_pc[pos]       <- rf_fit$n_pc     
      save.pca_vars[[pos]] <- rf_fit$pca_vars
    }
    cat("iteration", pos, "\n")
  }
  
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
  
  list(pred = save.pred, errors = errors, save.importance = save.importance, 
       save.pca        = save.pca,        
       save.n_pc       = save.n_pc,      
       save.pca_vars   = save.pca_vars)  
}
