rm(list=ls())
#setwd()
load("data/fredmd_cleaned.RData") 

library(glmnet)
library(HDeconometrics)
library(hdm)

Y = md
nprev=120
######################################
#LASSO AND ROLLING WINDOW FUNCTION
######################################
runlasso <- function(Y, h = 1, target_name = "UNRATE", alpha = 1, IC = "bic") {
  L_y  <- 4
  L_pc <- L_y
  
  # 0) Drop date; split into train (1..T-1) and last row T
  Y <- subset(Y, select = -date)
  Y_in  <- Y[-nrow(Y), , drop = FALSE]
  Y_out <- Y[nrow(Y),  , drop = FALSE]
  
  # Identify target & dummy (dummy = last col)
  indice  <- which(colnames(Y_in) == target_name)
  dum_idx <- which(colnames(Y_in) == "aft_break") # dummy var index
  
  # 1) y and X for PCA (exclude target + dummy)
  y_train     <- Y_in[, indice, drop = FALSE]
  pca_cols    <- setdiff(seq_len(ncol(Y_in)), c(indice, dum_idx))
  X_train_raw <- Y_in[, pca_cols, drop = FALSE]
  pca_vars    <- colnames(X_train_raw)
  
  # 2) PCA on TRAIN ONLY (X only)
  X_train_sc <- scale(X_train_raw, center = TRUE, scale = TRUE)
  max_pc <- min(ncol(X_train_sc), nrow(X_train_sc) - 1)
  pca <- prcomp(X_train_sc, center = FALSE, scale. = FALSE, rank. = max_pc)
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  n_pc <- which(cumsum(var_exp) >= 0.90)[1]; if (is.na(n_pc)) n_pc <- 1 #keep 90% variance
  
  pcs_train <- predict(pca, X_train_sc)[, seq_len(n_pc), drop = FALSE]
  colnames(pcs_train) <- paste0("PC", seq_len(n_pc))
  
  # Project last row T to PCs with train scalers/loadings
  x_t_raw <- Y_out[, pca_cols, drop = FALSE]
  x_t_sc  <- scale(x_t_raw,
                   center = attr(X_train_sc, "scaled:center"),
                   scale  = attr(X_train_sc, "scaled:scale"))
  pcs_t <- as.matrix(x_t_sc) %*% pca$rotation[, 1:n_pc, drop = FALSE]
  colnames(pcs_t) <- paste0("PC", seq_len(n_pc))
  
  # Contemporaneous dummy
  dum_train <- Y_in[, dum_idx, drop = FALSE]; colnames(dum_train) <- "DUM"
  dum_t     <- Y_out[, dum_idx, drop = FALSE]; colnames(dum_t)   <- "DUM"
  
  # Keep y, PCs, and contemporaneous dummy; stack with final row
  Y2_train   <- cbind(y = y_train[, 1], pcs_train, DUM = dum_train[, 1])
  Y2_predrow <- cbind(y = Y_out[, indice, drop = FALSE][, 1], pcs_t, DUM = dum_t[, 1])
  Y2_all <- rbind(as.matrix(Y2_train), as.matrix(Y2_predrow))
  
  # 3) Create lagged design and apply horizon fix for h-step forecast
  k <- max(L_y, L_pc) + h
  aux <- embed(as.matrix(Y2_all), k)
  
  base_names <- colnames(Y2_train)  # c("y","PC1",...,"DUM")
  colnames(aux) <- unlist(lapply(0:(k-1), function(L) paste0(base_names, "_L", L)))
  
  # Drop first h blocks (L0..L(h-1)); target is y_Lh
  block   <- ncol(Y2_all)
  aux2    <- aux[, -(seq_len(block * h)), drop = FALSE]
  y_col   <- paste0("y_L", h)
  if (!y_col %in% colnames(aux2)) stop("Target column not found after horizon fix.")
  
  # Keep ONLY contemporaneous dummy 
  all_feat_cols <- setdiff(colnames(aux2), y_col)
  dum_cols_all  <- grep("^DUM_L", colnames(aux2), value = TRUE)
  dum_keep      <- paste0("DUM_L", h)
  dum_drop      <- setdiff(dum_cols_all, dum_keep)
  feat_cols     <- setdiff(all_feat_cols, dum_drop)
  
  # Split y, X (train) and X_new (last row)
  y     <- as.numeric(aux2[1:(nrow(aux2) - 1), y_col])
  X     <- aux2[1:(nrow(aux2) - 1), feat_cols, drop = FALSE]
  X_new <- aux2[nrow(aux2),          feat_cols, drop = FALSE]
  
  # Separate dummy column to avoid scaling it
  dum_name <- dum_keep
  non_dummy <- setdiff(colnames(X), dum_name)
  X_non <- X[, non_dummy, drop = FALSE]
  X_non_sc <- scale(X_non)
  x_center <- attr(X_non_sc, "scaled:center")
  x_scale  <- attr(X_non_sc, "scaled:scale")
  
  X_train_mat <- cbind(X_non_sc, DUM = X[, dum_name, drop = FALSE])
  colnames(X_train_mat)[ncol(X_train_mat)] <- dum_name
  
  # Scale newx consistently
  X_new_non <- X_new[, non_dummy, drop = FALSE]
  X_new_sc  <- sweep(X_new_non, 2, x_center, "-")
  X_new_sc  <- sweep(X_new_sc, 2, x_scale, "/")
  newx <- cbind(X_new_sc, DUM = X_new[, dum_name, drop = FALSE])
  colnames(newx)[ncol(newx)] <- dum_name
  
  # 4) Fit LASSO via IC and predict
  fit <- HDeconometrics::ic.glmnet(
    x     = as.matrix(X_train_mat),
    y     = y,
    crit  = IC,
    alpha = alpha
  )
  pred <- as.numeric(predict(fit, newx = as.matrix(newx)))
  
  list(model = fit, pred = pred, pca = pca, n_pc = n_pc,
       var_exp = var_exp, pca_vars = pca_vars)
}


# Rolling window wrapper mirroring your rf2.rolling.window()
lasso2.rolling.window <- function(Y, nprev, h = 1, target_name = "UNRATE",
                                  alpha = 1, IC = "bic") {
  save.pred <- rep(NA_real_, nprev)
  save.pca      <- vector("list", nprev)
  save.n_pc     <- integer(nprev)
  save.pca_vars <- vector("list", nprev)
  save.coef     <- vector("list", nprev)  # store sparse coef safely
  
  for (i in nprev:max(h,1)) {
    Y.window <- Y[(1 + nprev - i):(nrow(Y) - i), , drop = FALSE]
    fitobj <- runlasso(Y.window, h = h, target_name = target_name,
                       alpha = alpha, IC = IC)
    
    t  <- nrow(Y) - i
    u  <- t + h
    pos <- u - (nrow(Y) - nprev)
    
    if (pos >= 1 && pos <= nprev) {
      save.pred[pos] <- fitobj$pred
      save.pca[[pos]]      <- fitobj$pca
      save.n_pc[pos]       <- fitobj$n_pc
      save.pca_vars[[pos]] <- fitobj$pca_vars
      
      # coefficients as numeric named vector
      cf <- as.numeric(fitobj$model$coef)
      names(cf) <- rownames(fitobj$model$coef)
      save.coef[[pos]] <- cf
    }
    cat("iteration", pos, "\n")
  }
  
  real <- Y[, which(colnames(Y) == target_name)]
  y_test_full <- tail(real, nprev)
  pred_full   <- save.pred
  valid     <- !is.na(pred_full)
  y_test <- y_test_full[valid]
  pred      <- pred_full[valid]
  
  rmse <- sqrt(mean((y_test - pred)^2))
  mae  <- mean(abs(y_test - pred))
  errors <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
  
  list(pred = save.pred, errors = errors,
       save.pca = save.pca, save.n_pc = save.n_pc, save.pca_vars = save.pca_vars,
       save.coef = save.coef)
}

############################################################################
#Penalized regression: LASSO forecasts (BIC, AIC, AICc)
############################################################################


alpha=1 #set alpha=1 for LASSO

#Run forecasts for LASSO (BIC)
lasso1c=lasso2.rolling.window(Y,nprev,h=1,target_name = "UNRATE",alpha,IC="bic")
lasso3c=lasso2.rolling.window(Y,nprev,h=3,target_name = "UNRATE",alpha,IC="bic")
lasso6c=lasso2.rolling.window(Y,nprev,h=6,target_name = "UNRATE",alpha,IC="bic")
lasso12c=lasso2.rolling.window(Y,nprev,h=12,target_name = "UNRATE",alpha,IC="bic")

plot(tail(Y[,"UNRATE"],120), type = 'l')
lines(lasso1c$pred, type='l', col = 'red')
