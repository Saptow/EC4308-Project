# ------------------------------------------
# Run function for hybrid method (rLASSO + RF)
# Expects a data matrix X as well as desired lags for Y and X, and options for maf and marx
# ------------------------------------------
runhybrid <- function(X, h=1, L_y=4, target_name="UNRATE"){
    L_y <- 4
    L_pc <- L_y # both are 4 lags

    # Initial data cleaning
    X <- subset(X, select= -date) # remove date column if present
    X_in <- X[-nrow(X), , drop=FALSE]
    X_out <- X[nrow(X), , drop=FALSE]

    dum_idx <- which(colnames(X) == "aft_break") # dummy ind
    ind <- which(colnames(X) == target_name) # target ind

    # separate y and X for PCA
    y_train <- X_in[, ind, drop=FALSE] 
    pca_cols <- setdiff(seq_len(ncol(X_in)), c(ind, dum_idx))
    X_train_raw <- X_in[, pca_cols, drop=FALSE]
    pca_vars <- colnames(X_train_raw)

    # PCA on train only (X only)
    X_train_sc <- scale(X_train_raw, center=TRUE, scale=TRUE)
    max_pc <- min(ncol(X_train_sc), nrow(X_train_sc) - 1)
    pca <- prcomp(X_train_sc, center=FALSE, scale.=FALSE, rank.=max_pc)
    var_exp <- pca$sdev^2 / sum(pca$sdev^2)
    n_pc <- which(cumsum(var_exp) >= 0.90)[1]; if (is.na(n_pc)) n_pc <- 1 # keep 90% variance

    pcs_train <- predict(pca, X_train_sc)[, seq_len(n_pc), drop=FALSE]
    colnames(pcs_train) <- paste0("PC", seq_len(n_pc))

    # Project last row T to PCs with train scalers/loadings
    x_t_raw <- X_out[, pca_cols, drop=FALSE]
    x_t_sc <- scale(x_t_raw,
                    center=attr(X_train_sc, "scaled:center"),
                    scale=attr(X_train_sc, "scaled:scale"))
    pcs_t <- as.matrix(x_t_sc) %*% pca$rotation[, 1:n_pc, drop=FALSE]
    colnames(pcs_t) <- paste0("PC", seq_len(n_pc))

    # Contemporaneous dummy
    dum_train <- X_in[, dum_idx, drop = FALSE]; colnames(dum_train) <- "DUM"
    dum_t     <- X_out[, dum_idx, drop = FALSE]; colnames(dum_t)   <- "DUM"

    # Keep y, PCs, and contemporaneous dummy; stack with final row
    Y2_train   <- cbind(y = y_train[, 1], pcs_train, DUM = dum_train[, 1])
    Y2_predrow <- cbind(y = X_out[, ind, drop = FALSE][, 1], pcs_t, DUM = dum_t[, 1])
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
    
    # First stage: fit rLASSO to get predictions and residuals
    rlasso.fit <- rlasso(as.matrix(X_train_mat), y, post=FALSE)
    rhat.rlasso <- rlasso.fit$residuals
    rlasso.pred <- predict(rlasso.fit, newdata=as.matrix(newx))
    # Second stage: fit RF on rLASSO residuals (use p/3 for regression)
    df_rf_train <- cbind.data.frame(rhat=rhat.rlasso, X_train_mat)
    rf_resid <- ranger(
        dependent.variable.name = "rhat",
        data = df_rf_train,
        max.depth = 5, 
        mtry=floor(ncol(X_train_mat)/3), # p/3 for regression
        importance="permutation"
    )
    rf_resid_pred <- predict(rf_resid, data = as.data.frame(newx))$predictions

    # Hybrid prediction: sum of rLASSO and RF residual predictions
    hybrid_pred <- as.numeric(rlasso.pred + rf_resid_pred)

    list(
        rlasso_model = rlasso.fit,
        rf_resid_model = rf_resid,
        hybrid_pred = hybrid_pred,
        pca = pca,
        n_pc = n_pc,
        var_exp = var_exp,
        pca_vars = pca_vars
    )
}

hybrid.rolling.window <- function(X, nprev, h=1, target_name = "UNRATE") {
    # Set up default variables 
    save.pred <- rep(NA_real_, nprev)
    save.pca      <- vector("list", nprev)
    save.n_pc     <- integer(nprev)
    save.pca_vars <- vector("list", nprev)
    save.lasso.coef    <- vector("list", nprev)  # store sparse coef safely

    for (i in nprev:max(h,1)) {
        X.window <- X[(1 + nprev - i):(nrow(X) - i), , drop = FALSE]
        fitobj <- runhybrid(X.window, h = h, target_name = target_name)

        t  <- nrow(X) - i
        u  <- t + h
        pos <- u - (nrow(X) - nprev)

        if (pos >= 1 && pos <= nprev) {
            save.pred[pos] <- fitobj$hybrid_pred
            save.pca[[pos]]      <- fitobj$pca
            save.n_pc[pos]       <- fitobj$n_pc
            save.pca_vars[[pos]] <- fitobj$pca_vars

            # lasso coefficients as numeric named vector
            lasso.cf <- coef(fitobj$rlasso_model)
            save.lasso.coef[[pos]] <- lasso.cf
            
        }
        cat("iteration", pos, "\n")
    }

    real <- X[, which(colnames(X) == target_name)]
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
         save.lasso.coef = save.lasso.coef)
}