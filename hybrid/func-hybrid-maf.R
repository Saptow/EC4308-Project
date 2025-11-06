# ------------------------------------------
# Run function for hybrid method (rLASSO + RF)
# Expects a data matrix X as well as desired lags for Y and X, and options for maf and marx
# ------------------------------------------
runhybrid_maf <- function(X, h=1, L_y=4, P_maf=4, target_name="UNRATE"){
    L_y <- L_y
    P_maf <- P_maf
    # Initial data cleaning
    X <- subset(X, select= -date) # remove date column if present
    X_in  <- X[-nrow(X), , drop=FALSE]
    X_out <- X[nrow(X), , drop=FALSE]

    dum_idx <- which(colnames(X) == "aft_break")
    ind     <- which(colnames(X) == target_name)

    X_train_raw <- as.matrix(X_in[, setdiff(seq_len(ncol(X_in)), c(ind, dum_idx)), drop=FALSE])
    source("./data_transformation/maf_transform.R")
    # Assert numeric X_train_raw
    X_train_raw <- apply(X_train_raw, 2, as.numeric)
    maf_train <- maf_transform(X_train_raw, P_maf = P_maf, scale_data = TRUE)
    y_in <- as.numeric(X_in[, ind, drop=FALSE])
    T_in <- nrow(X_in)

    t_start <- P_maf + 1
    t_end   <- T_in - h
    if (t_end < t_start) stop("Window too short for chosen h/L_y/P_maf.")
    t_idx <- t_start:t_end

    maf_rows <- t_idx - P_maf

    if (L_y > 0) {
    y_embed <- embed(y_in, L_y + 1)
    y_lags  <- y_embed[, -1, drop=FALSE]
    y_rows  <- t_idx - L_y
    y_lags_aligned <- y_lags[y_rows, , drop=FALSE]
    colnames(y_lags_aligned) <- paste0("y_L", 1:L_y)
    } else {
    y_lags_aligned <- NULL
    }

    # Dummy at t
    dum_t <- as.numeric(X_in[t_idx, dum_idx, drop=TRUE])

    # Target variable
    y_target <- y_in[t_idx + h]
    # Final design matrix for training
    X_train_df <- cbind(
        if (!is.null(y_lags_aligned)) as.data.frame(y_lags_aligned) else NULL,
        as.data.frame(maf_train[maf_rows, , drop=FALSE], check.names=FALSE),
        DUM = dum_t
    )

    # Build X_new for forecasting y_{T_in + h}
    if ((T_in - P_maf) < 1 || (T_in - P_maf) > nrow(maf_train)) {
        stop("Cannot form X_new: window too short relative to P_maf.")
    }
    X_new_maf <- maf_train[T_in - P_maf, , drop=FALSE]
    if (L_y > 0) {
        y_lags_new <- rev(y_in[(T_in - L_y):(T_in - 1)])
        names(y_lags_new) <- paste0("y_L", 1:L_y)
    } else {
        y_lags_new <- NULL
    }
    DUM_new <- as.numeric(X_out[, dum_idx, drop = TRUE])
    
    X_new_df <- as.data.frame(cbind(
        if (!is.null(y_lags_new)) t(y_lags_new) else NULL,
        X_new_maf,
        DUM = DUM_new
    ), check.names = FALSE)
    

    # 5) Standardise based on training set (except dummy)
    dum_name <- colnames(X_in)[dum_idx]
    non_dummy_cols <- setdiff(colnames(X_train_df), dum_name)
    
    X_non_train <- as.matrix(X_train_df[, non_dummy_cols, drop = FALSE])
    X_non_new   <- as.matrix(X_new_df[,   non_dummy_cols, drop = FALSE])
    means <- colMeans(X_non_train, na.rm = TRUE)
    sds   <- apply(X_non_train, 2, sd)
    sds[!is.finite(sds) | sds == 0] <- 1  # avoid division by zero
    
    X_train_mat <- cbind(
        sweep(sweep(X_non_train, 2, means, "-"), 2, sds, "/"),
        DUM = as.numeric(X_train_df[[dum_name]])
    )
    colnames(X_train_mat)[ncol(X_train_mat)] <- dum_name
    
    newx <- cbind(
        sweep(sweep(X_non_new, 2, means, "-"), 2, sds, "/"),
        DUM = as.numeric(X_new_df[[dum_name]])
    )
    colnames(newx)[ncol(newx)] <- dum_name
    
    # Drop all-constant columns
    keep <- which(colSums(abs(X_train_mat)) > 0)
    X_train_mat <- X_train_mat[, keep, drop = FALSE]
    newx        <- newx[,        keep, drop = FALSE]

    
    # First stage: fit rLASSO to get predictions and residuals
    rlasso.fit <- rlasso(as.matrix(X_train_mat), y_target, post=FALSE)
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
    hybrid_pred = hybrid_pred
    )
}

hybrid_maf.rolling.window <- function(X, nprev, h=1, P_maf=4, target_name = "UNRATE") {
    # Set up default variables 
    save.pred <- rep(NA_real_, nprev)
    save.pca      <- vector("list", nprev)
    save.n_pc     <- integer(nprev)
    save.pca_vars <- vector("list", nprev)
    save.lasso.coef    <- vector("list", nprev)  # store sparse coef safely

    for (i in nprev:max(h,1)) {
        X.window <- X[(1 + nprev - i):(nrow(X) - i), , drop = FALSE]
        fitobj <- runhybrid_maf(X.window, h = h, P_maf = P_maf, target_name = target_name)

        t  <- nrow(X) - i
        u  <- t + h
        pos <- u - (nrow(X) - nprev)

        if (pos >= 1 && pos <= nprev) {
            save.pred[pos] <- fitobj$hybrid_pred

            # lasso coefficients as numeric named vector
            lasso.cf <- coef(fitobj$rlasso_model)
            save.lasso.coef[[pos]] <- lasso.cf
            
        }
        cat("Completed iteration", pos, "of", nprev, "\n")
    }

    real <- X[, which(colnames(X) == target_name)]
    # Validate that real is a vector and numeric
    real <- as.numeric(real)
    y_test_full <- tail(real, nprev)
    pred_full   <- save.pred
    valid     <- !is.na(pred_full)
    y_test <- y_test_full[valid]
    pred      <- pred_full[valid]

    rmse <- sqrt(mean((y_test - pred)^2))
    mae  <- mean(abs(y_test - pred))
    errors <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
    list(pred = save.pred, errors = errors,
         save.lasso.coef = save.lasso.coef)
}