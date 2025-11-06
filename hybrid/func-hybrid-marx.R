# -----------------------------------------------------
# Run function for hybrid method (rLASSO + RF) for MARX 
# Expects a data matrix Y as well as desired lags for Y, and options for marx
# -----------------------------------------------------

runhybrid_marx <- function(X, h=1, L_y=4, target_name="UNRATE", P_marx=4){
    L_y <- L_y
    P_marx <- P_marx

    # Initial data cleaning
    X <- subset(X, select= -date) # remove date column if present
    X_in  <- X[-nrow(X), , drop=FALSE]
    X_out <- X[nrow(X), , drop=FALSE]

    dum_idx <- which(colnames(X) == "aft_break")
    ind     <- which(colnames(X) == target_name)

    X_train_raw <- as.matrix(X_in[, setdiff(seq_len(ncol(X_in)), c(ind, dum_idx)), drop=FALSE])
    source("./data_transformation/marx_transform.R")
    # Assert numeric X_train_raw
    X_train_raw <- apply(X_train_raw, 2, as.numeric)
    mx <- marx_transform(X_train_raw, n_lag = P_marx, scale_data = FALSE)
    X_marx <- mx$mat_x_marx # shd hv T_in - P_marx rows

    y_in <- as.numeric(X_in[, ind, drop=FALSE])
    T_in <- nrow(X_in)

    # get valid time indices
    t_start <- max(P_marx + 1, L_y + 1)
    t_end   <- T_in - h
    if (t_end < t_start) stop("Window too short for chosen h/L_y/P_marx.")
    t_idx <- t_start:t_end

    # map to MARX rows
    marx_rows <- t_idx - P_marx

    if (L_y > 0) {
        y_embed <- embed(y_in, L_y + 1) # lag y
        y_lags  <- y_embed[, -1, drop=FALSE]
        y_rows  <- t_idx - L_y
        y_lags_aligned <- y_lags[y_rows, , drop=FALSE]
        colnames(y_lags_aligned) <- paste0("y_L", 1:L_y) # label y_lags
    } else {
        y_lags_aligned <- NULL
    }

    dum_t <- as.numeric(X_in[t_idx, dum_idx, drop=TRUE]) # assert numeric dummy at t
    y_target <- y_in[t_idx + h] # target variable

    # Final design matrix for training (dont impute NA yet)
    X_train_df <- cbind(
        if (!is.null(y_lags_aligned)) as.data.frame(y_lags_aligned) else NULL,
        as.data.frame(X_marx[marx_rows, , drop=FALSE], check.names=FALSE),
        DUM = dum_t
    )

    # Drop NA rows
    non_dummy_cols <- setdiff(colnames(X_train_df), "DUM") # get non-dummy cols
    X_all_mat <- as.matrix(X_train_df[, c(non_dummy_cols, "DUM"), drop=FALSE])
    valid_rows <- complete.cases(X_all_mat) & is.finite(y_target)

    X_train_df <- X_train_df[valid_rows, , drop=FALSE] # valid rows only
    y_target   <- y_target[valid_rows] # valid target only

    # Build X_new for forecasting y_{T_in + h}
    if ((T_in - P_marx) < 1 || (T_in - P_marx) > nrow(X_marx)) {
        stop("Cannot form X_new: window too short relative to P_marx.")
    }

    X_new_marx <- X_marx[T_in - P_marx, , drop=FALSE]
    if (L_y > 0) {
        y_lags_new <- rev(y_in[(T_in - L_y):(T_in - 1)])
        names(y_lags_new) <- paste0("y_L", 1:L_y)
    } else {
        y_lags_new <- NULL
    }
    DUM_new <- as.numeric(X_out[, dum_idx, drop = TRUE])

    X_new_df <- as.data.frame(cbind(
        if (!is.null(y_lags_new)) t(y_lags_new) else NULL,
        X_new_marx,
        DUM = DUM_new
    ), check.names = FALSE)

    # standardise for LASSO (except dummy)
    non_dummy_cols <- setdiff(colnames(X_train_df), "DUM")

    # Ensure numeric matrices for standardisation
    X_non_train <- as.matrix(X_train_df[, non_dummy_cols, drop=FALSE])
    X_non_new   <- as.matrix(X_new_df[,   non_dummy_cols, drop=FALSE])

    # compute means and sds
    means <- colMeans(X_non_train)
    sds <- apply(X_non_train, 2, sd)
    sds[!is.finite(sds) | sds == 0] <- 1  # avoid division by zero

    X_train_mat <- cbind(
        sweep(sweep(X_non_train, 2, means, "-"), 2, sds, "/"), # standardise
        DUM = as.numeric(X_train_df[["DUM"]])
    )
    colnames(X_train_mat)[ncol(X_train_mat)] <- "DUM" # retain dummy name

    newx <- cbind(
        sweep(sweep(X_non_new, 2, means, "-"), 2, sds, "/"), # standardise
        DUM = as.numeric(X_new_df[["DUM"]])
    )

    # Drop any all-constant columns or zero variance columns
    keep <- which(colSums(abs(X_train_mat)) > 0)
    X_train_mat <- X_train_mat[, keep, drop=FALSE]
    newx <- newx[, keep, drop=FALSE]

    # First stage: fit rLASSO to get predictions and residuals
    rlasso.fit <- rlasso(as.matrix(X_train_mat), y_target, post=FALSE) # assume rlasso pkg loaded
    rhat.rlasso <- rlasso.fit$residuals
    rlasso.pred <- predict(rlasso.fit, newdata=as.matrix(newx)) # predict at newx for combining later

    # Second Stage: Fit RF on rLASSO residuals
    df_rf_train <- cbind.data.frame(rhat=rhat.rlasso, X_train_mat)
    rf_resid <- ranger(
        dependent.variable.name = "rhat",
        data = df_rf_train,
        max.depth = 5,
        mtry = floor(ncol(X_train_mat) / 3), # p/3 for regression
        importance = "permutation"
    )
    rf_resid_pred <- predict(rf_resid, data = as.data.frame(newx))$predictions

    # Combine rLASSO and RF residual predictions
    final_pred <- as.numeric(rlasso.pred) + as.numeric(rf_resid_pred)

    list(
        rlasso_model = rlasso.fit, 
        rf_resid_model = rf_resid,
        hybrid_pred = final_pred
    )

}

hybrid_marx.rolling.window <- function(X, nprev, h=1, target_name = "UNRATE", P_marx=4) {
    # Set up default variables 
    save.pred <- rep(NA_real_, nprev)
    save.marx      <- vector("list", nprev)
    save.lasso.coef   <- vector("list", nprev)  # store sparse coef safely

    for (i in nprev:max(h,1)) {
        X.window <- X[(1 + nprev - i):(nrow(X) - i), , drop = FALSE]
        fitobj <- runhybrid_marx(X.window, h = h, target_name = target_name, P_marx = P_marx)

        t  <- nrow(X) - i
        u  <- t + h
        pos <- u - (nrow(X) - nprev)

        if (pos>=1 && pos <= nprev) {
            save.pred[pos] <- fitobj$hybrid_pred
            # lasso coeffs as numeric named vector
            lasso.cf <- coef(fitobj$rlasso_model)
            save.lasso.coef[[pos]] <- lasso.cf
        }
        cat("Completed iteration", pos, "of", nprev, "\n")
    }

    real <- X[, which(colnames(X) == target_name)] # actual values
    # validate numeric vector real
    real <- as.numeric(real)

    # calculate rmse and mae
    y_test_full <- tail(real, nprev)
    pred_full <- save.pred
    valid <- !is.na(pred_full)
    y_test <- y_test_full[valid]
    pred <- pred_full[valid]

    rmse <- sqrt(mean((y_test - pred)^2))
    mae  <- mean(abs(y_test - pred))
    errs <- c(rmse = rmse, mae = mae, n_effective = sum(valid))
    return(
        list(
            pred = save.pred,
            errors= errs,
            save.lasso.coef = save.lasso.coef
        )
    )
}


