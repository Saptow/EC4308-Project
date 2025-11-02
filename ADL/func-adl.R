
# ============================================
# ADL direct h-step forecast (rolling window)
# ============================================
runARDL = function(Y, X = NULL, indice = 1,
                   h = 1,
                   type = c("fixed", "bic"),
                   p_fixed = 4, 
                   q_fixed = 0,
                   p_max = 4, 
                   q_max = 4,
                   use_x0 = TRUE,
                   search_mode=c('pq', 'q')
                   ) {
  type <- match.arg(type)
  search_mode <- match.arg(search_mode)
  
  lag_start <- if (use_x0) 0 else 1
  
  # data validation
  y <- as.numeric(Y[, indice])
  if (!is.null(X)) X <- as.matrix(X)
  
  shift_vec <- function(v, k) {
    if (k > 0)  c(rep(NA_real_, k), head(v, -k))
    else if (k < 0) c(tail(v, k), rep(NA_real_, -k))  # k negative -> lead
    else v
  }
  
  make_design <- function(p, q) {
    # y lags
    yl <- if (p > 0) {
      mats <- lapply(1:p, function(L) {
        v <- matrix(shift_vec(y, L), ncol = 1)
        colnames(v) <- paste0("L", L, ".y")
        v
      })
      do.call(cbind, mats)
    } else NULL
    
    # X lags 
    if (!is.null(X) && q > 0) {
      xlag_list <- list()
      for (j in seq_len(ncol(X))) {
        xj <- as.numeric(X[, j])
        for (L in lag_start:q) {
          v <- matrix(shift_vec(xj, L), ncol = 1)
          colnames(v) <- paste0("L", L, ".x", j)
          xlag_list[[length(xlag_list) + 1]] <- v
        }
      }
      Xlags <- if (length(xlag_list)) do.call(cbind, xlag_list) else NULL
    } else {
      Xlags <- NULL
    }
    
    y_lead <- shift_vec(y, -h)
    
    Z <- as.data.frame(cbind("y_lead" = y_lead, yl, Xlags))
    Z <- Z[stats::complete.cases(Z), , drop = FALSE]
    
    # build Xout
    get_last_non_na <- function(v) { vv <- v[!is.na(v)]; if (!length(vv)) NA_real_ else tail(vv, 1) }
    Xout <- c(1)
    if (p > 0) for (L in 1:p) Xout <- c(Xout, get_last_non_na(shift_vec(y, L)))
    if (!is.null(X) && q > 0) {
      for (j in seq_len(ncol(X))) {
        xj <- as.numeric(X[, j])
        for (L in lag_start:q) Xout <- c(Xout, get_last_non_na(shift_vec(xj, L)))
      }
    }
    varnames <- c("(Intercept)")
    if (p > 0) for (L in 1:p) varnames <- c(varnames, paste0("L", L, ".y"))
    if (!is.null(X) && q >= 0) {
      for (j in seq_len(ncol(X))) {
        for (L in lag_start:q) varnames <- c(varnames, paste0("L", L, ".x", j))
      }
    }
    names(Xout) <- varnames
    
    list(Z = Z, Xout = matrix(Xout, nrow = 1))
  }
  
  
  # --- Fixed mode ---
  fit_fixed <- function(p, q) {
    dm <- make_design(p, q)
    if (nrow(dm$Z) < (p + 1)) stop("Not enough data for given p, q.")
    model <- stats::lm(y_lead ~ . , data = dm$Z)
    coef  <- stats::coef(model)
    mm_cols <- colnames(stats::model.matrix(model))
    xout_named <- stats::setNames(as.numeric(dm$Xout), mm_cols)  # same order & names
    newdata <- as.data.frame(as.list(xout_named[-1]))            # drop intercept
    pred  <- as.numeric(stats::predict(model, newdata = newdata))
    list(model = model, pred = pred, coef = coef, p = p, q = q,
         bic = stats::BIC(model))  # optional: include BIC for consistency
  }
  
  if (type == "fixed") {
    return(fit_fixed(p_fixed, q_fixed))
  }
  
  # --- BIC search ---
  best <- NULL
  best_bic <- Inf
  
  q_grid <- if (is.null(X)) 0 else {
    if (q_max < lag_start) integer(0) else c(0, seq.int(lag_start, q_max))
  }
  p_seq <- if (search_mode == "q") p_fixed else 1:p_max
  for (p in p_seq) {
    for (q in q_grid) {
      dm <- try(make_design(p, q), silent = TRUE)
      if (inherits(dm, "try-error")) next
      
      # Check min sample size
      min_obs <- max(10, 2 * (p + q * ncol(X) + 1))
      if (nrow(dm$Z) < min_obs) next
      
      model <- try(stats::lm(y_lead ~ . , data = dm$Z), silent = TRUE)
      if (inherits(model, "try-error")) next
      
      bval <- suppressWarnings(stats::BIC(model))
      if (!is.finite(bval)) next
      
      if (bval < best_bic) {
        best_bic <- bval
        
        coef  <- stats::coef(model)
        mm_cols <- colnames(stats::model.matrix(model))
        xout_named <- stats::setNames(as.numeric(dm$Xout), mm_cols)
        newdata <- as.data.frame(as.list(xout_named[-1]))
        pred  <- as.numeric(stats::predict(model, newdata = newdata))
        
        best <- list(model = model, pred = pred, coef = coef,
                     p = p, q = q, bic = bval)
      }
    }
  }
  
  if (is.null(best)) {
    stop("BIC search failed.")
  }
  best
}

## ------------------------------------------------
# Rolling Window Helper Function 
## ------------------------------------------------

ardl.rolling.window = function(Y, X = NULL,
                               nprev,
                               indice = 1,
                               h = 1,
                               type = c("fixed", "bic"),
                               p_fixed = 4, q_fixed = 0,
                               p_max = 4, q_max = 4,
                               use_x0 = TRUE,
                               verbose = TRUE,
                               search_mode=c("pq","q")
                               ) {
  # Parse arguments
  type <- match.arg(type)
  search_mode <- match.arg(search_mode)
  
  nprev_eff <- nprev - (h - 1)
  if (nprev_eff <= 0) stop("nprev must be >= h")
  
  coef_list <- vector("list", nprev_eff)
  pred_vec  <- rep(NA_real_, nprev_eff)
  
  p_used  <- integer(nprev_eff)
  q_used  <- integer(nprev_eff)
  bic_used <- rep(NA_real_, nprev_eff)
  
  N <- nrow(Y)
  pos <- 1
  for (i in seq(nprev, h, by = -1)) {
    Y.window <- Y[(1 + nprev - i):(N - i), , drop = FALSE]
    X.window <- if (is.null(X)) NULL else X[(1 + nprev - i):(N - i), , drop = FALSE]
    
    fit <- runARDL(Y.window, X.window, indice = indice,
                   h = h,
                   type = type,
                   p_fixed = p_fixed, 
                   q_fixed = q_fixed,
                   p_max = p_max, 
                   q_max = q_max,
                   use_x0 = use_x0,
                   search_mode=search_mode
                   )
    if (verbose) {
      cat(sprintf("iteration %d: p=%d, q=%d, BIC=%.2f\n", 
                  pos, fit$p, fit$q, fit$bic))
    }
    
    coef_list[[pos]] <- fit$coef
    pred_vec[pos]    <- fit$pred
    
    p_used[pos] <- fit$p
    q_used[pos] <- fit$q
    bic_used[pos] <- if (!is.null(fit$bic)) fit$bic else NA_real_
    
    pos <- pos + 1
  }
  
  real   <- as.numeric(Y[, indice])
  y_true <- tail(real, nprev_eff)
  
  rmse <- sqrt(mean((y_true - pred_vec)^2))
  mae  <- mean(abs(y_true - pred_vec))
  errors <- c(rmse = rmse, mae = mae)
  
  all_names <- unique(unlist(lapply(coef_list, names)))
  coef_mat <- matrix(NA_real_, nprev_eff, length(all_names), dimnames = list(NULL, all_names))
  for (k in seq_len(nprev_eff)) {
    if (is.null(coef_list[[k]])) next
    coef_k <- coef_list[[k]]
    coef_mat[k, names(coef_k)] <- coef_k
  }
  
  list(
    pred = as.matrix(pred_vec),
    coef = coef_mat,
    errors = errors,
    meta = list(
      h = h, type = type,
      p_fixed = p_fixed, q_fixed = q_fixed,
      p_max = p_max, q_max = q_max, use_x0 = use_x0,
      p_chosen = p_used,
      q_chosen = q_used,
      bic = bic_used
    )
  )
}
