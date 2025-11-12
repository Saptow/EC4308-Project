#============================================
# Helper functions 
#============================================
# pad feature matrix to match target length by adding NA rows on top
pad_top_na <- function(M, target_n) {
  if (is.null(M)) return(M)
  n <- nrow(M)
  if (n >= target_n) return(M)
  pad <- matrix(NA_real_, nrow = target_n - n, ncol = ncol(M))
  colnames(pad) <- colnames(M)
  rbind(pad, M)
}

make_design <- function(y_raw, X, p, q, h, dummy_name = "aft_break") {
  # y-lags (do not overwrite raw series)
  y_lags <- if (p > 0) {
    yl <- embed(y_raw, p + 1)
    colnames(yl) <- c("L0.y", paste0("L", 1:p, ".y"))
    rbind(matrix(NA_real_, nrow = p, ncol = ncol(yl)), yl)
  } else {
    matrix(, nrow = length(y_raw), ncol = 0)
  }
  
  # direct h-step target
  y_lead <- c(y_raw[(1 + h):length(y_raw)], rep(NA_real_, h))
  
  # X block
  if (!is.null(X)) {
    if (q > 0) {
      stopifnot(dummy_name %in% colnames(X))
      X_wo <- X[, setdiff(colnames(X), dummy_name), drop = FALSE]
      tmp  <- embed(as.matrix(X_wo), q + 1)  # (q+1)*K
      K    <- ncol(X_wo)
      laglabs <- paste0("L", 0:q)
      base <- rep(colnames(X_wo), each = q + 1)
      lags <- rep(laglabs, times = K)
      colnames(tmp) <- paste0(base, "_", lags)
      x_lags <- rbind(matrix(NA_real_, nrow = q, ncol = ncol(tmp)), tmp)
      X_block <- cbind(setNames(X[, dummy_name, drop = FALSE], dummy_name), x_lags)
    } else {
      X_block <- as.matrix(X)
    }
  } else {
    X_block <- matrix(, nrow = length(y_raw), ncol = 0)
  }
  
  Z <- as.data.frame(cbind(y_lead = y_lead, y_lags, X_block))
  Z <- Z[stats::complete.cases(Z), , drop = FALSE]
  list(Z = Z)
}

fit_fixed <- function(y, X, p, q, h) {
  dm <- make_design(y, X, p, q, h)
  if (nrow(dm$Z) < (p + 1)) stop("Not enough data for given p, q.")
  model <- lm(y_lead ~ . , data = dm$Z)
  # last predictor row (drop target)
  last_pred <- as.data.frame(dm$Z[nrow(dm$Z), setdiff(colnames(dm$Z), "y_lead"), drop = FALSE])
  pred  <- as.numeric(predict(model, newdata = last_pred))
  list(model = model, pred = pred, coef = coef(model),
       p = p, q = q, bic = BIC(model))
}

# ============================================
# ADL direct h-step forecast (rolling window)
# ============================================
runARDL <- function(Y, X = NULL, indice = 1,
                    h = 1,
                    type = c("fixed"),
                    p_max = 4,
                    p_fixed = 4,
                    q_max = 4,
                    q_fixed = 4,
                    use_x0 = TRUE,
                    search_mode = c("pq","q"),
                    P_maf = 4,
                    marx_q = 4) {
  
  type <- match.arg(type)
  lag_start <- if (use_x0) 0 else 1
  
  y <- as.numeric(Y[, indice])
  if (!is.null(X)) X <- as.matrix(X)
  
  if (type == "fixed") {
    return(fit_fixed(y, X, p_fixed, q_fixed, h))
  }
}

# Rolling Window
ardl.rolling.window <- function(Y, X = NULL,
                                nprev,
                                indice = 1,
                                h = 1,
                                type = c("fixed"),
                                p_fixed = 4, q_fixed = 0,
                                use_x0 = TRUE,
                                verbose = TRUE,
                                search_mode = c("pq","q")) {
  # Parse args
  type <- match.arg(type)
  search_mode <- match.arg(search_mode)

  # Effective number of rolling evaluations for direct h-step
  nprev_eff <- nprev - (h - 1)
  if (nprev_eff <= 0) stop("nprev must be >= h")

  N <- nrow(Y)

  # Storage
  coef_list <- vector("list", nprev_eff)
  pred_vec  <- rep(NA_real_, nprev_eff)
  p_used    <- rep(NA_integer_, nprev_eff)
  q_used    <- rep(NA_integer_, nprev_eff)
  bic_used  <- rep(NA_real_,     nprev_eff)

  pos <- 1
  for (i in seq(nprev, h, by = -1)) {
    # Rolling window: expand-to-last with shrinking holdout
    # Y[(1 + nprev - i) : (N - i)]
    y_idx_start <- 1 + nprev - i
    y_idx_end   <- N - i
    Y.window <- Y[y_idx_start:y_idx_end, , drop = FALSE]
    X.window <- if (is.null(X)) NULL else X[y_idx_start:y_idx_end, , drop = FALSE]
    
    # Fit ADL model
    fit <- try(
      runARDL(
        Y.window, X.window, indice = indice,
        h = h,
        type = type,
        p_fixed = p_fixed, q_fixed = q_fixed,
        use_x0 = use_x0),
      silent = TRUE
    )

    if (inherits(fit, "try-error")) {
      if (verbose) {
        cat(sprintf("iteration %d: (window %d:%d) fit failed: %s\n",
                    pos, y_idx_start, y_idx_end, as.character(fit)))
      }
      # Leave NAs for this iteration and continue
    } else {
      if (verbose) {
        cat(sprintf("iteration %d: p=%s, q=%s, BIC=%s\n",
                    pos,
                    if (!is.null(fit$p)) fit$p else NA_integer_,
                    if (!is.null(fit$q)) fit$q else NA_integer_,
                    if (!is.null(fit$bic) && is.finite(fit$bic)) sprintf("%.2f", fit$bic) else "NA"))
      }
      coef_list[[pos]] <- fit$coef
      pred_vec[pos]    <- fit$pred
      p_used[pos]      <- if (!is.null(fit$p)) fit$p else NA_integer_
      q_used[pos]      <- if (!is.null(fit$q)) fit$q else NA_integer_
    }

    pos <- pos + 1
  }

  # Targets to evaluate against: last nprev_eff observations of chosen series
  real   <- as.numeric(Y[, indice])
  y_true <- tail(real, nprev_eff)

  # Errors (ignore NAs if any)
  rmse <- sqrt(mean((y_true - pred_vec)^2, na.rm = TRUE))
  mae  <- mean(abs(y_true - pred_vec), na.rm = TRUE)
  errors <- c(rmse = rmse, mae = mae)

  # Stack coefficients (union of names)
  all_names <- unique(unlist(lapply(coef_list, names)))
  coef_mat <- matrix(NA_real_, nprev_eff, length(all_names),
                     dimnames = list(NULL, all_names))
  for (k in seq_len(nprev_eff)) {
    ck <- coef_list[[k]]
    if (length(ck)) coef_mat[k, names(ck)] <- ck
  }

  list(
    pred = as.matrix(pred_vec),
    coef = coef_mat,
    errors = errors,
    meta = list(
      h = h, type = type,
      p_fixed = p_fixed, q_fixed = q_fixed,
      use_x0 = use_x0
    )
  )
}