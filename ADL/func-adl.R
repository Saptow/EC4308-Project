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
      # prebuilt features (MAF/MARX) – assume already aligned/padded
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
  model <- stats::lm(y_lead ~ . , data = dm$Z)
  # last predictor row (drop target)
  last_pred <- as.data.frame(dm$Z[nrow(dm$Z), setdiff(colnames(dm$Z), "y_lead"), drop = FALSE])
  pred  <- as.numeric(stats::predict(model, newdata = last_pred))
  list(model = model, pred = pred, coef = stats::coef(model),
       p = p, q = q, bic = stats::BIC(model))
}

# ============================================
# ADL direct h-step forecast (rolling window)
# ============================================
runARDL <- function(Y, X = NULL, indice = 1,
                    h = 1,
                    type = c("bic","fixed","maf","marx"),
                    p_max = 4,
                    p_fixed = 4,
                    q_max = 4,
                    q_fixed = 4,
                    use_x0 = TRUE,
                    search_mode = c("pq","q"),
                    P_maf = 4,
                    marx_q = 4) {
  
  type <- match.arg(type)
  search_mode <- match.arg(search_mode)
  lag_start <- if (use_x0) 0 else 1
  
  y <- as.numeric(Y[, indice])
  if (!is.null(X)) X <- as.matrix(X)
  
  if (type == "fixed") {
    return(fit_fixed(y, X, p_fixed, q_fixed, h))
  }

  if (type == "maf") {
    source("./data_transformation/maf_transform.R")
    dum <- X[, "aft_break", drop = FALSE]
    X_wo <- X[, colnames(X) != "aft_break", drop = FALSE]
    X_maf <- maf_transform(X_wo, P_maf = P_maf, scale_data = FALSE)
    # align dummy and pad to y length
    dum_aligned <- dum[-seq_len(P_maf - 1), , drop = FALSE]
    X_maf <- cbind(aft_break = dum_aligned[,1], X_maf)
    X_maf <- pad_top_na(X_maf, length(y))
    # # Final checks for X_maf and y
    # cat(sprintf("MAF X rows: %d, Y length: %d\n",
    #              nrow(X_maf), length(y)))
    # if (nrow(X_maf) != length(y)) {
    #   stop(sprintf("X and Y row mismatch after padding: %d vs %d",
    #                nrow(X_maf), length(y)))
    # }
    return(fit_fixed(y, X_maf, p = 0, q = 0, h = h))
  }
  
  if (type == "marx") {
    source("./data_transformation/marx_transform.R")
    dum <- X[, "aft_break", drop = FALSE]
    X_wo <- X[, colnames(X) != "aft_break", drop = FALSE]
    mx <- marx_transform(X_wo, n_lag = marx_q, scale_data = FALSE)
    X_marx <- if (is.list(mx)) mx$mat_x_marx else mx
    dum_aligned <- dum[-seq_len(marx_q - 1), , drop = FALSE]
    X_marx <- cbind(aft_break = dum_aligned[,1], X_marx)
    X_marx <- pad_top_na(X_marx, length(y))
    return(fit_fixed(y, X_marx, p = 0, q = 0, h = h))
  }
  
  if (type == "bic") {
    best <- NULL; best_bic <- Inf
    q_grid <- if (is.null(X)) 0 else {
      if (q_max < lag_start) integer(0) else c(0, seq.int(lag_start, q_max))
    }
    p_seq <- if (search_mode == "q") p_fixed else 1:p_max
    Kx <- if (is.null(X)) 0 else ncol(X)
    
    for (p in p_seq) {
      for (q in q_grid) {
        dm <- try(make_design(y, X, p, q, h), silent = TRUE)
        if (inherits(dm, "try-error")) next
        
        # crude min sample size rule
        min_obs <- max(10, 2 * (p + q * Kx + 1))
        if (nrow(dm$Z) < min_obs) next
        
        model <- try(stats::lm(y_lead ~ . , data = dm$Z), silent = TRUE)
        if (inherits(model, "try-error")) next
        
        bval <- suppressWarnings(stats::BIC(model))
        if (!is.finite(bval)) next
        
        if (bval < best_bic) {
          last_pred <- as.data.frame(dm$Z[nrow(dm$Z),
                                          setdiff(colnames(dm$Z), "y_lead"), drop = FALSE])
          pred <- as.numeric(stats::predict(model, newdata = last_pred))
          best <- list(model = model, pred = pred, coef = stats::coef(model),
                       p = p, q = q, bic = bval)
          best_bic <- bval
        }
      }
    }
    if (is.null(best)) stop("BIC search failed.")
    return(best)
  }
}

# Rolling Window
ardl.rolling.window <- function(Y, X = NULL,
                                nprev,
                                indice = 1,
                                h = 1,
                                type = c("fixed", "bic", "maf", "marx"),
                                p_fixed = 4, q_fixed = 0,
                                p_max = 4, q_max = 4,
                                use_x0 = TRUE,
                                verbose = TRUE,
                                search_mode = c("pq","q"),
                                P_maf = 4,
                                marx_q = 4) {
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

    # # Check X 
    # if (!is.null(X.window)) {
    #   cat(sprintf("Window %d:%d - Y rows: %d, X rows: %d\n",
    #               y_idx_start, y_idx_end,
    #               nrow(Y.window), nrow(X.window)))
    #   if (nrow(X.window) != nrow(Y.window)) {
    #     stop(sprintf("X and Y row mismatch in window: %d vs %d",
    #                  nrow(X.window), nrow(Y.window)))
    #   }
    # }
    
    # Fit ADL model
    fit <- try(
      runARDL(
        Y.window, X.window, indice = indice,
        h = h,
        type = type,
        p_fixed = p_fixed, q_fixed = q_fixed,
        p_max = p_max,   q_max = q_max,
        use_x0 = use_x0,
        search_mode = search_mode,
        P_maf = P_maf,
        marx_q = marx_q
      ),
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
      bic_used[pos]    <- if (!is.null(fit$bic) && is.finite(fit$bic)) fit$bic else NA_real_
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
      p_max = p_max, q_max = q_max, use_x0 = use_x0,
      P_maf = P_maf, marx_q = marx_q,
      p_chosen = p_used,
      q_chosen = q_used,
      bic = bic_used
    )
  )
}