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
# Clean X for PCA: remove all-NA cols, replace non-finite with col means, drop zero-variance cols
clean_for_pca <- function(X) {
  if (is.null(X) || !ncol(X)) return(X)
  # Drop all-NA columns
  X <- X[, colSums(is.finite(as.matrix(X))) > 0, drop = FALSE]
  if (!ncol(X)) return(X)
  # Replace non-finite values with column means
  X[] <- lapply(X, function(v) {
    v[!is.finite(v)] <- mean(v[is.finite(v)], na.rm = TRUE)
    v
  })
  # Drop zero-variance columns
  keep <- apply(X, 2, function(v) sd(v, na.rm = TRUE) > 0)
  X[, keep, drop = FALSE]
  # echo dropped columns
  if (any(!keep)) {
    dropped <- colnames(X)[!keep]
    cat(paste0("Dropped zero-variance columns for PCA: ", paste(dropped, collapse = ", ")))
    cat("\n")
  }
}

make_design <- function(y_raw, X, p, q, h, 
                        dummy_name = "aft_break",
                        x_dimred = c("none","pca"),
                        pca_var = 0.90,
                        pca_cap = 50) {
  x_dimred <- match.arg(x_dimred)

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

  pca_obj <- NULL; n_pc <- NA_integer_; pca_vars <- NULL

  # X block
  if (!is.null(X)) {
    stopifnot(dummy_name %in% colnames(X))
    if (q > 0) { # 
      dum_col <- X[, dummy_name, drop = FALSE]
      X_wo    <- X[, setdiff(colnames(X), dummy_name), drop = FALSE]

      if (x_dimred == "pca" && ncol(X_wo) > 0) {
        # Train PCA on the current window (center/scale)
        X_wo <- clean_for_pca(X_wo)
        X_sc   <- scale(X_wo, center = TRUE, scale = TRUE)
        max_pc <- min(ncol(X_sc), nrow(X_sc) - 1)
        if (is.finite(max_pc) && max_pc >= 1) {
          pca_obj <- prcomp(X_sc, center = FALSE, scale. = FALSE, rank. = max_pc)
          varexp  <- pca_obj$sdev^2 / sum(pca_obj$sdev^2)
          n_pc    <- which(cumsum(varexp) >= pca_var)[1]
          if (is.na(n_pc)) n_pc <- 1
          n_pc <- min(n_pc, pca_cap)

          pcs <- predict(pca_obj, newdata = X_sc)[, seq_len(n_pc), drop = FALSE]
          colnames(pcs) <- paste0("PC", seq_len(n_pc))
          pca_vars <- colnames(X_wo)
          X_use <- pcs
        } else {
          # fallback to raw X if PCA rank is degenerate
          X_use <- X_wo
        }
      } else {
        X_use <- X_wo
      }

      # lag the chosen X representation
      tmp  <- embed(as.matrix(X_use), q + 1)  # (q+1)*K'
      Kp   <- ncol(X_use)
      laglabs <- paste0("L", 0:q)
      base <- rep(colnames(X_use), each = q + 1)
      lags <- rep(laglabs, times = Kp)
      colnames(tmp) <- paste0(base, "_", lags)
      x_lags <- rbind(matrix(NA_real_, nrow = q, ncol = ncol(tmp)), tmp)

      # contemporaneous dummy (L0 only) stays unlagged in the design
      X_block <- cbind(setNames(dum_col, dummy_name), x_lags)

    } else {
      # prebuilt features (MAF/MARX) – assume already aligned/padded
      X_block <- as.matrix(X)
    }
  } else {
    X_block <- matrix(, nrow = length(y_raw), ncol = 0)
  }

  Z <- as.data.frame(cbind(y_lead = y_lead, y_lags, X_block))
  Z <- Z[complete.cases(Z), , drop = FALSE] # check for NA rows
  list(Z = Z, pca = pca_obj, n_pc = n_pc, pca_vars = pca_vars)
}

fit_fixed <- function(y, X, p, q, h,
                      x_dimred = c("none","pca"),
                      pca_var = 0.90,
                      pca_cap = 50,
                      dummy_name = "aft_break") {
  x_dimred <- match.arg(x_dimred)
  dm <- make_design(y, X, p, q, h, dummy_name = dummy_name,
                    x_dimred = x_dimred, pca_var = pca_var, pca_cap = pca_cap)
  if (nrow(dm$Z) < (p + 1)) stop("Not enough data for given p, q.")
  model <- lm(y_lead ~ . , data = dm$Z)

  # last predictor row (drop target)
  last_pred <- as.data.frame(dm$Z[nrow(dm$Z), setdiff(colnames(dm$Z), "y_lead"), drop = FALSE])
  pred  <- as.numeric(predict(model, newdata = last_pred))

  list(model = model, coef = coef(model), pred = pred,
       p = p, q = q, h=h,
       pca = dm$pca, n_pc = dm$n_pc, pca_vars = dm$pca_vars)
}

# ============================================
# ADL direct h-step forecast (rolling window)
# ============================================
runARDL <- function(Y, X = NULL, indice = 1,
                    h = 1,
                    type = c("fixed","maf","marx"),
                    p_fixed = 4,
                    q_fixed = 4,
                    use_x0 = TRUE,
                    P_maf = 4,
                    marx_q = 4,
                    x_dimred = c("none","pca"),
                    pca_var = 0.90,
                    pca_cap = 50,
                    dummy_name = "aft_break") {

  type <- match.arg(type)
  x_dimred <- match.arg(x_dimred)

  y <- as.numeric(Y[, indice])
  if (!is.null(X)) X <- as.matrix(X)

  if (type == "fixed") {
    return(fit_fixed(
      y, X, p_fixed, q_fixed, h,
      x_dimred = x_dimred, pca_var = pca_var, pca_cap = pca_cap,
      dummy_name = dummy_name
    ))
  }

  if (type == "maf") {
    source("./data_transformation/maf_transform.R")
    dum <- X[, dummy_name, drop = FALSE]
    X_wo <- X[, colnames(X) != dummy_name, drop = FALSE]
    X_maf <- maf_transform(X_wo, P_maf = P_maf, scale_data = TRUE)
    dum_aligned <- dum[-seq_len(P_maf - 1), , drop = FALSE]
    X_maf  <- cbind(tmp_dummy = dum_aligned[,1], X_maf)
    colnames(X_maf)[1] <- dummy_name
    X_maf <- pad_top_na(X_maf, length(y))
    return(fit_fixed(y, X_maf, p = 0, q = 0, h = h,
                     x_dimred = "none", dummy_name = dummy_name))
  }

  if (type == "marx") {
    source("./data_transformation/marx_transform.R")
    dum <- X[, dummy_name, drop = FALSE]
    X_wo <- X[, colnames(X) != dummy_name, drop = FALSE]
    mx <- marx_transform(X_wo, n_lag = marx_q, scale_data = FALSE)
    X_marx <- mx$mat_x_marx
    dum_aligned <- dum[-seq_len(marx_q - 1), , drop = FALSE]
    X_marx <- cbind(tmp_dummy = dum_aligned[,1], X_marx)
    colnames(X_marx)[1] <- dummy_name
    X_marx <- pad_top_na(X_marx, length(y))
    return(fit_fixed(y, X_marx, p = 0, q = 0, h = h,
                     x_dimred = "none", dummy_name = dummy_name))
  }
}



# Rolling Window
ardl.rolling.window <- function(Y, X = NULL,
                                nprev,
                                indice = 1,
                                h = 1,
                                type = c("fixed", "maf", "marx"),
                                x_dimred = c("none","pca"),
                                p_fixed = 4, q_fixed = 4,
                                use_x0 = TRUE,
                                verbose = TRUE,
                                P_maf = 4,
                                marx_q = 4) {
  # Parse args
  type <- match.arg(type)
  x_dimred <- match.arg(x_dimred)
  # Effective number of rolling evaluations for direct h-step
  nprev_eff <- nprev - (h - 1)
  if (nprev_eff <= 0) stop("nprev must be >= h")

  N <- nrow(Y)


  # Ensure all numeric

  # Storage
  coef_list <- vector("list", nprev_eff)
  pred_vec  <- rep(NA_real_, nprev_eff)
  p_used    <- rep(NA_integer_, nprev_eff)
  q_used    <- rep(NA_integer_, nprev_eff)

  pos <- 1
  for (i in seq(nprev, h, by = -1)) {
    # Rolling window: expand-to-last with shrinking holdout
    # Y[(1 + nprev - i) : (N - i)]
    y_idx_start <- 1 + nprev - i
    y_idx_end   <- N - i
    Y.window <- Y[y_idx_start:y_idx_end, , drop = FALSE]
    X.window <- if (is.null(X)) NULL else X[y_idx_start:y_idx_end, , drop = FALSE]

    
    # Fit ADL model
    fit <- runARDL(
        Y.window, X.window, indice = indice,
        h = h,
        type = type,
        p_fixed = p_fixed, q_fixed = q_fixed,
        use_x0 = use_x0,
        P_maf = P_maf,
        marx_q = marx_q,
        x_dimred = "pca",
      )

    # Leave NAs for this iteration and continue
      if (verbose) {
        cat(sprintf(
          "iteration %d: p=%s, q=%s\n",
          pos,
          if (!is.null(fit$p)) as.character(fit$p) else "NA",
          if (!is.null(fit$q)) as.character(fit$q) else "NA"
        ))
      }
      coef_list[[pos]] <- fit$coef
      pred_vec[pos]    <- fit$pred
      p_used[pos]      <- if (!is.null(fit$p)) fit$p else NA_integer_
      q_used[pos]      <- if (!is.null(fit$q)) fit$q else NA_integer_

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
      use_x0 = use_x0,
      P_maf = P_maf, marx_q = marx_q,
      p_chosen = p_used,
      q_chosen = q_used
    )
  )
}