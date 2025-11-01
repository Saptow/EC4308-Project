# Moving Average Factor proposed by Coulombe (2021)

# Leakage-safe per-variable MAF (PCs of lag blocks)
# Arguments kept backward-compatible; new ones are optional.
maf_transform <- function(
    X,
    p_lag = 4,
    q_maf = 2,             # number of PCs per variable to keep
    scale_lags = TRUE,
    pca_train_rows = NULL  # fit scale/PCA on first rows only
) {
  X <- as.matrix(X)
  Tn <- nrow(X); k <- ncol(X)
  if (Tn <= p_lag) stop("Not enough rows for requested p_lag.")
  
  # Effective rows after lagging (we use lags t-1..t-p_lag, causal)
  T_eff <- Tn - p_lag
  get_lag_block <- function(vec, p) {
    # Columns are x_{t-1},...,x_{t-p}; size T_eff x p
    embed(vec, p + 1)[, -1, drop = FALSE]
  }
  
  # Choose training rows for scaling/PCA (to avoid leakage)
  # If not set, we default to using ALL effective rows (original behavior).
  if (is.null(pca_train_rows)) {
    pca_train_rows <- T_eff
  }
  pca_train_rows <- max(1L, min(pca_train_rows, T_eff))
  
  maf_list <- vector("list", k)
  colnames_list <- vector("list", k)
  
  for (j in 1:k) {
    # T_eff x p_lag
    L_j <- get_lag_block(X[, j], p_lag)
    
    # Scale using training-window stats only (if requested)
    if (scale_lags) {
      mu  <- colMeans(L_j[1:pca_train_rows, , drop = FALSE])
      sdv <- apply(L_j[1:pca_train_rows, , drop = FALSE], 2L, sd)
      sdv[sdv == 0 | is.na(sdv)] <- 1
      L_j <- sweep(L_j, 2L, mu, "-")
      L_j <- sweep(L_j, 2L, sdv, "/")
    }
    
    # Fit PCA on training block only (no look-ahead), then score ALL rows
    pc_fit <- prcomp(L_j[1:pca_train_rows, , drop = FALSE], center = FALSE, scale. = FALSE)
    q_use  <- min(q_maf, ncol(L_j), ncol(pc_fit$rotation))
    if (q_use < 1L) {
      # fallback: no components possible, return zeros
      scores <- matrix(0, nrow = nrow(L_j), ncol = 1)
      colnames(scores) <- paste0("MAF_", colnames(X)[j], "_PC1")
    } else {
      load_k <- pc_fit$rotation[, 1:q_use, drop = FALSE]  # p_lag x q_use
      scores <- L_j %*% load_k                             # T_eff x q_use
      colnames(scores) <- paste0("MAF_", colnames(X)[j], "_PC", 1:q_use)
    }
    
    maf_list[[j]] <- scores
    colnames_list[[j]] <- colnames(scores)
  }
  
  maf_mat <- do.call(cbind, maf_list)             # T_eff x (k*q_use)
  colnames(maf_mat) <- unlist(colnames_list)
  
  # Optionally return aligned X rows (current-period) if you need them
  mat_y <- X[(p_lag + 1):Tn, , drop = FALSE]      # T_eff x k
  
  list(mat_y = mat_y, maf = maf_mat, T_eff = nrow(maf_mat))
}
