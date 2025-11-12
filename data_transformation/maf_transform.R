# Moving Average Factor proposed by Coulombe (2021)

# Instead of conducting a global pca on the X variables, we first create lags of X.
# Then, conduct PCA on each of these X's individually and keep the number of components at explained at least 90% variance
# Augment the PCs with lags of y to form the design matrix

maf_transform <- function(X, P_maf = 4, scale_data = TRUE) {
  
  T <- nrow(X)
  K <- ncol(X)
  maf_list <- vector("list", K) 
  
  for (j in seq_len(K)) {
    # Build lag matrix for variable j
    xj <- X[, j]
    lag_mat <- embed(xj, P_maf)  
    
    # Standardise if true
    if (scale_data) {
      lag_mat <- scale(lag_mat)
    }
    
    # PCA on lag matrix within-variable
    pca_j <- prcomp(lag_mat, center = FALSE, scale. = FALSE)
    
    # Keep PCs that explain at least 90% variance
    eig_var  <- pca_j$sdev^2
    var_exp  <- eig_var / sum(eig_var)
    cum_var  <- cumsum(var_exp)
    k_needed <- which(cum_var >= 0.90)[1]
    if (is.na(k_needed)) k_needed <- length(var_exp)
    
    # safe keeping to avoid zero-PC case
    k_keep <- max(1, min(k_needed, length(var_exp)))
    
    maf_j <- pca_j$x[, seq_len(k_keep), drop = FALSE]
    
    # Rename column
    base_name <- colnames(X)[j]
    colnames(maf_j) <- paste0(base_name, "_MAF", seq_len(k_keep))
    
    maf_list[[j]] <- maf_j
  }
  
  # Combine all MAFs into one feature matrix
  maf_mat <- do.call(cbind, maf_list)
  rownames(maf_mat) <- rownames(embed(X[, 1], P_maf)) # align rownames
  return(maf_mat)
}

