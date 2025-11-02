# Moving Average Factor proposed by Coulombe (2021)

# Instead of conducting a global pca on the X variables, we first create lags of X.
# Then, conduct PCA on each of these X's individually and keep the first 2 components
# Augment the PCs with lags of y to form the design matrix

maf_transform <- function(X, P_maf = 4, q_maf = 2, scale_data = TRUE) {
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
    
    # Keep first q_maf PCs 
    n_keep <- min(q_maf, ncol(pca_j$x))
    maf_j <- pca_j$x[, 1:n_keep, drop = FALSE]
    
    # Rename column
    base_name <- colnames(X)[j]
    colnames(maf_j) <- paste0(base_name, "_MAF", seq_len(n_keep))
    
    maf_list[[j]] <- maf_j
  }
  
  # Combine all MAFs into one feature matrix
  maf_mat <- do.call(cbind, maf_list)
  rownames(maf_mat) <- rownames(embed(X[, 1], P_maf))  # optional row alignment
  return(maf_mat)
}

