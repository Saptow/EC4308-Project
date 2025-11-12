# MARX Transformation (Moving Average Rotation of X)
# Adapted from: Philippe Goulet-Coulombe, "MARX_cheap_code.R"
# Source: https://github.com/philgoucou/marx/blob/main/MARX_cheap_code.R
# Reference: Goulet-Coulombe, P. (2019, 2020), "Macroeconomic Random Forests"

# Function returns:
  #   A list of 3 elements:
  #     1. mat_y      - Matrix of current-period values for all variables (X_t/Y_t).
  #     2. mat_x      - Matrix of raw lagged regressors (X_{t-1}, ..., X_{t-p}).
  #     3. mat_x_marx - Matrix of MARX-transformed regressors, where each lag 
  #                     is replaced by a moving average of its own past lags. 



#install.packages('vars')

marx_transform = function(X, n_lag = 12, scale_data = FALSE) {
  
  library(vars)
  n_var = ncol(X)
  var = VAR(X, p = n_lag, type = "const")
  
  
  # extract data matrix for VAR
  matata = as.matrix(var$datamat)
  mat_y = matata[,1:n_var]      # current value of X
  mat_x = matata[,-c(1:n_var)]  # Apply lag of X
  
  # ex-ante scaling, may be desirable in certain applications
  if (scale_data) {
    mat_y = as.matrix(scale(mat_y))
    mat_x = as.matrix(scale(mat_x))
  }
  
  mat_x_marx = mat_x
  for(l in 2:n_lag){
    for(v in 1:n_var){
      whotoavg=seq(from=v,to=n_var*(l-1)+v,by=n_var)
      mat_x_marx[,n_var*(l-1)+v]=apply(mat_x[,whotoavg],1,mean)
    }}
  
  colnames(mat_x_marx)=paste('MARX_',colnames(mat_x),sep='')
  
  return(list(
    mat_y = mat_y,
    mat_x = mat_x,
    mat_x_marx = mat_x_marx
  ))
  
}



