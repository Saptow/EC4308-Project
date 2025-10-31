# ============================================
# AR(p) direct h-step forecast 
# ============================================

runAR = function(Y, indice, lag, type="fixed"){
  # make sure Y2 is a numeric matrix for embed()
  Y2  = as.matrix(Y[, indice, drop = FALSE])
  aux = embed(Y2, 4 + lag)
  
  y = aux[, 1, drop = FALSE]
  X = aux[, -(1:(ncol(Y2) * lag)), drop = FALSE]   # AR lags (up to 4)
  
  # unified, horizon-safe X.out
  X.out = t(tail(aux, 1)[1:ncol(X), drop = FALSE])
  
  if(type == "fixed"){  # AR(4)
    model = lm(y ~ X)
    ar.coef = coef(model)                          # length 1+4 in usual cases
    coef = rep(0, ncol(X) + 1)                     # pad to 5
    coef[1:length(ar.coef)] = ar.coef
  }
  
  if(type == "bic"){   # choose p ∈ {1,2,3,4} by BIC
    bb = Inf
    best = NULL
    for(i in seq_len(ncol(X))){
      m = lm(y ~ X[, 1:i, drop = FALSE])
      crit = BIC(m)
      if(crit < bb){
        bb = crit
        best = m
      }
    }
    model   = best
    ar.coef = coef(model)                          # length 1 + p (p ≤ 4)
    coef    = rep(0, ncol(X) + 1)                  # force length 5
    coef[1:length(ar.coef)] = ar.coef
  }
  
  pred = c(1, X.out[1, 1:ncol(X)]) %*% coef
  list(model = model, pred = as.numeric(pred), coef = coef)
}



# =====================================================
# Rolling window for AR(p) direct h-step forecasting
# =====================================================

ar.rolling.window = function(Y, nprev, indice = 1, lag = 1, type = "fixed"){
  
  # For h>1, the last (h-1) forecasts would target beyond sample.
  # Compute and score only the comparable ones.
  # ### FIX: effective OOS size for horizon h
  nprev_eff = nprev - (lag - 1)
  if(nprev_eff <= 0) stop("nprev must be >= lag")
  
  save.coef = matrix(NA_real_, nprev_eff, 5)         # intercept + up to 4 lags
  save.pred = matrix(NA_real_, nprev_eff, 1)
  
  N = nrow(Y)
  pos = 1
  # i goes from nprev down to lag so that target (end + lag) stays within sample
  for(i in seq(nprev, lag, by = -1)){
    Y.window = Y[(1 + nprev - i):(N - i), , drop = FALSE]
    
    fact = runAR(Y.window, indice, lag, type)
    save.coef[pos, ] = fact$coef
    save.pred[pos, ] = fact$pred
    cat("iteration", pos, "\n")
    pos = pos + 1
  }
  
  # --- Plot actual vs. predictions (aligned) ---
  real = Y[, indice]
  # last nprev_eff actuals align with the produced forecasts
  y_true = tail(real, nprev_eff)

  # --- Errors on aligned OOS segment ---
  rmse = sqrt(mean((y_true - save.pred[, 1])^2))
  mae  = mean(abs(y_true - save.pred[, 1]))
  errors = c("rmse" = rmse, "mae" = mae)
  
  return(list("pred" = save.pred, "coef" = save.coef, "errors" = errors))
}
