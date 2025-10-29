# Random forest functions

runrf = function(Y,indice,lag){ 
  # Drop date column
  Y = Y[, -1]   
  
  # Extract target and predictors columns
  y_target = Y[,indice, drop = FALSE]
  y_predictors = Y[,-indice, drop=FALSE]
  
  # Run PCA on predictors
  comp = princomp(scale(y_predictors,scale=FALSE))
  var_cum = cumsum(comp$sdev^2 / sum(comp$sdev^2)) #Calculate cumulative variance explained
  n_keep = min(which(var_cum >= 0.9)) #keep components that explained at least 90% of variance
  pcs = comp$scores[, 1:n_keep]
  Y2 = cbind(y_target, pcs)
  aux = embed(as.matrix(Y2), 4 + lag) #create 4 lags + forecast horizon shift (=lag option)
  y = aux[, 1]  
  X = aux[, -seq_len(ncol(Y2) * lag)]
  
  if (lag == 1) {
    X.out = tail(aux, 1)[1:ncol(X)]
  } else {
    X.out = aux[, -seq_len(ncol(Y2) * (lag - 1))]
    X.out = tail(X.out, 1)[1:ncol(X)]
  }
  
  
  feat_names = paste0("x", seq_len(ncol(X)))
  colnames(X) = feat_names
  X.out = as.numeric(X.out)  r
  X.out = as.data.frame(t(X.out), check.names = FALSE)
  colnames(X.out) = feat_names
  
  # --- Fit random forest and predict ---
  model = randomForest(X, y, importance = TRUE)
  pred = predict(model, X.out)
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


# Rolling window for random forest
rf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,nprev,1) ##blank for forecasts
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    rf=runrf(Y.window,indice,lag)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+nprev-i),]=rf$pred #save the forecast
    save.importance[[i]]=importance(rf$model) #save variable importance
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice]#get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}












