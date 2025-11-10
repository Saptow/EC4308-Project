rm(list=ls())
#setwd()
load("fredmd.RData") 

library(glmnet)
library(HDeconometrics)
library(sandwich) #library to estimate variance for DM test regression using NeweyWest()
library(hdm)

### Some preliminary data manipulation
Y = md
dum=rep(0,nrow(Y)) 
dum[372:480]=1 #create dummy DEC 2010 onwards = 1
Y=cbind(Y,dum=dum) #add dummy to data matrix
Y <- Y[, !(names(Y) %in% c("date","ACOGNO")), drop = FALSE] #remove date col and ACOGNO
Y <- as.matrix(Y)

indice = which(colnames(Y) == "UNRATE")

yy=Y[, "UNRATE"] #get the y variable - unemployment rate

nprev=120 #number of out-of-sample observations (test window )

oosy=tail(yy,nprev) #auxiliary:get the out-of-sample true values (last 120 obs. using tail())


######################################
#LASSO AND ROLLING WINDOW FUNCTION
######################################

runlasso=function(Y,indice,lag,alpha=1,IC="bic"){
  
  dum=Y[,ncol(Y)] # extract dummy from data
  Y=Y[,-ncol(Y)] #data without the dummy
  comp=prcomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:8]) #augment predictors by the first 8 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]   # lags of Y (predictors) corresponding to forecast horizon   
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)] #retrieve the last observations if one-step forecast  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
  }
  dum=tail(dum,length(y)) #cut the dummy to size to account for lost observations due to lags
  
  #Here we use the glmnet wrapper written by the authors that does selection on IC:
  model=ic.glmnet(cbind(scale(X),dum),y,crit=IC,alpha = alpha) #fit the LASSO model selected on IC
  
  pred=predict(model,c(X.out,1)) #generate the forecast (note c(X.out,0) gives the last observations on X's and the dummy (the zero))
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}

########
lasso.rolling.window=function(Y,nprev,indice=1,lag=1,alpha=1,IC="bic"){
  
  save.coef=matrix(NA,nprev,37-3+ncol(Y[,-1])*4 ) #blank matrix for coefficients at each iteration
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  for(i in nprev:1){ #NB: backwards FOR loop: going from 120 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    lasso=runlasso(Y.window,indice,lag,alpha,IC) #call the function to fit the LASSO selected on IC and generate h-step forecast
    save.coef[(1+nprev-i),]=lasso$model$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=lasso$pred #save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
    cat("pred", save.pred[(1+nprev-i)],"\n")
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}



############################################################################
#Penalized regression: LASSO forecasts (BIC, AIC, AICc)
############################################################################

#Add the functions  in func-lasso.R (must be in your working directory)
#Or simply open up func-lasso.R and execute the function commands there


alpha=1 #set alpha=1 for LASSO

#Run forecasts for LASSO (BIC)
lasso1c=lasso.rolling.window(Y,nprev,indice,1,alpha,IC="bic")
lasso3c=lasso.rolling.window(Y,nprev,indice,3,alpha,IC="bic")
lasso6c=lasso.rolling.window(Y,nprev,indice,6,alpha,IC="bic")
lasso12c=lasso.rolling.window(Y,nprev,indice,12,alpha,IC="bic")

#LASSO(BIC) RMSE's
lasso.rmse1=lasso1c$errors[1]
lasso.rmse3=lasso3c$errors[1]
lasso.rmse6=lasso6c$errors[1]
lasso.rmse12=lasso12c$errors[1]


#Run forecasts for LASSO (AIC)

lasso1ca=lasso.rolling.window(Y,nprev,indice,1,alpha,IC="aic")
lasso3ca=lasso.rolling.window(Y,nprev,indice,3,alpha,IC="aic")
lasso6ca=lasso.rolling.window(Y,nprev,indice,6,alpha,IC="aic")
lasso12ca=lasso.rolling.window(Y,nprev,indice,12,alpha,IC="aic")

#LASSO(AIC) RMSE's
lassoa.rmse1=lasso1ca$errors[1]
lassoa.rmse3=lasso3ca$errors[1]
lassoa.rmse6=lasso6ca$errors[1]
lassoa.rmse12=lasso12ca$errors[1]


#Run forecasts for LASSO (AICc)
lasso1caic=lasso.rolling.window(Y,nprev,indice,1,alpha,IC="aicc")
lasso3caic=lasso.rolling.window(Y,nprev,indice,3,alpha,IC="aicc")
lasso6caic=lasso.rolling.window(Y,nprev,indice,6,alpha,IC="aicc")
lasso12caic=lasso.rolling.window(Y,nprev,indice,12,alpha,IC="aicc")

#LASSO(AICc) RMSE's
lassoac.rmse1=lasso1caic$errors[1]
lassoac.rmse3=lasso3caic$errors[1]
lassoac.rmse6=lasso6caic$errors[1]
lassoac.rmse12=lasso12caic$errors[1]

################################################
##Plot ML forecasts for 1, 3, 6, 12-steps
##############################################

#Create the time series object collecting 1-step best=performing ML forecasts
bench1.ts=ts(cbind(oosy,lasso1caic$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench1.ts)=c("True Value","LASSO")

#Create the time series object collecting 1-step best=performing ML forecasts
bench3.ts=ts(cbind(oosy,lasso3caic$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench3.ts)=c("True Value","LASSO")

#Create the time series object collecting 1-step best=performing ML forecasts
bench6.ts=ts(cbind(oosy,lasso6c$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench6.ts)=c("True Value","LASSO")

#Create the time series object collecting 1-step best=performing ML forecasts
bench12.ts=ts(cbind(oosy,lasso12caic$pred), start=c(2010,1), end=c(2019,12), freq=12)
colnames(bench12.ts)=c("True Value","LASSO")

par(mfrow = c(2,2))

#Plot the graph for 1-step forecasts
plot.ts(bench1.ts[,1], main="1-step LASSO forecast", cex.axis=1.5, lwd=2, ylab="UNRATE")
points(bench1.ts[,2], type="l", col="red",lwd=2.8)
legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))

#Plot the graph for 3-step forecasts
plot.ts(bench3.ts[,1], main="3-step LASSO forecast", cex.axis=1.5, lwd=2, ylab="UNRATE")
points(bench3.ts[,2], type="l", col="red",lwd=2.8)
legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))

#Plot the graph for 6-step forecasts
plot.ts(bench6.ts[,1], main="6-step LASSO forecast", cex.axis=1.5, lwd=2, ylab="UNRATE")
points(bench6.ts[,2], type="l", col="red",lwd=2.8)
legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))

#Plot the graph for 12-step forecasts
plot.ts(bench12.ts[,1], main="12-step LASSO forecast", cex.axis=1.5, lwd=2, ylab="UNRATE")
points(bench12.ts[,2], type="l", col="red",lwd=2.8)
legend("bottomleft", c("UNRATE","LASSO"), lty=c(1,1) ,col=c("black","red"))