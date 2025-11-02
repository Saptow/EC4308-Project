rm(list=ls())
load("fredmd.RData") 

library(glmnet)

# --- 1) -----------------------------------------------
Yraw <- md

# keep columns with no NAs (or skip this if you prefer to impute)
Y <- Yraw[, colSums(is.na(Yraw)) == 0, drop = FALSE]

# parse date & build dummy (Dec 2010 onward)
Y$date <- as.Date(Y$date)  # adjust if your format differs
Y$dummy <- as.numeric(Y$date >= as.Date("2010-12-01"))

nprev <- 120
yy <- Y$UNRATE
oosy  <- tail(yy, nprev)  


# --- 2) Define y and X -------------------------------------------
y_vec <- as.numeric(Y$UNRATE)
X_df  <- Y[, setdiff(names(Y), c("UNRATE", "date")), drop = FALSE]

# ensure numeric matrix
stopifnot(all(sapply(X_df, is.numeric)))
X_mat <- as.matrix(X_df)

# optional: check & handle non-finite
X_mat[!is.finite(X_mat)] <- NA
y_vec[!is.finite(y_vec)] <- NA

# if any NA remain, drop those rows consistently (or impute instead)
keep <- complete.cases(X_mat, y_vec)
X_mat <- X_mat[keep, , drop = FALSE]
y_vec <- y_vec[keep]
date_vec <- Y$date[keep]  # for plotting later if needed

# --- 3) Add 8 principal components ----------------------------------------
pca <- princomp(scale(X_mat, scale = FALSE))
pc_scores <- pca$scores[, 1:8, drop = FALSE]
X_aug <- cbind(X_mat, pc_scores)

# --- 4) Add 4 lags to ALL predictors (current + 4 lags) -------------------
k <- 5  # 1 current + 4 lags
lagged <- embed(X_aug, k)                      # (n - 4) x (k * p)
X_lagged <- lagged[, -(1:ncol(X_aug)), drop=FALSE]  # drop “current” block
y_lagged <- y_vec[k:length(y_vec)]                 # align y to lags
date_lagged <- date_vec[k:length(date_vec)]        # optional for plotting

stopifnot(nrow(X_lagged) == length(y_lagged))

# --- 5) Rolling LASSO (CV lambda per window) ------------------------------
rolling_lasso <- function(x, y, window = 120, h = 1, alpha = 1) {
  n <- nrow(x); preds <- rep(NA_real_, n)
  grid <- 10^seq(1, -4, length = 100)
  for (t in seq(window, n - h)) {
    tr <- (t - window + 1):t
    va <- t + h
    cv  <- cv.glmnet(x[tr, ], y[tr], alpha = alpha, lambda = grid)
    fit <- glmnet(   x[tr, ], y[tr], alpha = alpha, lambda = grid)
    preds[va] <- predict(fit, s = cv$lambda.min, newx = x[va, , drop = FALSE])
  }
  valid <- which(!is.na(preds))
  list(
    pred = preds,
    rmse = sqrt(mean((y[valid] - preds[valid])^2))
  )
}

# Create multi-horizon target vectors
y1  <- y_lagged[1:(length(y_lagged) - 1)]
y3  <- y_lagged[1:(length(y_lagged) - 3)]
y6  <- y_lagged[1:(length(y_lagged) - 6)]
y12 <- y_lagged[1:(length(y_lagged) - 12)]

# Corresponding predictor matrices (trim last h rows so y and X align)
x1  <- X_lagged[1:(nrow(X_lagged) - 1), , drop = FALSE]
x3  <- X_lagged[1:(nrow(X_lagged) - 3), , drop = FALSE]
x6  <- X_lagged[1:(nrow(X_lagged) - 6), , drop = FALSE]
x12 <- X_lagged[1:(nrow(X_lagged) - 12), , drop = FALSE]


set.seed(578903)
res1  <- rolling_lasso(x1,  y1,  window = 120, h = 1,  alpha = 1)
res3  <- rolling_lasso(x3,  y3,  window = 120, h = 3,  alpha = 1)
res6  <- rolling_lasso(x6,  y6,  window = 120, h = 6,  alpha = 1)
res12 <- rolling_lasso(x12, y12, window = 120, h = 12, alpha = 1)

res1$rmse; res3$rmse; res6$rmse; res12$rmse

oos_slice <- function(res, y, nprev) {
  idx <- (length(y) - nprev + 1):length(y)
  list(
    true = y[idx],
    pred = tail(res$pred, nprev),
    idx  = idx
  )
}

# extract OOS segments for each horizon
o1  <- oos_slice(res1,  y1,  nprev)
o3  <- oos_slice(res3,  y3,  nprev)
o6  <- oos_slice(res6,  y6,  nprev)
o12 <- oos_slice(res12, y12, nprev)


# --- plot ---
par(mfrow = c(2,2))

plot(oosy,  type="l", main="1-Step Ahead",  ylab="UNRATE", xlab="Time")
lines(o1$pred,  col="red")
legend("topright", c("Actual","Pred"), col=c("black","red"), lty=1, bty="n")

plot(oosy,  type="l", main="3-Step Ahead",  ylab="UNRATE", xlab="Time")
lines(o3$pred,  col="blue")

plot(oosy,  type="l", main="6-Step Ahead",  ylab="UNRATE", xlab="Time")
lines(o6$pred,  col="green")

plot(oosy, type="l", main="12-Step Ahead", ylab="UNRATE", xlab="Time")
lines(o12$pred, col="purple")

par(mfrow = c(1,1))