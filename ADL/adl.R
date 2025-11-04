## package for ADL model
# install.packages("ARDL")
knitr::opts_chunk$set(echo = TRUE)

## Load library
# Assume working directory is at root of repo
library(dplyr)
source('./ADL/func-adl.R')

# =============================================================================
## For our simple ARDL benchmark, we will be using term spread and yield spread
# =============================================================================

# Load FRED-MD data
load('./data/fredmd_cleaned.RData')
md=data.frame(md) # convert to data.frame
class(md) # check that this is data.frame

## Select relevant variables and convert to data frame
data = md %>%
  select(date, UNRATE, HOUST, GS10, TB3MS, BAA, AAA, aft_break) %>%
  na.omit() %>%
  mutate(
    date=date,
    UNRATE=UNRATE,
    HOUST=HOUST,
    term_spread=GS10 - TB3MS,
    credit_spread=BAA - AAA, 
    aft_break=aft_break,
    .keep="none"
    ) %>%
  arrange(date)

# Prepare Y and X matrices
Y = as.matrix(data[, "UNRATE", drop=FALSE])
X = as.matrix(data[, c("HOUST", "term_spread", "credit_spread", "aft_break")])

# Set number of out-of-sample forecasts
nprev=120

# Define horizon windows (in months)
horizon_windows=c(1,3,6,12)

# Option 1: Run using BIC 
for (h in horizon_windows){
  print(paste0("Running horizon: ", h))
  res_bic=ardl.rolling.window(
    Y = Y,
    X = X,
    nprev = nprev,
    indice = 1, # UNRATE col in Y
    h = h,
    type = "bic",
    p_max = 4,
    p_fixed=4,
    q_max = 4,
    use_x0 = TRUE,
    verbose = TRUE,
    search_mode="q"
  )
  save(res_bic, file = paste0("./ADL/adl_rolling_bic_h", h, ".RData"))
}

# Option 2: Run fixed
for (h in horizon_windows){
  print(paste0("Running horizon: ", h))
  res_fixed=ardl.rolling.window(
    Y = Y,
    X = X,
    nprev = nprev,
    indice = 1, # UNRATE col in Y
    h = h,
    type = "fixed",
    p_fixed = 4,
    q_fixed = 4,
    use_x0 = TRUE,
    verbose = TRUE
  )
  save(res_fixed, file = paste0("./ADL/adl_rolling_fixed_h", h, ".RData"))
}

# Option 3: Run using MAF
for (h in horizon_windows){
  print(paste0("Running horizon: ", h))
  res_maf=ardl.rolling.window(
    Y = Y,
    X = X,
    nprev = nprev,
    indice = 1, # UNRATE col in Y
    h = h,
    type = "maf",
    p_max = 4,
    p_fixed=4,
    q_max = 4,
    use_x0 = TRUE,
    verbose = TRUE,
    P_maf = 4
  )
  save(res_maf, file = paste0("./ADL/adl_rolling_maf_h", h, ".RData"))
}

# Option 4: Run using MARX
for (h in horizon_windows){
     print(paste0("Running horizon: ", h))
     res_marx=ardl.rolling.window(
     Y = Y,
     X = X,
     nprev = nprev,
     indice = 1, # UNRATE col in Y
     h = h,
     type = "marx",
     p_max = 4,
     p_fixed=4,
     q_max = 4,
     use_x0 = TRUE,
     verbose = TRUE,
     marx_q = 4
     )
     save(res_marx, file = paste0("./ADL/adl_rolling_marx_h", h, ".RData"))
     }

# Load all results
res_bic1 <- get(load("./ADL/adl_rolling_bic_h1.RData"))
res_bic3 <- get(load("./ADL/adl_rolling_bic_h3.RData"))
res_bic6 <- get(load("./ADL/adl_rolling_bic_h6.RData"))
res_bic12 <- get(load("./ADL/adl_rolling_bic_h12.RData"))

res_fixed1 <- get(load("./ADL/adl_rolling_fixed_h1.RData"))
res_fixed3 <- get(load("./ADL/adl_rolling_fixed_h3.RData"))
res_fixed6 <- get(load("./ADL/adl_rolling_fixed_h6.RData"))
res_fixed12 <- get(load("./ADL/adl_rolling_fixed_h12.RData"))

res_maf1 <- get(load("./ADL/adl_rolling_maf_h1.RData"))
res_maf3 <- get(load("./ADL/adl_rolling_maf_h3.RData"))
res_maf6 <- get(load("./ADL/adl_rolling_maf_h6.RData"))
res_maf12 <- get(load("./ADL/adl_rolling_maf_h12.RData"))

res_marx1 <- get(load("./ADL/adl_rolling_marx_h1.RData"))
res_marx3 <- get(load("./ADL/adl_rolling_marx_h3.RData"))
res_marx6 <- get(load("./ADL/adl_rolling_marx_h6.RData"))   
res_marx12 <- get(load("./ADL/adl_rolling_marx_h12.RData"))
# ============================================
# Helper Functions
# ============================================

get_p <- function(coef_row) {
  y_lag_cols <- grep("^L[0-9]+\\.y$", names(coef_row), value = TRUE)
  sum(!is.na(coef_row[y_lag_cols]))
}

get_q <- function(coef_row) {
  x_lag_cols <- grep("^L[0-9]+\\.x[0-9]+$", names(coef_row), value = TRUE)
  if (length(x_lag_cols) == 0) return(0)
  n_x_vars <- length(unique(sub(".*x([0-9]+)$", "\\1", x_lag_cols)))
  sum(!is.na(coef_row[x_lag_cols])) / n_x_vars
}

# ============================================
# Extract p and q selections (BIC models)
# ============================================

p_bic1 <- apply(res_bic1$coef, 1, get_p)
p_bic3 <- apply(res_bic3$coef, 1, get_p)
p_bic6 <- apply(res_bic6$coef, 1, get_p)
p_bic12 <- apply(res_bic12$coef, 1, get_p)

q_bic1 <- apply(res_bic1$coef, 1, get_q)
q_bic3 <- apply(res_bic3$coef, 1, get_q)
q_bic6 <- apply(res_bic6$coef, 1, get_q)
q_bic12 <- apply(res_bic12$coef, 1, get_q)



# ============================================
# Plot (p,q) selections over time
# ============================================

par(mfrow = c(2, 2))
plot(p_bic1, type = "l", main = "p selection (h=1)", ylab = "p", 
     xlab = "Iteration", ylim = c(0, 4), col = "blue", lwd = 1.5)
abline(h = mean(p_bic1), col = "red", lty = 2)
text(x = length(p_bic1) * 0.8, y = mean(p_bic1) + 0.3, 
     labels = paste0("Mean: ", round(mean(p_bic1), 2)), col = "red")

plot(p_bic3, type = "l", main = "p selection (h=3)", ylab = "p", 
     xlab = "Iteration", ylim = c(0, 4), col = "blue", lwd = 1.5)
abline(h = mean(p_bic3), col = "red", lty = 2)
text(x = length(p_bic3) * 0.8, y = mean(p_bic3) + 0.3, 
     labels = paste0("Mean: ", round(mean(p_bic3), 2)), col = "red")

plot(p_bic6, type = "l", main = "p selection (h=6)", ylab = "p", 
     xlab = "Iteration", ylim = c(0, 4), col = "blue", lwd = 1.5)
abline(h = mean(p_bic6), col = "red", lty = 2)
text(x = length(p_bic6) * 0.8, y = mean(p_bic6) + 0.3, 
     labels = paste0("Mean: ", round(mean(p_bic6), 2)), col = "red")

plot(p_bic12, type = "l", main = "p selection (h=12)", ylab = "p", 
     xlab = "Iteration", ylim = c(0, 4), col = "blue", lwd = 1.5)
abline(h = mean(p_bic12), col = "red", lty = 2)
text(x = length(p_bic12) * 0.8, y = mean(p_bic12) + 0.3, 
     labels = paste0("Mean: ", round(mean(p_bic12), 2)), col = "red")
par(mfrow = c(1, 1))

# Same for q
par(mfrow = c(2, 2))
plot(q_bic1, type = "l", main = "q selection (h=1)", ylab = "q", 
     xlab = "Iteration", ylim = c(0, 4), col = "darkgreen", lwd = 1.5)
abline(h = mean(q_bic1), col = "red", lty = 2)

plot(q_bic3, type = "l", main = "q selection (h=3)", ylab = "q", 
     xlab = "Iteration", ylim = c(0, 4), col = "darkgreen", lwd = 1.5)
abline(h = mean(q_bic3), col = "red", lty = 2)

plot(q_bic6, type = "l", main = "q selection (h=6)", ylab = "q", 
     xlab = "Iteration", ylim = c(0, 4), col = "darkgreen", lwd = 1.5)
abline(h = mean(q_bic6), col = "red", lty = 2)

plot(q_bic12, type = "l", main = "q selection (h=12)", ylab = "q", 
     xlab = "Iteration", ylim = c(0, 4), col = "darkgreen", lwd = 1.5)
abline(h = mean(q_bic12), col = "red", lty = 2)
par(mfrow = c(1, 1))

# ============================================
# Compare forecasts: BIC vs Fixed
# ============================================

# Get true out-of-sample values
real <- as.numeric(Y[, 1])
nprev_eff <- length(res_bic1$pred)
oosy <- tail(real, nprev_eff)

# h=1
options(repr.plot.width = 12, repr.plot.height = 6)
bench1.ts <- ts(cbind(res_bic1$pred, res_fixed1$pred, oosy), 
                start = c(2010, 1), end = c(2019, 12), freq = 12)
colnames(bench1.ts) <- c("ARDL-BIC", "ARDL-Fixed", "True")
plot.ts(bench1.ts[, "True"], main = "1-step Ahead Forecast", 
        cex.axis = 1.2, lwd = 2, col = "black", 
        ylab = "Change in Unemployment Rate", ylim = range(bench1.ts))
lines(bench1.ts[, "ARDL-BIC"], col = "blue", lwd = 1.5)
lines(bench1.ts[, "ARDL-Fixed"], col = "red", lwd = 1.5, lty = 2)
legend("topright", 
       legend = c("ARDL-BIC", "ARDL-Fixed", "Actual"),
       col = c("blue", "red", "black"),
       lty = c(1, 2, 1), lwd = c(1.5, 1.5, 2), 
       bty = "n", cex = 0.8)

# h=3
bench3.ts <- ts(cbind(res_bic3$pred, res_fixed3$pred, oosy), 
                start = c(2010, 1), end = c(2019, 12), freq = 12)
colnames(bench3.ts) <- c("ARDL-BIC", "ARDL-Fixed", "True")
plot.ts(bench3.ts[, "True"], main = "3-step Ahead Forecast", 
        cex.axis = 1.2, lwd = 2, col = "black", 
        ylab = "Change in Unemployment Rate", ylim = range(bench3.ts))
lines(bench3.ts[, "ARDL-BIC"], col = "blue", lwd = 1.5)
lines(bench3.ts[, "ARDL-Fixed"], col = "red", lwd = 1.5, lty = 2)
legend("topright", 
       legend = c("ARDL-BIC", "ARDL-Fixed", "Actual"),
       col = c("blue", "red", "black"),
       lty = c(1, 2, 1), lwd = c(1.5, 1.5, 2), 
       bty = "n", cex = 0.8)

# h=6
bench6.ts <- ts(cbind(res_bic6$pred, res_fixed6$pred, oosy), 
                start = c(2010, 1), end = c(2019, 12), freq = 12)
colnames(bench6.ts) <- c("ARDL-BIC", "ARDL-Fixed", "True")
plot.ts(bench6.ts[, "True"], main = "6-step Ahead Forecast", 
        cex.axis = 1.2, lwd = 2, col = "black", 
        ylab = "Change in Unemployment Rate", ylim = range(bench6.ts))
lines(bench6.ts[, "ARDL-BIC"], col = "blue", lwd = 1.5)
lines(bench6.ts[, "ARDL-Fixed"], col = "red", lwd = 1.5, lty = 2)
legend("topright", 
       legend = c("ARDL-BIC", "ARDL-Fixed", "Actual"),
       col = c("blue", "red", "black"),
       lty = c(1, 2, 1), lwd = c(1.5, 1.5, 2), 
       bty = "n", cex = 0.8)

# h=12
bench12.ts <- ts(cbind(res_bic12$pred, res_fixed12$pred, oosy), 
                 start = c(2010, 1), end = c(2019, 12), freq = 12)
colnames(bench12.ts) <- c("ARDL-BIC", "ARDL-Fixed", "True")
plot.ts(bench12.ts[, "True"], main = "12-step Ahead Forecast", 
        cex.axis = 1.2, lwd = 2, col = "black", 
        ylab = "Change in Unemployment Rate", ylim = range(bench12.ts))
lines(bench12.ts[, "ARDL-BIC"], col = "blue", lwd = 1.5)
lines(bench12.ts[, "ARDL-Fixed"], col = "red", lwd = 1.5, lty = 2)
legend("topright", 
       legend = c("ARDL-BIC", "ARDL-Fixed", "Actual"),
       col = c("blue", "red", "black"),
       lty = c(1, 2, 1), lwd = c(1.5, 1.5, 2), 
       bty = "n", cex = 0.8)
par(mfrow = c(1, 1))

# ============================================
# Compare forecasts: MAF vs MARX vs BIC
# ============================================
# Get true out-of-sample values

# Helper: plot ARDL benchmark for a given horizon
plot_ardl_bench <- function(res_bic, res_fixed, real,
                            h, end = c(2019, 12), freq = 12,
                            ylab = "Change in Unemployment Rate",
                            main = NULL) {
  # Align lengths (just in case)
  L <- min(length(res_bic$pred), length(res_fixed$pred))
  stopifnot(L > 0)
  bic   <- as.numeric(res_bic$pred)[seq_len(L)]
  fixed <- as.numeric(res_fixed$pred)[seq_len(L)]
  true  <- tail(as.numeric(real), L)

  M <- cbind(bic, fixed, true)
  colnames(M) <- c("ARDL-BIC", "ARDL-Fixed", "True")

  # Use fixed end date; ts() computes the correct start automatically
  obj <- ts(M, end = end, frequency = freq)

  if (is.null(main)) main <- sprintf("%d-step Ahead Forecast", h)

  plot.ts(obj[, "True"], main = main,
          cex.axis = 1.2, lwd = 2, col = "black",
          ylab = ylab, ylim = range(obj, na.rm = TRUE))
  lines(obj[, "ARDL-BIC"],   col = "blue", lwd = 1.5)
  lines(obj[, "ARDL-Fixed"], col = "red",  lwd = 1.5, lty = 2)
  legend("topright",
         legend = c("ARDL-BIC", "ARDL-Fixed", "Actual"),
         col = c("blue", "red", "black"),
         lty = c(1, 2, 1), lwd = c(1.5, 1.5, 2),
         bty = "n", cex = 0.8)
}


# True series and end date
real <- as.numeric(Y[, 1])
end_date <- c(2019, 12)

options(repr.plot.width = 12, repr.plot.height = 6)

# h = 1
plot_ardl_bench(res_bic1,  res_fixed1,  real, h = 1,  end = end_date)

# h = 3
plot_ardl_bench(res_bic3,  res_fixed3,  real, h = 3,  end = end_date)

# h = 6
plot_ardl_bench(res_bic6,  res_fixed6,  real, h = 6,  end = end_date)

# h = 12
plot_ardl_bench(res_bic12, res_fixed12, real, h = 12, end = end_date)

par(mfrow = c(1, 1))

# Build a performance table for BIC, Fixed, MAF, MARX across h = 1,3,6,12
make_perf_table <- function() {
  horizons <- c(1, 3, 6, 12)
  model_key <- c("BIC" = "bic", "Fixed(4,4)" = "fixed", "MAF" = "maf", "MARX" = "marx")

  rows <- list()
  for (h in horizons) {
    for (model_label in names(model_key)) {
      tag <- model_key[[model_label]]
      obj_name <- sprintf("res_%s%d", tag, h)  # e.g., res_bic3, res_maf6, etc.

      res <- get0(obj_name, inherits = TRUE, ifnotfound = NULL)
      if (is.null(res)) next  # skip if you haven't run that model/horizon

      # errors = c(rmse, mae) per your runARDL/rolling code
      rmse <- as.numeric(res$errors[1])
      mae  <- as.numeric(res$errors[2])

      rows[[length(rows) + 1]] <- data.frame(
        Horizon = paste0("h=", h),
        Model   = model_label,
        RMSE    = rmse,
        MAE     = mae,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    warning("No results found (res_* objects missing?).")
    return(invisible(NULL))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Optional: order by Horizon then Model
  out$Horizon <- factor(out$Horizon, levels = paste0("h=", c(1,3,6,12)))
  out <- out[order(out$Horizon, out$Model), ]

  out
}

performance_table <- make_perf_table()
print(performance_table, row.names = FALSE)