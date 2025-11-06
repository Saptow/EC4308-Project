rm(list=ls())
## package for ADL model
# install.packages("ARDL")
knitr::opts_chunk$set(echo = TRUE)

## Load library
# Assume working directory is at root of repo
library(dplyr)
source('./ADL/func-adl-pca.R')

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

# # Option 1: Run using BIC 
# for (h in horizon_windows){
#   print(paste0("Running horizon: ", h))
#   res_bic=ardl.rolling.window(
#     Y = Y,
#     X = X,
#     nprev = nprev,
#     indice = 1, # UNRATE col in Y
#     h = h,
#     type = "bic",
#     p_max = 4,
#     p_fixed=4,
#     q_max = 4,
#     use_x0 = TRUE,
#     verbose = TRUE,
#     search_mode="q"
#   )
#   save(res_bic, file = paste0("./ADL/adl_rolling_bic_h", h, ".RData"))
# }

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
    x_dimred = "pca",
    pca_var = 0.90
  )
  save(res_fixed, file = paste0("./ADL/pca_adl_rolling_fixed_h", h, ".RData"))
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
    p_fixed=4,
    use_x0 = TRUE,
    P_maf = 4,
    x_dimred = "pca",
    pca_var = 0.90
  )
  save(res_maf, file = paste0("./ADL/pca_adl_rolling_maf_h", h, ".RData"))
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
      p_fixed=4,
      use_x0 = TRUE,
      marx_q = 4,
      x_dimred = "pca",
      pca_var = 0.90
     )
     save(res_marx, file = paste0("./ADL/pca_adl_rolling_marx_h", h, ".RData"))
     }

# Load all results
res_fixed1 <- get(load("./ADL/pca_adl_rolling_fixed_h1.RData"))
res_fixed3 <- get(load("./ADL/pca_adl_rolling_fixed_h3.RData"))
res_fixed6 <- get(load("./ADL/pca_adl_rolling_fixed_h6.RData"))
res_fixed12 <- get(load("./ADL/pca_adl_rolling_fixed_h12.RData"))

res_maf1 <- get(load("./ADL/pca_adl_rolling_maf_h1.RData"))
res_maf3 <- get(load("./ADL/pca_adl_rolling_maf_h3.RData"))
res_maf6 <- get(load("./ADL/pca_adl_rolling_maf_h6.RData"))
res_maf12 <- get(load("./ADL/pca_adl_rolling_maf_h12.RData"))

res_marx1 <- get(load("./ADL/pca_adl_rolling_marx_h1.RData"))
res_marx3 <- get(load("./ADL/pca_adl_rolling_marx_h3.RData"))
res_marx6 <- get(load("./ADL/pca_adl_rolling_marx_h6.RData"))   
res_marx12 <- get(load("./ADL/pca_adl_rolling_marx_h12.RData"))

# Helper: plot ARDL benchmark for a given horizon
plot_ardl_bench <- function(res_bic, res_fixed, res_maf, res_marx, real,
                            h, end = c(2019, 12), freq = 12,
                            ylab = "Change in Unemployment Rate",
                            main = NULL) {
  # Align lengths across ALL models for safety
  L <- min(length(res_bic$pred),
           length(res_fixed$pred),
           length(res_maf$pred),
           length(res_marx$pred))
  stopifnot(L > 0)

  bic   <- as.numeric(res_bic$pred)[seq_len(L)]
  fixed <- as.numeric(res_fixed$pred)[seq_len(L)]
  maf   <- as.numeric(res_maf$pred)[seq_len(L)]
  marx  <- as.numeric(res_marx$pred)[seq_len(L)]
  true  <- tail(as.numeric(real), L)

  M <- cbind(bic, fixed, maf, marx, true)
  colnames(M) <- c("ARDL-BIC", "ARDL-Fixed", "ARDL-MAF", "ARDL-MARX", "True")

  # Fixed end date; ts() infers the start
  obj <- ts(M, end = end, frequency = freq)

  if (is.null(main)) main <- sprintf("%d-step Ahead Forecast", h)

  plot.ts(obj[, "True"], main = main,
          cex.axis = 1.2, lwd = 2, col = "black",
          ylab = ylab, ylim = range(obj, na.rm = TRUE))
  lines(obj[, "ARDL-BIC"],   col = "blue",   lwd = 1.5)
  lines(obj[, "ARDL-Fixed"], col = "red",    lwd = 1.5, lty = 2)
  lines(obj[, "ARDL-MAF"],   col = "green",  lwd = 1.5)
  lines(obj[, "ARDL-MARX"],  col = "purple", lwd = 1.5)
  legend("topright",
         legend = c("ARDL-BIC", "ARDL-Fixed", "ARDL-MAF", "ARDL-MARX", "Actual"),
         col = c("blue", "red", "green", "purple", "black"),
         lty = c(1, 2, 1, 1, 1), lwd = c(1.5, 1.5, 1.5, 1.5, 2),
         bty = "n", cex = 0.8)
}

# ---- 4 plots in 1 window ----
op <- par(mfrow = c(2, 2))          # 2x2 grid
on.exit(par(op), add = TRUE)        # restore on exit

options(repr.plot.width = 12, repr.plot.height = 8)

real <- as.numeric(Y[, 1])
end_date <- c(2019, 12)

plot_ardl_bench(res_bic1,  res_fixed1,  res_maf1,  res_marx1,  real, h = 1,  end = end_date)
plot_ardl_bench(res_bic3,  res_fixed3,  res_maf3,  res_marx3,  real, h = 3,  end = end_date)
plot_ardl_bench(res_bic6,  res_fixed6,  res_maf6,  res_marx6,  real, h = 6,  end = end_date)
plot_ardl_bench(res_bic12, res_fixed12, res_maf12, res_marx12, real, h = 12, end = end_date)

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

     # compute errors to evaluate
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
