rm(list=ls())
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



# Run fixed
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


# Load all results
res_fixed1 <- get(load("./ADL/adl_rolling_fixed_h1.RData"))
res_fixed3 <- get(load("./ADL/adl_rolling_fixed_h3.RData"))
res_fixed6 <- get(load("./ADL/adl_rolling_fixed_h6.RData"))
res_fixed12 <- get(load("./ADL/adl_rolling_fixed_h12.RData"))
