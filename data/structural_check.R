load("./data/fredmd.RData")

# Testing for structural breaks 
library(strucchange)
library(ggplot2)
md$date <- as.Date(md$date)

unrate_ts <- ts(md$UNRATE,
                start = c(as.numeric(format(min(md$date), "%Y")),
                          as.numeric(format(min(md$date), "%m"))),
                frequency = 12)

break_test=breakpoints(unrate_ts~1)
summary(break_test)
plot(break_test)
breaks <- breakpoints(break_test)$breakpoints
fitted_vals <- fitted(break_test, breaks = 1)

plot_data <- data.frame(
  date = md$date,
  unrate = md$UNRATE,
  fitted = fitted_vals
)
# Plot the ts out
ggplot(plot_data, aes(date, unrate)) +
  geom_line(color = "black") +
  geom_line(aes(y = fitted), color = "red", size = 1) +
  geom_vline(xintercept = plot_data$date[breaks], color = "blue", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Structural Breaks in Unemployment Rate",
       x = "Date", y = "UNRATE")

plot_data$date[breaks]


# Using Chow's test to test for that exact date
index_from_year_month <- function(ts_obj, year, month) {
  st   <- start(ts_obj)         
  freq <- frequency(ts_obj)      # 12 for monthly
  stopifnot(freq %in% c(4, 12))  # quarterly or monthly (adjust if needed)
  as.integer((year - st[1]) * freq + (month - st[2]) + 1)
}

i_break <- index_from_year_month(unrate_ts, 2010, 11)

sctest(unrate_ts ~ 1, type = "Chow", point = i_break)

X <- data.frame(ardl_bic = as.numeric(bench1.ts[, "ARDL-BIC"]))
sctest(y ~ ardl_bic, type = "Chow", point = i_break, data = X)
# p-value is 0.0002375, reject Ho, structural break exists at Nov 2010 (Financial Crisis)