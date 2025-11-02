# Random forest forecast

# load FRED-MD data
load("data/fredmd.RData")
library(randomForest)

Y = md
Y = Y[,-59] #drop New Orders for Consumer Goods (ACOGNO) due to insufficient data

#Create dummy variable after Nov 2010 to handle structural break
cutoff = as.Date("2010-11-01")
Y$DUM = ifelse(Y[, 1] > cutoff, 1, 0)
nprev = 120 #test size




#Use random forest 1 (Going to take some time)
source("Tree/func-rf.R")
rf1c=rf.rolling.window(Y,nprev,idx,1)
rf3c=rf.rolling.window(Y,nprev,idx,3)
rf6c=rf.rolling.window(Y,nprev,idx,6)
rf12c=rf.rolling.window(Y,nprev,idx,12)

#Use random forest 2
source("Tree/func-rf2.R")
rf12c = rf2.rolling.window(Y,nprev,h=1, "UNRATE")
rf32c = rf2.rolling.window(Y,nprev,h=3, "UNRATE")
rf62c = rf2.rolling.window(Y,nprev,h=6, "UNRATE")
rf122c = rf2.rolling.window(Y,nprev,h=12, "UNRATE")



#See the RMSE:
rf.rmse1=rf1c$errors[1]
rf.rmse1
rf.rmse3=rf3c$errors[1]
rf.rmse6=rf6c$errors[1]
rf.rmse12=rf12c$errors[1]

rf12c$errors[1]
rf32c$errors[1]
rf62c$errors[1]
rf122c$errors[1]


############################################################################
# Plotting actual vs predicted values h = 1
dates <- tail(Y[, 1], 120)
actual <- tail(Y[, "UNRATE"], 120)

# Plot actual vs predicted (1-step ahead)
plot(dates, actual, type = "l", col = "black", lwd = 2,
     ylab = "Change in Unemployment Rate",
     xlab = "Date",
     main = "Random Forest Forecast vs Actual (1-step ahead)")
lines(dates, rf12c$pred, col = "red", lwd = 2)
legend("topright",
       legend = c("Actual", "RF"),
       col = c("black", "red"),
       lty = 1, lwd = 2)

############################################################################
# Plot actual vs predicted (3-step ahead)
plot(dates, actual, type = "l", col = "black", lwd = 2,
     ylab = "Change in Unemployment Rate",
     xlab = "Date",
     main = "Random Forest Forecast vs Actual (3-step ahead)")
lines(dates, rf32c$pred, col = "red", lwd = 2)
legend("topright",
       legend = c("Actual", "RF"),
       col = c("black", "red"),
       lty = 1, lwd = 2)

############################################################################
# Plot actual vs predicted (6-step ahead)
plot(dates, actual, type = "l", col = "black", lwd = 2,
     ylab = "Change in Unemployment Rate",
     xlab = "Date",
     main = "Random Forest Forecast vs Actual (6-step ahead)")
lines(dates, rf62c$pred, col = "red", lwd = 2)
legend("topright",
       legend = c("Actual", "RF"),
       col = c("black", "red"),
       lty = 1, lwd = 2)


##############################################################################
# Plot actual vs predicted (12-step ahead)
plot(dates, actual, type = "l", col = "black", lwd = 2,
     ylab = "Change in Unemployment Rate",
     xlab = "Date",
     main = "Random Forest Forecast vs Actual (12-step ahead)")
lines(dates, rf122c$pred, col = "red", lwd = 2)
legend("topright",
       legend = c("Actual", "RF"),
       col = c("black", "red"),
       lty = 1, lwd = 2)

##############################################################################

library(reshape2)
library(ggplot2)

# Extract the list of importance matrices
imp_list <- rf1c$save.importance

# (If your loop runs backward, reverse it so iteration 1 = earliest)
imp_list <- rev(imp_list)

# Combine into one matrix
imp_mat <- do.call(cbind, lapply(imp_list, function(x) x[, "%IncMSE"]))

# Set iteration labels as column names
colnames(imp_mat) <- paste0("iter_", seq_len(ncol(imp_mat)))

# Check what it looks like
head(imp_mat[, 1:5])



# Select top 15 variables by mean importance
avg_imp <- rowMeans(imp_mat, na.rm = TRUE)
top_vars <- names(sort(avg_imp, decreasing = TRUE))[1:15]
imp_top <- imp_mat[top_vars, ]

# Convert to long format for ggplot
imp_long_top <- melt(imp_top, varnames = c("Variable", "Iteration"), value.name = "Importance")

# Convert Iteration to numeric so we can control tick spacing
imp_long_top$Iteration <- as.numeric(gsub("\\D", "", imp_long_top$Iteration))

# Plot
ggplot(imp_long_top, aes(x = Iteration, y = Variable, fill = Importance)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Top 15 Variable Importances over Time (%IncMSE)",
    x = "Rolling Iteration",
    y = NULL
  ) +
  # Show tick marks every 10 iterations
  scale_x_continuous(breaks = seq(0, max(imp_long_top$Iteration, na.rm = TRUE), by = 10))



