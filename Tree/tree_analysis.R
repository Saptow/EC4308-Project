###### 
# Random forest analysis
######
#load FRED-MD data
load("data/fredmd.RData")
Y = md
actual = tail(Y[, c("date", "UNRATE")], 120)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(tibble)
library(reshape2)
############################################################
#Load results for h=1
############################################################
load("Tree/rf_h1.RData")
load("Tree/marx_rf_h1.RData")
load("Tree/maf_rf_h1.RData")
plot(actual$date, actual$UNRATE, type = "l", col = "grey", lwd = 2,
     ylab = "Change in Unemployment Rate", xlab = "Date",
     main = "Random Forest 1-step ahead forecast")
lines(actual$date, rf12c$pred, col = "#D55E00", lwd = 2)
lines(actual$date, marx_rf1$pred, col = "#0072B2", lwd = 2)
lines(actual$date, maf_rf1$pred,  col = "#009E73", lwd = 2)
legend("bottomright",
       legend = c("Actual", "Base RF", "MARX-RF", "MAF-RF"),
       col = c("grey40", "#D55E00", "#0072B2", "#009E73"),
       lty = 1, lwd = 2,
       bty = "n")
#Inspect rsme and mae figures
rf12c$errors # 0.1440469   0.1142141
marx_rf1$errors # 0.1475153 0.1141694
maf_rf1$errors # 0.1492330  0.1150709

############################################################
#Load results for h=3
############################################################
load("Tree/rf_h3.RData")
load("Tree/marx_rf_h3.RData")
load("Tree/maf_rf_h3.RData")
plot(actual$date, actual$UNRATE, type = "l", col = "grey", lwd = 2,
     ylab = "Change in Unemployment Rate", xlab = "Date",
     main = "Random Forest 3-step ahead forecast")
lines(actual$date, rf32c$pred, col = "#D55E00", lwd = 2)
lines(actual$date, marx_rf3$pred, col = "#0072B2", lwd = 2)
lines(actual$date, maf_rf3$pred,  col = "#009E73", lwd = 2)
legend("bottomright",
       legend = c("Actual", "Base RF", "MARX-RF", "MAF-RF"),
       col = c("grey40", "#D55E00", "#0072B2", "#009E73"),
       lty = 1, lwd = 2,
       bty = "n")
#Inspect rsme and mae figures
rf32c$errors #0.1480140   0.1144344
marx_rf3$errors #0.1479183   0.1160091
maf_rf3$errors #0.1483849   0.1156695
############################################################
#Load results for h=6
############################################################
load("Tree/rf_h6.RData")
load("Tree/marx_rf_h6.RData")
load("Tree/maf_rf_h6.RData")
plot(actual$date, actual$UNRATE, type = "l", col = "grey", lwd = 2,
     ylab = "Change in Unemployment Rate", xlab = "Date",
     main = "Random Forest 6-step ahead forecast")
lines(actual$date, rf62c$pred, col = "#D55E00", lwd = 2)
lines(actual$date, marx_rf6$pred, col = "#0072B2", lwd = 2)
lines(actual$date, maf_rf6$pred,  col = "#009E73", lwd = 2)
legend("bottomright",
       legend = c("Actual", "Base RF", "MARX-RF", "MAF-RF"),
       col = c("grey40", "#D55E00", "#0072B2", "#009E73"),
       lty = 1, lwd = 2,
       bty = "n")
#Inspect rsme and mae figures
rf62c$errors #0.1530741   0.1205586
marx_rf6$errors #0.1442572   0.1124492
maf_rf6$errors #0.1446665   0.1127085
############################################################
#Load results for h=12
############################################################
load("Tree/rf_h12.RData")
load("Tree/marx_rf_h12.RData")
load("Tree/maf_rf_h12.RData")
plot(actual$date, actual$UNRATE, type = "l", col = "grey", lwd = 2,
     ylab = "Change in Unemployment Rate", xlab = "Date",
     main = "Random Forest 12-step ahead forecast")
lines(actual$date, rf122c$pred, col = "#D55E00", lwd = 2)
lines(actual$date, marx_rf12$pred, col = "#0072B2", lwd = 2)
lines(actual$date, maf_rf12$pred,  col = "#009E73", lwd = 2)
legend("bottomright",
       legend = c("Actual", "Base RF", "MARX-RF", "MAF-RF"),
       col = c("grey40", "#D55E00", "#0072B2", "#009E73"),
       lty = 1, lwd = 2,
       bty = "n")
#Inspect rsme and mae figures
rf122c$errors #0.1757856   0.1347257
marx_rf12$errors #0.1396497   0.1103640
maf_rf12$errors #0.1397184   0.1086710

###########
# Explore importance
###########

imp_list <- marx_rf6$save.importance
imp_list <- Filter(function(x) !is.null(x) && length(x) > 0, imp_list)
stopifnot(length(imp_list) > 0)


vecs <- lapply(imp_list, function(m) {
  m <- as.matrix(m)                 
  v <- m[, 1]                       
  names(v) <- rownames(m)           
  v
})

all_vars <- Reduce(union, lapply(vecs, names))
imp_mat <- sapply(vecs, function(v) {
  out <- setNames(rep(NA_real_, length(all_vars)), all_vars)
  out[names(v)] <- v
  out
})
rownames(imp_mat) <- all_vars
colnames(imp_mat) <- paste0("iter_", seq_len(ncol(imp_mat)))

# top-N variables by mean importance
top_n <- 10
mean_imp <- rowMeans(imp_mat, na.rm = TRUE)
keep_idx <- order(mean_imp, decreasing = TRUE)[seq_len(min(top_n, length(mean_imp)))]
imp_mat  <- imp_mat[keep_idx, , drop = FALSE]


df_impt <- as.data.frame(imp_mat) %>%
  tibble::rownames_to_column("var") %>%
  pivot_longer(starts_with("iter_"), names_to = "iter", values_to = "incmse") %>%
  mutate(
    iter = readr::parse_number(iter),
    var  = factor(var, levels = rev(rownames(imp_mat)))
  )

ggplot(df_impt, aes(x = iter, y = var, fill = incmse)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", direction = 1) +
  labs(
    title = "Permutation importance over rolling iterations — MARX-RF (h=12)",
    x = "Iteration", y = NULL, fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9)
  )


