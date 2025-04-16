
###############################################################################
#                                                                             #
#                                W5453 - ATSaF                                #                                                 #
#                      R code -Project 2                                      #
#          Description, application & comparison of the GARCH, Log-           #
#          GARCH, FIGARCH and FI-Log-GARCH using rugarch and ufRisk           #
#                                                                             #
###############################################################################

library(yfR)
library(ggplot2)
library(zoo)
library(ggpubr)
library(fGarch)
library(dplyr)
library(reshape2)
library(rugarch)
library(ufRisk)

# We choose Mercedes-Benz stock data
data <- yf_get("MBG.DE", first_date = "2000-01-03", last_date = "2025-01-30")
Close <- data$price_close
Date <- as.Date(data$ref_date)
Close <- zoo(Close, order.by = Date)
Ret <- diff(log(Close))

p1 <- autoplot.zoo(Close) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Closing price") +
  ggtitle("Mercedes-Benz closing price series")

p2 <- autoplot.zoo(Ret) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Log-return") +
  ggtitle("Mercedes-Benz log-return series")

price_returns_plot <- ggarrange(p1, p2, nrow = 2, ncol = 1, align = "v")
print(price_returns_plot)


garch11 <- varcast(coredata(Close), model = "sGARCH", order = c(1,1), distr = "norm", n.out = 250)
loggarch11 <- varcast(coredata(Close), model = "lGARCH", order = c(1,1), distr = "norm", n.out = 250)
figarch11 <- varcast(coredata(Close), model = "fiGARCH", distr = "norm", n.out = 250)
filgarch11 <- varcast(coredata(Close), model = "filGARCH", distr = "norm", n.out = 250)

# GARCH(1,1) coefficient
garch11_coef <- garch11$model.fit@fit$coef
garch11_se <- garch11$model.fit@fit$se.coef
# FIGARCH coefficient
figarch11_coef <- figarch11$model.fit@fit$coef
figarch11_se<- figarch11$model.fit@fit$se.coef

vol_fc_garch11 <- garch11$sig.fc %>%
  zoo(order.by = tail(time(Ret), 250))
vol_fc_loggarch11 <- loggarch11$sig.fc %>%
  zoo(order.by = tail(time(Ret), 250))
vol_fc_figarch11 <- figarch11$sig.fc %>%
  zoo(order.by = tail(time(Ret), 250))
vol_fc_filgarch11 <- filgarch11$sig.fc %>%
  zoo(order.by = tail(time(Ret), 250))

vol_df <- data.frame(
  Date = index(vol_fc_garch11),
  GARCH = coredata(vol_fc_garch11),
  LogGARCH = coredata(vol_fc_loggarch11),
  FIGARCH = coredata(vol_fc_figarch11),
  FILogGARCH = coredata(vol_fc_filgarch11)
)


vol_long <- reshape2::melt(vol_df, id.vars = "Date", 
                           variable.name = "Model", 
                           value.name = "Volatility")

p_vol_combined <- ggplot(vol_long, aes(x = Date, y = Volatility, color = Model)) +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom") +
  xlab("Date") +
  ylab("Estimated volatility") +
  ggtitle("Comparison of one-step volatility forecasts across all models")

p_garch <- autoplot.zoo(vol_fc_garch11) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Date") +
  ylab("Volatility") +
  ggtitle("GARCH(1,1)")

p_loggarch <- autoplot.zoo(vol_fc_loggarch11) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Date") +
  ylab("Volatility") +
  ggtitle("LogGARCH")

p_figarch <- autoplot.zoo(vol_fc_figarch11) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Date") +
  ylab("Volatility") +
  ggtitle("FIGARCH")

p_filgarch <- autoplot.zoo(vol_fc_filgarch11) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Date") +
  ylab("Volatility") +
  ggtitle("FI-LogGARCH")


grid_individual <- ggarrange(p_garch, p_loggarch, p_figarch, p_filgarch, 
                             ncol = 2, nrow = 2, align = "hv")
print(p_vol_combined)
print(grid_individual)


# Calculation of descriptive statistics for all models' volatility forecasts
vol_stats_all <- data.frame(
  Model = c("GARCH(1,1)", "LogGARCH", "FIGARCH", "FI-LogGARCH"),
  Mean = c(mean(vol_fc_garch11), mean(vol_fc_loggarch11), mean(vol_fc_figarch11), mean(vol_fc_filgarch11)),
  Median = c(median(vol_fc_garch11), median(vol_fc_loggarch11), median(vol_fc_figarch11), median(vol_fc_filgarch11)),
  Min = c(min(vol_fc_garch11), min(vol_fc_loggarch11), min(vol_fc_figarch11), min(vol_fc_filgarch11)),
  Max = c(max(vol_fc_garch11), max(vol_fc_loggarch11), max(vol_fc_figarch11), max(vol_fc_filgarch11)),
  SD = c(sd(vol_fc_garch11), sd(vol_fc_loggarch11), sd(vol_fc_figarch11), sd(vol_fc_filgarch11))
)

var99_garch11 <- garch11$VaR.v %>% 
  zoo(order.by = tail(time(Ret), 250))
var99_loggarch11 <- loggarch11$VaR.v %>% 
  zoo(order.by = tail(time(Ret), 250))
var99_figarch11 <- figarch11$VaR.v %>% 
  zoo(order.by = tail(time(Ret), 250))
var99_filgarch11 <- filgarch11$VaR.v %>% 
  zoo(order.by = tail(time(Ret), 250))

pot_garch_index <- which(-garch11$ret.out > coredata(var99_garch11))
pot_loggarch_index <- which(-loggarch11$ret.out > coredata(var99_loggarch11))
pot_figarch_index <- which(-figarch11$ret.out > coredata(var99_figarch11))
pot_filgarch_index <- which(-filgarch11$ret.out > coredata(var99_filgarch11))

garch_breaches <- length(pot_garch_index)
loggarch_breaches <- length(pot_loggarch_index)
figarch_breaches <- length(pot_figarch_index)
filgarch_breaches <- length(pot_filgarch_index)
expected_breaches <- 0.01 * 250  # Expected number of breaches at 99% VaR for 250 observations

breach_df <- data.frame(
  Model = c("GARCH(1,1)", "LogGARCH", "FIGARCH", "FI-LogGARCH"),
  Breaches = c(garch_breaches, loggarch_breaches, figarch_breaches, filgarch_breaches),
  Expected = rep(expected_breaches, 4),
  Breach_Rate = c(garch_breaches, loggarch_breaches, figarch_breaches, filgarch_breaches)/250 * 100
)

garch_mse <- mean((garch11$ret.out - garch11$mean)^2)
loggarch_mse <- mean((loggarch11$ret.out - loggarch11$mean)^2)
figarch_mse <- mean((figarch11$ret.out - figarch11$mean)^2)
filgarch_mse <- mean((filgarch11$ret.out - filgarch11$mean)^2)

cat("\n--- Volatility Statistics ---\n")
print(vol_stats_all)

cat("\n--- VaR Analysis ---\n")
print(breach_df)


df_garch <- data.frame(
  Date = time(var99_garch11),
  VaR = -coredata(var99_garch11),
  Return = garch11$ret.out
)

df_garch_breach <- data.frame(
  Date = time(var99_garch11)[pot_garch_index],
  Breach = -coredata(var99_garch11)[pot_garch_index]
)

plot_VaR_garch <- ggplot(df_garch) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_segment(aes(x = Date, y = Return, xend = Date, yend = 0, color = "Returns")) +
  geom_line(aes(x = Date, y = VaR, color = "99%-VaR")) +
  geom_point(data = df_garch_breach, aes(x = Date, y = Breach, shape = "VaR Breach"), 
             color = "red", size = 2.5) +
  xlab("Date") +
  ylab("Log-return and 99%-VaR") +
  ggtitle("GARCH(1,1) Model") +
  scale_color_manual(name = "Series", 
                     values = c("Returns" = "black", "99%-VaR" = "forestgreen")) +
  scale_shape_manual(name = "Points", values = c("VaR Breach" = 16)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


df_loggarch <- data.frame(
  Date = time(var99_loggarch11),
  VaR = -coredata(var99_loggarch11),
  Return = loggarch11$ret.out
)

df_loggarch_breach <- data.frame(
  Date = time(var99_loggarch11)[pot_loggarch_index],
  Breach = -coredata(var99_loggarch11)[pot_loggarch_index]
)

plot_VaR_loggarch <- ggplot(df_loggarch) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_segment(aes(x = Date, y = Return, xend = Date, yend = 0, color = "Returns")) +
  geom_line(aes(x = Date, y = VaR, color = "99%-VaR")) +
  geom_point(data = df_loggarch_breach, aes(x = Date, y = Breach, shape = "VaR Breach"), 
             color = "red", size = 2.5) +
  xlab("Date") +
  ylab("Log-return and 99%-VaR") +
  ggtitle("LogGARCH Model") +
  scale_color_manual(name = "Series", 
                     values = c("Returns" = "black", "99%-VaR" = "forestgreen")) +
  scale_shape_manual(name = "Points", values = c("VaR Breach" = 16)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))

df_figarch <- data.frame(
  Date = time(var99_figarch11),
  VaR = -coredata(var99_figarch11),
  Return = figarch11$ret.out
)

df_figarch_breach <- data.frame(
  Date = time(var99_figarch11)[pot_figarch_index],
  Breach = -coredata(var99_figarch11)[pot_figarch_index]
)

plot_VaR_figarch <- ggplot(df_figarch) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_segment(aes(x = Date, y = Return, xend = Date, yend = 0, color = "Returns")) +
  geom_line(aes(x = Date, y = VaR, color = "99%-VaR")) +
  geom_point(data = df_figarch_breach, aes(x = Date, y = Breach, shape = "VaR Breach"), 
             color = "red", size = 2.5) +
  xlab("Date") +
  ylab("Log-return and 99%-VaR") +
  ggtitle("FIGARCH Model") +
  scale_color_manual(name = "Series", 
                     values = c("Returns" = "black", "99%-VaR" = "forestgreen")) +
  scale_shape_manual(name = "Points", values = c("VaR Breach" = 16)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


df_filgarch <- data.frame(
  Date = time(var99_filgarch11),
  VaR = -coredata(var99_filgarch11),
  Return = filgarch11$ret.out
)

df_filgarch_breach <- data.frame(
  Date = time(var99_filgarch11)[pot_filgarch_index],
  Breach = -coredata(var99_filgarch11)[pot_filgarch_index]
)

plot_VaR_filgarch <- ggplot(df_filgarch) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_segment(aes(x = Date, y = Return, xend = Date, yend = 0, color = "Returns")) +
  geom_line(aes(x = Date, y = VaR, color = "99%-VaR")) +
  geom_point(data = df_filgarch_breach, aes(x = Date, y = Breach, shape = "VaR Breach"), 
             color = "red", size = 2.5) +
  xlab("Date") +
  ylab("Log-return and 99%-VaR") +
  ggtitle("FI-LogGARCH Model") +
  scale_color_manual(name = "Series", 
                     values = c("Returns" = "black", "99%-VaR" = "forestgreen")) +
  scale_shape_manual(name = "Points", values = c("VaR Breach" = 16)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))


var_plots_grid <- ggarrange(plot_VaR_garch, plot_VaR_loggarch, 
                            plot_VaR_figarch, plot_VaR_filgarch, 
                            ncol = 2, nrow = 2, 
                            common.legend = TRUE, align = "hv", 
                            legend = "bottom")

plot(var_plots_grid)

# Create a combined data frame for all models' VaR
var_combined <- data.frame(
  Date = time(var99_garch11),
  GARCH = -coredata(var99_garch11),
  LogGARCH = -coredata(var99_loggarch11),
  FIGARCH = -coredata(var99_figarch11),
  FILogGARCH = -coredata(var99_filgarch11),
  Returns = garch11$ret.out  # Using returns from one model (same for all)
)

var_long <- melt(var_combined, id.vars = c("Date", "Returns"), 
                 variable.name = "Model", value.name = "VaR")

combined_var_plot <- ggplot(var_long) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_segment(aes(x = Date, y = Returns, xend = Date, yend = 0), color = "black") +
  geom_line(aes(x = Date, y = VaR, color = Model)) +
  xlab("Date") +
  ylab("Log-return and 99%-VaR") +
  ggtitle("Comparison of 99%-VaR Forecasts Across All Models") +
  theme(legend.position = "bottom")

plot(combined_var_plot)

