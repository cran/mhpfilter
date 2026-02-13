## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----library------------------------------------------------------------------
library(mhpfilter)
library(fastverse)
library(tidyverse)

## ----lambda-effect, fig.cap="Effect of Lambda on Trend Smoothness"------------
set.seed(100)
y <- cumsum(rnorm(80)) + 2*sin((1:80)*pi/20)

lambdas <- c(100, 400, 1600, 6400)

# Create data frame for all lambda values
plot_data <- data.frame()

for (lam in lambdas) {
  result <- hp_filter(y, lambda = lam)
  
  temp_data <- data.frame(
    time = rep(1:length(y), 2),
    value = c(y, result$trend),
    series = rep(c("Original", "Trend"), each = length(y)),
    lambda = paste("λ =", lam)
  )
  
  plot_data <- rbind(plot_data, temp_data)
}

# Create faceted plot with combined legend
ggplot(plot_data, aes(x = time, y = value, color = series, linewidth = series)) +
  geom_line() +
  scale_color_manual(values = c("Original" = "black", "Trend" = "blue")) +
  scale_linewidth_manual(values = c("Original" = 0.7, "Trend" = 1.2)) +
  facet_wrap(~ lambda, ncol = 2) +
  labs(x = "Time", y = "Value", color = "Series", linewidth = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"))


## ----gcv-curve, fig.cap="GCV as a Function of Lambda"-------------------------
set.seed(42)
y <- cumsum(rnorm(100, 0.5, 0.3)) + arima.sim(list(ar = 0.8), 100)

# Compute GCV for range of lambdas
lambdas <- seq(100, 5000, by = 50)
gcv_values <- sapply(lambdas, function(lam) {
  res <- hp_filter(y, lambda = lam, as_dt = FALSE)
  T <- length(y)
  resid_ss <- sum((y - res$trend)^2)
  (1 + 2*T/lam) * resid_ss / T
})

# Create data frame for plotting
plot_data <- data.frame(
  lambda = lambdas,
  gcv = gcv_values
)

# Find minimum
opt_idx <- which.min(gcv_values)
opt_lambda <- lambdas[opt_idx]
opt_gcv <- gcv_values[opt_idx]

# Create ggplot
ggplot(plot_data, aes(x = lambda, y = gcv)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = opt_lambda, color = "red", linetype = "dashed", linewidth = 0.8) +
  geom_point(data = data.frame(lambda = opt_lambda, gcv = opt_gcv), 
             aes(x = lambda, y = gcv), 
             color = "red", size = 3) +
  # Corrected: Use annotate() instead of geom_label() with aes()
  annotate("label",
           x = opt_lambda + 500, 
           y = max(gcv_values) * 0.95,
           label = paste("Optimal λ =", opt_lambda),
           color = "red",
           fill = "white",
           size = 4) +
  labs(
    x = expression(lambda),
    y = "GCV",
    title = "Generalized Cross-Validation Function"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

## ----simulation, fig.cap="Simulation: True vs Estimated Cycles"---------------
set.seed(2024)
n <- 100

# Generate true components
trend_true <- cumsum(c(0, rnorm(n-1, 0.5, 0.2)))
cycle_true <- arima.sim(list(ar = c(1.2, -0.4)), n, sd = 1.5)
y <- trend_true + cycle_true

# Apply both filters
mhp_result <- mhp_filter(y, max_lambda = 10000)
hp_result <- hp_filter(y, lambda = 1600)

# Compute MSE
mse_mhp <- mean((mhp_result$cycle - cycle_true)^2)
mse_hp <- mean((hp_result$cycle - cycle_true)^2)

oldpar <- par(mfrow = c(1, 1))
plot(cycle_true, type = "l", lwd = 2, ylim = range(c(cycle_true, mhp_result$cycle, hp_result$cycle)),
     main = "True vs Estimated Cycles", ylab = "Cycle")
lines(mhp_result$cycle, col = "blue", lwd = 2)
lines(hp_result$cycle, col = "red", lwd = 2, lty = 2)
legend("topright", 
       c(paste0("True Cycle"),
         paste0("MHP (λ=", get_lambda(mhp_result), ", MSE=", round(mse_mhp, 2), ")"),
         paste0("HP (λ=1600, MSE=", round(mse_hp, 2), ")")),
       col = c("black", "blue", "red"), lty = c(1, 1, 2), lwd = 2)
par(oldpar)

## ----empirical-demo-----------------------------------------------------------
# Demonstrate cross-country variation
set.seed(999)

# Simulate countries with different characteristics
countries <- data.table(
  name = c("Stable_Developed", "Volatile_Emerging", "Moderate"),
  trend_sd = c(0.2, 0.5, 0.3),
  cycle_sd = c(0.5, 2.0, 1.0),
  cycle_ar = c(0.9, 0.7, 0.8)
)

results <- lapply(1:nrow(countries), function(i) {
  trend <- cumsum(rnorm(80, 0.5, countries$trend_sd[i]))
  cycle <- arima.sim(list(ar = countries$cycle_ar[i]), 80, sd = countries$cycle_sd[i])
  y <- trend + cycle
  res <- mhp_filter(y, max_lambda = 10000)
  data.table(country = countries$name[i], lambda = get_lambda(res))
})

print(rbindlist(results))

## ----max-lambda-choice--------------------------------------------------------
# For most macro applications, 10000 is sufficient
result <- mhp_filter(y, max_lambda = 10000)
cat("Optimal lambda:", get_lambda(result), "\n")

# If optimal λ hits the upper bound, increase max_lambda
if (get_lambda(result) >= 9900) {
  warning("Lambda near upper bound - consider increasing max_lambda")
}

## ----diagnostics--------------------------------------------------------------
comp <- mhp_compare(y, frequency = "quarterly")
print(comp)

# Interpretation guide:
# 1. Large lambda difference: series-specific smoothing important
# 2. Positive sd_diff: HP under-estimates cycle volatility
# 3. Large ar1_diff: affects model calibration

