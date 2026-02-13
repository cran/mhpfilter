## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5.5,
  dpi = 96,
  message = FALSE,
  warning = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("myaseen208/mhpfilter")

## ----library------------------------------------------------------------------
library(mhpfilter)
library(data.table)
library(ggplot2)

## ----gdp-simulation-----------------------------------------------------------
set.seed(2024)
n <- 120  # 30 years of quarterly data

# Generate components
trend_growth <- 0.005  # 0.5% quarterly growth
trend <- cumsum(c(100, rnorm(n - 1, mean = trend_growth, sd = 0.002)))

# Business cycle: AR(2) process
cycle <- arima.sim(list(ar = c(1.3, -0.5)), n, sd = 2)

# Observed GDP (in logs)
gdp <- trend + cycle

# Apply Modified HP filter
result <- mhp_filter(gdp, max_lambda = 10000)
cat("Optimal lambda:", get_lambda(result), "\n")
cat("GCV value:", format(get_gcv(result), digits = 4), "\n")

## ----autoplot-example, fig.cap="Figure 1: Modified HP Filter Decomposition"----
result_obj <- mhp_filter(gdp, max_lambda = 10000, as_dt = FALSE)
autoplot(result_obj) +
  labs(
    title = "Modified HP Filter Decomposition of Simulated GDP",
    subtitle = paste0("Optimal λ = ", get_lambda(result_obj))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "bottom"
  )

## ----comparison-setup---------------------------------------------------------
# Apply both filters
mhp_res <- mhp_filter(gdp, max_lambda = 10000)
hp_res <- hp_filter(gdp, lambda = 1600)

# Extract key statistics
comp <- mhp_compare(gdp, frequency = "quarterly", max_lambda = 10000)
print(comp)

## ----comparison-plot, fig.cap="Figure 2: HP vs Modified HP Filter Comparison", fig.height=7----
plot_comparison(gdp, frequency = "quarterly", max_lambda = 10000, show_cycle = TRUE) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.box = "vertical"
  )

## ----interpretation-----------------------------------------------------------
# Optimal lambda from MHP
opt_lambda <- get_lambda(mhp_res)
cat("Optimal λ (Modified HP):", opt_lambda, "\n")
cat("Fixed λ (Standard HP):  1600\n\n")

# Cyclical volatility
sd_mhp <- sd(mhp_res$cycle)
sd_hp <- sd(hp_res$cycle)
cat("Cycle SD (Modified HP):", round(sd_mhp, 3), "\n")
cat("Cycle SD (Standard HP):", round(sd_hp, 3), "\n")
cat("Difference:", round(sd_mhp - sd_hp, 3), 
    paste0("(", round(100 * (sd_mhp - sd_hp) / sd_hp, 1), "%)"), "\n")

## ----batch-setup--------------------------------------------------------------
set.seed(456)
n_time <- 100  # 25 years quarterly

# Simulate GDP for multiple countries with different characteristics
countries <- c("USA", "Germany", "Japan", "China", "Brazil", "India")
characteristics <- data.table(
  country = countries,
  trend_growth = c(0.5, 0.4, 0.3, 1.5, 0.6, 1.2),  # % quarterly
  trend_vol = c(0.2, 0.15, 0.15, 0.4, 0.5, 0.6),
  cycle_vol = c(1.5, 1.2, 1.0, 2.5, 3.0, 2.0),
  cycle_ar = c(0.85, 0.88, 0.90, 0.75, 0.70, 0.75)
)

# Generate data
gdp_data <- sapply(1:nrow(characteristics), function(i) {
  char <- characteristics[i, ]
  trend <- 100 + cumsum(rnorm(n_time, 
                               mean = char$trend_growth / 100, 
                               sd = char$trend_vol / 100))
  cycle <- arima.sim(list(ar = char$cycle_ar), n_time, sd = char$cycle_vol)
  trend + cycle
})
colnames(gdp_data) <- countries

# Apply Modified HP filter to all countries
batch_result <- mhp_batch(gdp_data, max_lambda = 10000)

# View optimal lambdas
lambdas_table <- attr(batch_result, "lambdas")
print(lambdas_table)

## ----batch-cycles-plot, fig.cap="Figure 3: Business Cycles Across Countries", fig.height=8----
plot_batch(batch_result, show = "cycle", facet = TRUE) +
  labs(
    title = "Business Cycle Components: Cross-Country Comparison",
    subtitle = "Modified HP Filter with Country-Specific λ",
    caption = "Note: Each country has its own optimal smoothing parameter"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(0.8, "lines")
  )

## ----batch-highlight-plot, fig.cap="Figure 4: Highlighted Comparison", fig.height=6----
plot_batch(batch_result, show = "cycle", facet = FALSE, 
           highlight = c("USA", "China", "India")) +
  labs(
    title = "Business Cycles: Advanced vs Emerging Economies",
    subtitle = "USA (developed) vs China & India (emerging markets)",
    color = "Country (λ)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.key.height = unit(0.8, "lines")
  )

## ----batch-compare------------------------------------------------------------
comparison <- batch_compare(gdp_data, frequency = "quarterly", max_lambda = 10000)
print(comparison)

# Analyze differences
cat("\nSummary of Lambda Differences:\n")
cat("Mean optimal λ:", round(mean(comparison$lambda), 0), "\n")
cat("Fixed λ (HP):  1600\n")
cat("Range of optimal λ:", paste0("[", min(comparison$lambda), ", ", 
                                   max(comparison$lambda), "]"), "\n\n")

cat("Countries with λ > 2000:", 
    paste(comparison$series[comparison$lambda > 2000], collapse = ", "), "\n")
cat("Countries with λ < 1000:", 
    paste(comparison$series[comparison$lambda < 1000], collapse = ", "), "\n")

## ----business-cycle-stats-----------------------------------------------------
# Focus on one country for detailed analysis
usa_gdp <- gdp_data[, "USA"]
usa_mhp <- mhp_filter(usa_gdp, max_lambda = 10000, as_dt = FALSE)
usa_hp <- hp_filter(usa_gdp, lambda = 1600, as_dt = FALSE)

# Create comparison data.table
bc_stats <- data.table(
  Method = c("Modified HP", "Standard HP"),
  Lambda = c(usa_mhp$lambda, 1600),
  `Mean Cycle` = c(mean(usa_mhp$cycle), mean(usa_hp$cycle)),
  `SD Cycle` = c(sd(usa_mhp$cycle), sd(usa_hp$cycle)),
  `Min Cycle` = c(min(usa_mhp$cycle), min(usa_hp$cycle)),
  `Max Cycle` = c(max(usa_mhp$cycle), max(usa_hp$cycle)),
  `AR(1) Coef` = c(
    cor(usa_mhp$cycle[-1], usa_mhp$cycle[-length(usa_mhp$cycle)]),
    cor(usa_hp$cycle[-1], usa_hp$cycle[-length(usa_hp$cycle)])
  )
)

print(bc_stats)

## ----cycle-properties, fig.cap="Figure 5: Cyclical Properties Comparison", fig.height=6----
# Create data for plotting
plot_data <- data.table(
  Time = rep(1:n_time, 2),
  Cycle = c(usa_mhp$cycle, usa_hp$cycle),
  Method = rep(c("Modified HP", "Standard HP"), each = n_time),
  Lambda = rep(c(paste0("λ = ", usa_mhp$lambda), "λ = 1600"), each = n_time)
)

ggplot(plot_data, aes(x = Time, y = Cycle, color = Method, linetype = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Modified HP" = "#0072B2", "Standard HP" = "#D55E00")) +
  scale_linetype_manual(values = c("Modified HP" = "solid", "Standard HP" = "dashed")) +
  labs(
    title = "Business Cycle Components: Method Comparison",
    subtitle = "Modified HP filter allows for data-driven smoothing",
    x = "Time Period (Quarters)",
    y = "Cyclical Component",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

## ----lambda-sensitivity, fig.cap="Figure 6: Sensitivity to Lambda Choice", fig.height=7----
# Try different lambda values
lambdas_to_try <- c(100, 400, 1600, 6400, get_lambda(mhp_res))
lambda_results <- lapply(lambdas_to_try, function(lam) {
  res <- hp_filter(gdp, lambda = lam, as_dt = FALSE)
  data.table(
    Time = 1:length(gdp),
    Original = gdp,
    Trend = res$trend,
    Cycle = res$cycle,
    Lambda = paste0("λ = ", lam),
    Is_Optimal = lam == get_lambda(mhp_res)
  )
})
sens_data <- rbindlist(lambda_results)

# Plot trends
ggplot(sens_data, aes(x = Time)) +
  geom_line(aes(y = Original), color = "gray60", linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = Trend, color = Is_Optimal), linewidth = 0.9) +
  facet_wrap(~Lambda, ncol = 2) +
  scale_color_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#0072B2"),
                     labels = c("Sub-optimal", "Optimal (MHP)"),
                     name = NULL) +
  labs(
    title = "Effect of Lambda on Trend Estimation",
    subtitle = "Lower λ = less smooth (overfitting), Higher λ = too smooth (underfitting)",
    x = "Time Period",
    y = "Value",
    caption = "Gray line shows original series"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray95", color = NA),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

## ----max-lambda-guidance------------------------------------------------------
# For most macroeconomic applications
result_standard <- mhp_filter(gdp, max_lambda = 10000)
cat("Standard search (max_lambda=10000):\n")
cat("  Optimal λ:", get_lambda(result_standard), "\n")
cat("  GCV:", format(get_gcv(result_standard), digits = 4), "\n\n")

# For very smooth trends or unusual series
result_extended <- mhp_filter(gdp, max_lambda = 50000)
cat("Extended search (max_lambda=50000):\n")
cat("  Optimal λ:", get_lambda(result_extended), "\n")
cat("  GCV:", format(get_gcv(result_extended), digits = 4), "\n\n")

# Check if we're near the boundary
if (get_lambda(result_standard) > 0.95 * 10000) {
  cat("WARNING: Optimal λ near upper bound. Consider increasing max_lambda.\n")
}

## ----batch-efficiency, eval=FALSE---------------------------------------------
# # Efficient for multiple series
# large_dataset <- matrix(rnorm(100 * 50), nrow = 100, ncol = 50)
# system.time(batch_result <- mhp_batch(large_dataset, max_lambda = 5000))
# 
# # Less efficient: loop over columns
# system.time({
#   manual_result <- lapply(1:50, function(i) {
#     mhp_filter(large_dataset[, i], max_lambda = 5000)
#   })
# })

