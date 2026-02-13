#' @useDynLib mhpfilter, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import RcppArmadillo
#' @importFrom data.table data.table setDT setnames setattr rbindlist
#' @importFrom collapse fmean fsd
#' @importFrom ggplot2 ggplot aes geom_line geom_hline labs theme_minimal
#'   scale_color_manual facet_wrap theme element_text ggtitle xlab ylab
#' @importFrom stats setNames
NULL

#' mhpfilter: Fast Modified Hodrick-Prescott Filter
#'
#' @description
#' High-performance implementation of the Modified HP Filter for decomposing
#' time series into trend and cyclical components. Based on the methodology
#' of Choudhary, Hanif & Iqbal (2014) which uses generalized cross-validation
#' to automatically select the optimal smoothing parameter lambda.
#'
#' @details
#' The standard Hodrick-Prescott (1997) filter decomposes a time series
#' \eqn{y_t} into trend \eqn{g_t} and cycle \eqn{c_t} components by minimizing:
#'
#' \deqn{\sum_{t=1}^{T}(y_t - g_t)^2 + \lambda \sum_{t=1}^{T-2}[(g_{t+2} - g_{t+1}) - (g_{t+1} - g_t)]^2}
#'
#' where \eqn{\lambda} is the smoothing parameter that controls the trade-off
#' between trend smoothness and cycle fit.
#'
#' The Modified HP Filter (McDermott, 1997) selects \eqn{\lambda}
#' optimally using generalized cross-validation (GCV). The GCV criterion is:
#'
#' \deqn{GCV(\lambda) = \frac{SSR(\lambda)}{T} \left[ 1 + \frac{2}{T\lambda} \right]}
#'
#' where \eqn{SSR(\lambda)} is the sum of squared residuals. The optimal
#' \eqn{\lambda} minimizes this GCV criterion.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{mhp_filter}}: Apply Modified HP filter to single series
#'   \item \code{\link{hp_filter}}: Apply standard HP filter with fixed lambda
#'   \item \code{\link{mhp_batch}}: Batch process multiple series efficiently
#'   \item \code{\link{mhp_compare}}: Compare HP vs Modified HP for single series
#'   \item \code{\link{batch_compare}}: Compare HP vs Modified HP for multiple series
#'   \item \code{\link{autoplot.mhp}}: ggplot2 visualization for mhp objects
#'   \item \code{\link{plot_comparison}}: Compare HP and Modified HP visually
#'   \item \code{\link{plot_batch}}: Visualize batch processing results
#'   \item \code{\link{get_lambda}}: Extract optimal lambda from results
#'   \item \code{\link{get_gcv}}: Extract GCV value from results
#' }
#'
#' @references
#' Choudhary, M.A., Hanif, M.N., & Iqbal, J. (2014). On smoothing macroeconomic
#' time series using the modified HP filter. \emph{Applied Economics}, 46(19),
#' 2205-2214.
#'
#' Hodrick, R.J., & Prescott, E.C. (1997). Postwar US business cycles: An
#' empirical investigation. \emph{Journal of Money, Credit and Banking}, 29(1), 1-16.
#'
#' McDermott, C.J. (1997). Note on the modified Hodrick-Prescott filter.
#' \emph{IMF Working Paper No. 97/108}.
#'
#' @docType package
#' @name mhpfilter-package
#' @aliases mhpfilter
"_PACKAGE"


#' Modified Hodrick-Prescott Filter
#'
#' @description
#' Decomposes a time series into trend and cyclical components using the
#' Modified HP Filter, which automatically selects the optimal smoothing
#' parameter lambda via generalized cross-validation (GCV).
#'
#' @param x Numeric vector. The time series to decompose. Must have at least
#'   5 observations and no missing values.
#' @param max_lambda Integer. Maximum lambda value to search over. Default is
#'   100000, which covers most macroeconomic applications. The search ranges
#'   from 1 to `max_lambda`.
#' @param as_dt Logical. If TRUE (default), returns a data.table. If FALSE,
#'   returns a list with class "mhp".
#'
#' @return
#' If \code{as_dt = TRUE}: A \code{data.table} with columns:
#' \describe{
#'   \item{original}{The input series}
#'   \item{trend}{The estimated trend component}
#'   \item{cycle}{The cyclical component (original - trend)}
#' }
#' With attributes \code{lambda} (optimal lambda) and \code{gcv} (GCV value).
#'
#' If \code{as_dt = FALSE}: A list with class "mhp" containing elements:
#' \describe{
#'   \item{original}{The input series}
#'   \item{trend}{The estimated trend component}
#'   \item{cycle}{The cyclical component}
#'   \item{lambda}{Optimal smoothing parameter}
#'   \item{gcv}{Generalized cross-validation value}
#' }
#'
#' @details
#' The function performs a grid search over lambda values from 1 to `max_lambda`
#' and selects the lambda that minimizes the GCV criterion. For each lambda,
#' it solves the system:
#'
#' \deqn{(I + \lambda K'K)g = y}
#'
#' where \eqn{K} is the second-difference matrix, \eqn{g} is the trend,
#' and \eqn{y} is the original series.
#'
#' @seealso \code{\link{hp_filter}}, \code{\link{autoplot.mhp}}, \code{\link{get_lambda}}, \code{\link{get_gcv}}
#'
#' @examples
#' # Simulate a trend + cycle series
#' set.seed(42)
#' n <- 100
#' trend <- cumsum(c(0, rnorm(n - 1, mean = 0.5, sd = 0.2)))
#' cycle <- 2 * sin(2 * pi * (1:n) / 20) + rnorm(n, sd = 0.5)
#' y <- trend + cycle
#'
#' # Apply Modified HP filter
#' result <- mhp_filter(y, max_lambda = 10000)
#'
#' # Extract optimal lambda
#' get_lambda(result)
#'
#' # Extract GCV value
#' get_gcv(result)
#'
#' # Print summary
#' print(result)
#'
#' # Plot with ggplot2
#' if (require(ggplot2)) {
#'   autoplot(mhp_filter(y, max_lambda = 10000, as_dt = FALSE))
#' }
#'
#' @references
#' Choudhary, M.A., Hanif, M.N., & Iqbal, J. (2014). On smoothing macroeconomic
#' time series using the modified HP filter. \emph{Applied Economics}, 46(19),
#' 2205-2214.
#'
#' @export
mhp_filter <- function(x, max_lambda = 100000L, as_dt = TRUE) {
  if (!is.numeric(x)) stop("'x' must be a numeric vector", call. = FALSE)
  x <- as.numeric(x)
  if (anyNA(x)) stop("NA values not allowed", call. = FALSE)
  if (length(x) < 5) stop("Series too short (minimum 5 observations required)", call. = FALSE)
  if (max_lambda < 1) stop("'max_lambda' must be >= 1", call. = FALSE)

  # Use internal wrapper function
  res <- .mhp_core(x, as.integer(max_lambda))

  if (as_dt) {
    out <- data.table::data.table(
      original = x,
      trend = as.numeric(res$trend),
      cycle = as.numeric(res$cycle)
    )
    data.table::setattr(out, "lambda", res$lambda)
    data.table::setattr(out, "gcv", res$gcv)
    return(out)
  }

  structure(
    list(
      original = x, trend = as.numeric(res$trend),
      cycle = as.numeric(res$cycle), lambda = res$lambda, gcv = res$gcv
    ),
    class = "mhp"
  )
}


#' Standard Hodrick-Prescott Filter
#'
#' @description
#' Decomposes a time series into trend and cyclical components using the
#' standard HP filter with a fixed smoothing parameter lambda.
#'
#' @param x Numeric vector. The time series to decompose.
#' @param lambda Numeric. The smoothing parameter. Default 1600 (quarterly data).
#'   Common values: 1600 (quarterly), 100 (annual), 14400 (monthly).
#' @param as_dt Logical. If TRUE (default), returns a data.table. If FALSE,
#'   returns a list.
#'
#' @return
#' If \code{as_dt = TRUE}: A \code{data.table} with columns:
#' \describe{
#'   \item{original}{The input series}
#'   \item{trend}{The estimated trend component}
#'   \item{cycle}{The cyclical component}
#' }
#' With attribute \code{lambda} (the input lambda value).
#'
#' If \code{as_dt = FALSE}: A list containing \code{original}, \code{trend},
#' \code{cycle}, and \code{lambda}.
#'
#' @details
#' The HP filter solves the minimization problem:
#'
#' \deqn{\min_{\{g_t\}} \left\{ \sum_{t=1}^T (y_t - g_t)^2 + \lambda \sum_{t=2}^{T-1} [(g_{t+1} - g_t) - (g_t - g_{t-1})]^2 \right\}}
#'
#' The solution is obtained by solving:
#'
#' \deqn{(I + \lambda K'K)g = y}
#'
#' where \eqn{K} is the second-difference matrix.
#'
#' @examples
#' # Example 1: Simple random walk with cycle
#' set.seed(123)
#' n <- 80
#' y <- cumsum(rnorm(n)) + sin((1:n) * pi / 10)
#' result <- hp_filter(y, lambda = 1600)
#' head(result)
#'
#' # Example 2: GDP-like series
#' set.seed(456)
#' gdp <- cumsum(rnorm(100, mean = 0.5, sd = 0.3)) + 2 * cos(2 * pi * (1:100) / 40)
#' gdp_decomp <- hp_filter(gdp, lambda = 1600)
#'
#' # Plot the decomposition
#' if (require(ggplot2)) {
#'   plot_data <- data.table::data.table(
#'     t = 1:length(gdp),
#'     Original = gdp,
#'     Trend = gdp_decomp$trend,
#'     Cycle = gdp_decomp$cycle
#'   )
#'   plot_data_long <- data.table::melt(plot_data, id.vars = "t")
#'
#'   ggplot2::ggplot(plot_data_long, ggplot2::aes(x = t, y = value, color = variable)) +
#'     ggplot2::geom_line(linewidth = 0.8) +
#'     ggplot2::facet_wrap(~variable, ncol = 1, scales = "free_y") +
#'     ggplot2::labs(
#'       title = "HP Filter Decomposition (lambda = 1600)",
#'       x = "Time", y = "Value"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(legend.position = "none")
#' }
#'
#' @references
#' Hodrick, R.J., & Prescott, E.C. (1997). Postwar US business cycles: An
#' empirical investigation. \emph{Journal of Money, Credit and Banking}, 29(1), 1-16.
#'
#' Ravn, M.O., & Uhlig, H. (2002). On adjusting the Hodrick-Prescott filter
#' for the frequency of observations. \emph{Review of Economics and Statistics}, 84(2), 371-376.
#'
#' @export
hp_filter <- function(x, lambda = 1600, as_dt = TRUE) {
  if (!is.numeric(x)) stop("'x' must be a numeric vector", call. = FALSE)
  x <- as.numeric(x)
  if (anyNA(x)) stop("NA values not allowed", call. = FALSE)
  if (length(x) < 5) stop("Series too short (minimum 5 observations required)", call. = FALSE)
  if (lambda <= 0) stop("'lambda' must be positive", call. = FALSE)

  # Use internal wrapper function
  res <- .hp_core(x, as.double(lambda))

  if (as_dt) {
    out <- data.table::data.table(
      original = x,
      trend = as.numeric(res$trend),
      cycle = as.numeric(res$cycle)
    )
    data.table::setattr(out, "lambda", lambda)
    return(out)
  }

  list(
    original = x, trend = as.numeric(res$trend),
    cycle = as.numeric(res$cycle), lambda = lambda
  )
}


#' Batch Modified HP Filter
#'
#' @description
#' Process multiple time series efficiently using the Modified HP Filter.
#' Optimized for processing large collections of time series (e.g., panel data).
#'
#' @param X Matrix or data.frame. Each column is a separate time series.
#'   Rows represent time periods, columns represent different series.
#' @param max_lambda Integer. Maximum lambda value to search. Default 100000.
#'
#' @return
#' A data.table in long format with columns:
#' \describe{
#'   \item{series}{Series identifier (column name or V1, V2, ...)}
#'   \item{t}{Time index (1, 2, ..., T)}
#'   \item{original}{Original series values}
#'   \item{trend}{Estimated trend component}
#'   \item{cycle}{Cyclical component}
#' }
#'
#' With attribute \code{"lambdas"} containing a data.table of optimal lambdas
#' and GCV values for each series:
#' \describe{
#'   \item{series}{Series identifier}
#'   \item{lambda}{Optimal lambda for the series}
#'   \item{gcv}{GCV value at optimal lambda}
#' }
#'
#' @details
#' This function efficiently processes multiple series by:
#' 1. Pre-computing the \eqn{K'K} matrix once for all series
#' 2. Performing parallelizable grid search for each series
#' 3. Using optimized C++ routines via RcppArmadillo
#'
#' @examples
#' \donttest{
#' # Example 1: Multiple macroeconomic series
#' set.seed(456)
#' n_periods <- 60
#' n_countries <- 5
#' gdp_matrix <- matrix(nrow = n_periods, ncol = n_countries)
#' colnames(gdp_matrix) <- c("USA", "UK", "Germany", "France", "Japan")
#'
#' # Generate series with different characteristics
#' for (i in 1:n_countries) {
#'   trend <- cumsum(rnorm(n_periods, mean = 0.5, sd = 0.3))
#'   cycle <- rnorm(n_periods, sd = 1 + i * 0.2) # Increasing volatility
#'   gdp_matrix[, i] <- trend + cycle
#' }
#'
#' # Apply batch Modified HP filter
#' results <- mhp_batch(gdp_matrix, max_lambda = 10000)
#'
#' # Extract optimal lambdas
#' lambdas <- attr(results, "lambdas")
#' print(lambdas)
#'
#' # Example 2: Sectoral data
#' set.seed(789)
#' n_time <- 120
#' n_sectors <- 8
#' sector_names <- c(
#'   "Agriculture", "Mining", "Manufacturing", "Construction",
#'   "Trade", "Transport", "Finance", "Services"
#' )
#' sector_data <- matrix(rnorm(n_time * n_sectors), nrow = n_time)
#'
#' # Add sector-specific trends and cycles
#' for (i in 1:n_sectors) {
#'   trend_growth <- runif(1, 0.2, 1.0)
#'   cycle_amplitude <- runif(1, 0.5, 3.0)
#'   sector_data[, i] <- cumsum(rnorm(n_time, mean = trend_growth / 4, sd = 0.3)) +
#'     cycle_amplitude * sin(2 * pi * (1:n_time) / (20 + i * 5))
#' }
#' colnames(sector_data) <- sector_names
#'
#' sector_results <- mhp_batch(sector_data, max_lambda = 50000)
#'
#' # View results for first few periods
#' head(sector_results)
#' }
#'
#' @export
mhp_batch <- function(X, max_lambda = 100000L) {
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) stop("X must be matrix or data.frame", call. = FALSE)
  if (!is.numeric(X)) stop("X must be numeric", call. = FALSE)
  if (anyNA(X)) stop("NA values not allowed", call. = FALSE)
  if (nrow(X) < 5) stop("Series too short (minimum 5 observations required)", call. = FALSE)

  # Use internal wrapper function
  res <- .mhp_batch_cpp(X, as.integer(max_lambda))

  nms <- colnames(X)
  if (is.null(nms)) nms <- paste0("V", seq_len(ncol(X)))

  out <- data.table::data.table(
    series = rep(nms, each = nrow(X)),
    t = rep(seq_len(nrow(X)), ncol(X)),
    original = as.numeric(X),
    trend = as.numeric(res$trends),
    cycle = as.numeric(res$cycles)
  )

  lam_dt <- data.table::data.table(
    series = nms,
    lambda = as.integer(res$lambdas),
    gcv = as.numeric(res$gcvs)
  )
  data.table::setattr(out, "lambdas", lam_dt)
  out
}


#' Compare HP vs Modified HP Filter
#'
#' @description
#' Compare the standard HP filter with the Modified HP filter for a single series.
#' Provides summary statistics for both methods including cycle properties.
#'
#' @param x Numeric vector. The time series to analyze.
#' @param frequency Character. Data frequency: "quarterly" (lambda=1600) or "annual" (lambda=100).
#' @param max_lambda Integer. Maximum lambda for Modified HP search. Default 100000.
#'
#' @return
#' A data.table with comparison statistics for both methods:
#' \describe{
#'   \item{method}{"HP" or "Modified HP"}
#'   \item{lambda}{Smoothing parameter used}
#'   \item{cycle_sd}{Standard deviation of cyclical component}
#'   \item{cycle_mean}{Mean of cyclical component}
#'   \item{ar1}{First-order autocorrelation of cyclical component}
#'   \item{cycle_range}{Range of cyclical component (max - min)}
#'   \item{gcv}{GCV value (NA for standard HP)}
#' }
#'
#' @details
#' The comparison includes:
#' 1. Standard HP filter with fixed lambda (1600 for quarterly, 100 for annual)
#' 2. Modified HP filter with GCV-optimized lambda
#'
#' Statistics calculated on the cyclical component help assess filter performance:
#' - Lower cycle SD suggests smoother trend
#' - AR(1) near 0 suggests successful cycle extraction
#' - Near-zero mean suggests proper centering
#'
#' @examples
#' # Example 1: Quarterly GDP-like series
#' set.seed(789)
#' n <- 100
#' gdp <- cumsum(rnorm(n, mean = 0.7, sd = 0.5)) + 2 * cos(2 * pi * (1:n) / 32)
#' comparison <- mhp_compare(gdp, frequency = "quarterly", max_lambda = 10000)
#' print(comparison)
#'
#' # Example 2: Annual series
#' set.seed(101)
#' n_annual <- 50
#' annual_series <- cumsum(rnorm(n_annual, mean = 2.0, sd = 1.0)) +
#'   3 * sin(2 * pi * (1:n_annual) / 10)
#' annual_comparison <- mhp_compare(annual_series, frequency = "annual", max_lambda = 5000)
#' print(annual_comparison)
#'
#' # Example 3: Visual comparison
#' set.seed(2023)
#' test_series <- cumsum(rnorm(120, mean = 0.5, sd = 0.4)) +
#'   runif(1, 1, 3) * sin(2 * pi * (1:120) / 30)
#'
#' comp_result <- mhp_compare(test_series, frequency = "quarterly", max_lambda = 20000)
#'
#' if (require(ggplot2)) {
#'   # Create visualization
#'   hp_result <- hp_filter(test_series, lambda = 1600, as_dt = FALSE)
#'   mhp_result <- mhp_filter(test_series, max_lambda = 20000, as_dt = FALSE)
#'
#'   plot_comparison(test_series, frequency = "quarterly", max_lambda = 20000)
#' }
#'
#' @export
mhp_compare <- function(x, frequency = c("quarterly", "annual"),
                        max_lambda = 100000L) {
  frequency <- match.arg(frequency)
  fixed_lam <- if (frequency == "quarterly") 1600 else 100

  hp <- hp_filter(x, lambda = fixed_lam, as_dt = FALSE)
  mhp <- mhp_filter(x, max_lambda = max_lambda, as_dt = FALSE)

  data.table::data.table(
    method = c("HP", "Modified HP"),
    lambda = c(fixed_lam, mhp$lambda),
    cycle_sd = c(collapse::fsd(hp$cycle), collapse::fsd(mhp$cycle)),
    cycle_mean = c(collapse::fmean(hp$cycle), collapse::fmean(mhp$cycle)),
    ar1 = c(.ar1(hp$cycle), .ar1(mhp$cycle)),
    cycle_range = c(diff(range(hp$cycle)), diff(range(mhp$cycle))),
    gcv = c(NA_real_, mhp$gcv)
  )
}


#' Batch Comparison of HP vs Modified HP
#'
#' @description
#' Compare HP and Modified HP filters across multiple time series.
#' Useful for panel data analysis and method validation.
#'
#' @param X Matrix or data.frame. Each column is a separate time series.
#' @param frequency Character. Data frequency: "quarterly" or "annual".
#' @param max_lambda Integer. Maximum lambda for Modified HP search. Default 100000.
#'
#' @return
#' A data.table with comparison metrics for each series:
#' \describe{
#'   \item{series}{Series identifier}
#'   \item{hp_lambda}{Lambda used for HP filter (1600 or 100)}
#'   \item{mhp_lambda}{Optimal lambda from Modified HP}
#'   \item{hp_cycle_sd}{Cycle standard deviation (HP)}
#'   \item{mhp_cycle_sd}{Cycle standard deviation (Modified HP)}
#'   \item{sd_diff}{Difference in cycle SD (MHP - HP)}
#'   \item{hp_ar1}{Cycle AR(1) coefficient (HP)}
#'   \item{mhp_ar1}{Cycle AR(1) coefficient (Modified HP)}
#'   \item{ar1_diff}{Difference in AR(1) (MHP - HP)}
#'   \item{relative_sd}{mhp_cycle_sd / hp_cycle_sd}
#' }
#'
#' @details
#' For each series in X, this function:
#' 1. Applies standard HP filter with frequency-appropriate lambda
#' 2. Applies Modified HP filter with GCV optimization
#' 3. Calculates comparison statistics on cyclical components
#'
#' The comparison helps identify:
#' - Series where Modified HP substantially changes cycle properties
#' - Optimal lambdas across different types of series
#' - Relative performance of automatic vs fixed smoothing
#'
#' @examples
#' # Example 1: Country GDP comparison
#' set.seed(101)
#' n <- 80
#' countries <- c("USA", "UK", "Japan", "Germany", "France", "Italy", "Canada", "Australia")
#' gdp_data <- sapply(countries, function(ctry) {
#'   # Varying volatility and persistence
#'   vol <- runif(1, 0.5, 2.5)
#'   persist <- runif(1, 0.6, 0.95)
#'   trend <- cumsum(rnorm(n, 0.5, 0.3))
#'   cycle <- arima.sim(list(ar = persist), n, sd = vol)
#'   trend + cycle
#' })
#'
#' results <- batch_compare(gdp_data, frequency = "quarterly", max_lambda = 10000)
#' print(results)
#'
#' # Example 2: Sectoral analysis with visualization
#' set.seed(2024)
#' n_time <- 100
#' sectors <- c("Tech", "Finance", "Energy", "Healthcare", "Consumer")
#' sector_returns <- matrix(rnorm(n_time * length(sectors)), nrow = n_time)
#'
#' # Add sector-specific characteristics
#' for (i in 1:length(sectors)) {
#'   drift <- runif(1, -0.1, 0.3)
#'   volatility <- runif(1, 0.5, 2.0)
#'   sector_returns[, i] <- cumsum(rnorm(n_time, mean = drift / 100, sd = volatility / 100)) +
#'     runif(1, 0.5, 2) * sin(2 * pi * (1:n_time) / (20 + i * 3))
#' }
#' colnames(sector_returns) <- sectors
#'
#' sector_comparison <- batch_compare(sector_returns, frequency = "quarterly", max_lambda = 5000)
#'
#' if (require(ggplot2)) {
#'   # Plot lambda comparison
#'   lambda_plot <- ggplot2::ggplot(
#'     sector_comparison,
#'     ggplot2::aes(x = series, y = mhp_lambda)
#'   ) +
#'     ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
#'     ggplot2::geom_hline(yintercept = 1600, linetype = "dashed", color = "red") +
#'     ggplot2::labs(
#'       title = "Modified HP Optimal Lambdas by Sector",
#'       subtitle = "Red line shows fixed HP lambda (1600)",
#'       x = "Sector", y = "Optimal Lambda"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
#'
#'   print(lambda_plot)
#' }
#'
#' @references
#' Choudhary, M.A., Hanif, M.N., & Iqbal, J. (2014). On smoothing macroeconomic
#' time series using the modified HP filter. \emph{Applied Economics}, 46(19),
#' 2205-2214.
#'
#' @export
batch_compare <- function(X, frequency = c("quarterly", "annual"),
                          max_lambda = 100000L) {
  frequency <- match.arg(frequency)
  fixed_lam <- if (frequency == "quarterly") 1600 else 100

  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) stop("X must be matrix or data.frame", call. = FALSE)

  nms <- colnames(X)
  if (is.null(nms)) nms <- paste0("V", seq_len(ncol(X)))

  # Use internal wrapper function
  mhp_res <- .mhp_batch_cpp(X, as.integer(max_lambda))

  results <- vector("list", ncol(X))
  for (j in seq_len(ncol(X))) {
    x <- X[, j]
    # Use internal wrapper function
    hp_res <- .hp_core(x, fixed_lam)
    hp_cycle <- x - as.numeric(hp_res$trend)
    mhp_cycle <- as.numeric(mhp_res$cycles[, j])

    hp_sd <- collapse::fsd(hp_cycle)
    mhp_sd <- collapse::fsd(mhp_cycle)

    results[[j]] <- data.table::data.table(
      series = nms[j],
      hp_lambda = fixed_lam,
      mhp_lambda = as.integer(mhp_res$lambdas[j]),
      hp_cycle_sd = hp_sd,
      mhp_cycle_sd = mhp_sd,
      sd_diff = mhp_sd - hp_sd,
      hp_ar1 = .ar1(hp_cycle),
      mhp_ar1 = .ar1(mhp_cycle),
      ar1_diff = .ar1(mhp_cycle) - .ar1(hp_cycle),
      relative_sd = ifelse(hp_sd > 0, mhp_sd / hp_sd, NA_real_)
    )
  }
  data.table::rbindlist(results)
}


#' Extract Optimal Lambda
#'
#' @description
#' Extract the optimal smoothing parameter lambda from Modified HP filter results.
#'
#' @param x Result from \code{\link{mhp_filter}} (either data.table or mhp object).
#'
#' @return Integer. The optimal lambda value.
#'
#' @examples
#' set.seed(123)
#' result <- mhp_filter(cumsum(rnorm(100)), max_lambda = 10000)
#' get_lambda(result)
#'
#' # With mhp object
#' result_obj <- mhp_filter(cumsum(rnorm(100)), max_lambda = 10000, as_dt = FALSE)
#' get_lambda(result_obj)
#'
#' @export
get_lambda <- function(x) {
  lam <- attr(x, "lambda")
  if (is.null(lam) && is.list(x)) lam <- x$lambda
  if (is.null(lam)) stop("No lambda found", call. = FALSE)
  lam
}


#' Extract GCV Value
#'
#' @description
#' Extract the generalized cross-validation (GCV) value from Modified HP filter results.
#'
#' @param x Result from \code{\link{mhp_filter}} (either data.table or mhp object).
#'
#' @return Numeric. The GCV value at the optimal lambda.
#'
#' @examples
#' set.seed(123)
#' result <- mhp_filter(cumsum(rnorm(100)), max_lambda = 10000)
#' get_gcv(result)
#'
#' # With mhp object
#' result_obj <- mhp_filter(cumsum(rnorm(100)), max_lambda = 10000, as_dt = FALSE)
#' get_gcv(result_obj)
#'
#' @export
get_gcv <- function(x) {
  gcv <- attr(x, "gcv")
  if (is.null(gcv) && is.list(x)) gcv <- x$gcv
  if (is.null(gcv)) stop("No GCV found", call. = FALSE)
  gcv
}


#' Plot Method for mhp Objects
#'
#' @description
#' Create a publication-quality ggplot2 visualization of Modified HP filter results.
#'
#' @param object An object of class "mhp" from \code{mhp_filter(as_dt = FALSE)}.
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @details
#' Creates a three-panel plot showing:
#' 1. Original series with trend overlay
#' 2. Trend component
#' 3. Cyclical component
#'
#' The plot includes optimal lambda and GCV in the title, and uses
#' consistent formatting suitable for publications.
#'
#' @examples
#' set.seed(42)
#' n <- 120
#' # Create a realistic macroeconomic series
#' trend <- cumsum(c(0, rnorm(n - 1, mean = 0.5, sd = 0.3)))
#' cycle <- 3 * sin(2 * pi * (1:n) / 30) + rnorm(n, sd = 0.8)
#' y <- trend + cycle + 100 # Add level for realism
#'
#' result <- mhp_filter(y, max_lambda = 10000, as_dt = FALSE)
#'
#' if (require(ggplot2)) {
#'   # Basic plot
#'   autoplot(result)
#'
#'   # Customized plot
#'   p <- autoplot(result)
#'   p <- p +
#'     ggplot2::theme(
#'       plot.title = ggplot2::element_text(size = 14, face = "bold"),
#'       strip.text = ggplot2::element_text(size = 12, face = "bold")
#'     ) +
#'     ggplot2::labs(caption = "Data: Simulated macroeconomic series")
#'   print(p)
#' }
#'
#' @method autoplot mhp
#' @importFrom ggplot2 autoplot
#' @export
autoplot.mhp <- function(object, ...) {
  n <- length(object$original)

  plot_data <- data.table::data.table(
    t = rep(seq_len(n), 3),
    value = c(object$original, object$trend, object$cycle),
    component = rep(c("Original", "Trend", "Cycle"), each = n)
  )
  plot_data$component <- factor(plot_data$component,
    levels = c("Original", "Trend", "Cycle")
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = t, y = value, color = component)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, linewidth = 0.3) +
    ggplot2::scale_color_manual(values = c(
      "Original" = "gray40",
      "Trend" = "darkblue",
      "Cycle" = "darkred"
    )) +
    ggplot2::facet_wrap(~component, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Modified HP Filter Decomposition (lambda = ", object$lambda, ")"),
      subtitle = paste0("GCV = ", format(object$gcv, digits = 4, scientific = TRUE)),
      x = "Time Period", y = "Value", color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle = ggplot2::element_text(color = "gray40", size = 10),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.5, "lines")
    ) +
    ggplot2::scale_x_continuous(expand = c(0.02, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.05, 0.05))
}


#' Plot Comparison of HP vs Modified HP
#'
#' @description
#' Create a ggplot2 comparison of HP and Modified HP filter trends.
#' Useful for visualizing differences between fixed and optimized smoothing.
#'
#' @param x Numeric vector. The time series.
#' @param frequency Character. Data frequency: "quarterly" or "annual".
#' @param max_lambda Integer. Maximum lambda for Modified HP search.
#' @param show_cycle Logical. If TRUE, also show cyclical components.
#'
#' @return A ggplot object.
#'
#' @details
#' Creates comparison plots showing:
#' 1. Original series with HP and Modified HP trends overlaid
#' 2. (Optional) Cyclical components from both methods
#'
#' The plot uses distinct colors and line styles to differentiate methods,
#' with annotations showing lambda values.
#'
#' @examples
#' set.seed(123)
#' # Simulate realistic economic data
#' n <- 100
#' base_level <- 100
#' growth_rate <- 0.5
#' volatility <- 1.2
#'
#' y <- base_level + cumsum(rnorm(n, mean = growth_rate / 100, sd = volatility / 100)) +
#'   2.5 * sin(2 * pi * (1:n) / 25) + rnorm(n, sd = 0.5)
#'
#' if (require(ggplot2)) {
#'   # Basic comparison
#'   plot_comparison(y, frequency = "quarterly", max_lambda = 10000)
#'
#'   # With cycles
#'   plot_comparison(y, frequency = "quarterly", max_lambda = 10000, show_cycle = TRUE)
#'
#'   # Customized plot
#'   p <- plot_comparison(y, frequency = "quarterly", max_lambda = 10000)
#'   p <- p +
#'     ggplot2::labs(
#'       title = "HP vs Modified HP: Trend Comparison",
#'       subtitle = "Quarterly macroeconomic series"
#'     ) +
#'     ggplot2::theme(
#'       plot.title = ggplot2::element_text(face = "bold", size = 14),
#'       legend.title = ggplot2::element_blank(),
#'       legend.position = "bottom"
#'     )
#'   print(p)
#' }
#'
#' @export
plot_comparison <- function(x, frequency = c("quarterly", "annual"),
                            max_lambda = 100000L, show_cycle = FALSE) {
  frequency <- match.arg(frequency)
  fixed_lam <- if (frequency == "quarterly") 1600 else 100

  hp <- hp_filter(x, lambda = fixed_lam, as_dt = FALSE)
  mhp <- mhp_filter(x, max_lambda = max_lambda, as_dt = FALSE)

  n <- length(x)

  if (show_cycle) {
    # Show both trends and cycles
    plot_data <- data.table::data.table(
      t = rep(seq_len(n), 5),
      value = c(x, hp$trend, mhp$trend, hp$cycle, mhp$cycle),
      series = rep(c(
        "Original",
        paste0("HP Trend (lambda=", fixed_lam, ")"),
        paste0("MHP Trend (lambda=", mhp$lambda, ")"),
        paste0("HP Cycle (lambda=", fixed_lam, ")"),
        paste0("MHP Cycle (lambda=", mhp$lambda, ")")
      ), each = n),
      type = rep(c("Original", "Trend", "Trend", "Cycle", "Cycle"), each = n)
    )

    plot_data$type <- factor(plot_data$type, levels = c("Trend", "Cycle", "Original"))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = t, y = value, color = series,
      linetype = series, linewidth = series
    )) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~type, ncol = 1, scales = "free_y") +
      ggplot2::scale_color_manual(values = setNames(
        c("gray40", "red", "blue", "orange", "purple"),
        c(
          "Original",
          paste0("HP Trend (lambda=", fixed_lam, ")"),
          paste0("MHP Trend (lambda=", mhp$lambda, ")"),
          paste0("HP Cycle (lambda=", fixed_lam, ")"),
          paste0("MHP Cycle (lambda=", mhp$lambda, ")")
        )
      )) +
      ggplot2::scale_linetype_manual(values = setNames(
        c("solid", "solid", "dashed", "solid", "dashed"),
        c(
          "Original",
          paste0("HP Trend (lambda=", fixed_lam, ")"),
          paste0("MHP Trend (lambda=", mhp$lambda, ")"),
          paste0("HP Cycle (lambda=", fixed_lam, ")"),
          paste0("MHP Cycle (lambda=", mhp$lambda, ")")
        )
      )) +
      ggplot2::scale_linewidth_manual(values = setNames(
        c(0.8, 1, 1, 0.8, 0.8),
        c(
          "Original",
          paste0("HP Trend (lambda=", fixed_lam, ")"),
          paste0("MHP Trend (lambda=", mhp$lambda, ")"),
          paste0("HP Cycle (lambda=", fixed_lam, ")"),
          paste0("MHP Cycle (lambda=", mhp$lambda, ")")
        )
      )) +
      ggplot2::labs(
        title = "HP vs Modified HP Filter Comparison",
        subtitle = paste0("Fixed lambda = ", fixed_lam, " vs Optimized lambda = ", mhp$lambda),
        x = "Time Period", y = "Value", color = "Series", linetype = "Series"
      )
  } else {
    # Show only trends with original
    plot_data <- data.table::data.table(
      t = rep(seq_len(n), 3),
      value = c(x, hp$trend, mhp$trend),
      series = rep(c(
        "Original",
        paste0("HP (lambda=", fixed_lam, ")"),
        paste0("MHP (lambda=", mhp$lambda, ")")
      ), each = n)
    )

    plot_data$series <- factor(plot_data$series, levels = unique(plot_data$series))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = t, y = value, color = series,
      linetype = series
    )) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_color_manual(values = setNames(
        c("gray40", "red", "blue"),
        c(
          "Original",
          paste0("HP (lambda=", fixed_lam, ")"),
          paste0("MHP (lambda=", mhp$lambda, ")")
        )
      )) +
      ggplot2::scale_linetype_manual(values = setNames(
        c("solid", "solid", "dashed"),
        c(
          "Original",
          paste0("HP (lambda=", fixed_lam, ")"),
          paste0("MHP (lambda=", mhp$lambda, ")")
        )
      )) +
      ggplot2::labs(
        title = "HP vs Modified HP Filter Comparison",
        subtitle = paste0("Fixed lambda = ", fixed_lam, " vs Optimized lambda = ", mhp$lambda),
        x = "Time Period", y = "Value", color = "Method", linetype = "Method"
      )
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle = ggplot2::element_text(color = "gray40", size = 10),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "gray90"),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}


#' Plot Batch Results
#'
#' @description
#' Create a ggplot2 visualization of batch filter results.
#' Useful for comparing multiple series' cyclical components or trends.
#'
#' @param x Result from \code{\link{mhp_batch}}.
#' @param show Character. What to show: "cycle" (default) or "trend".
#' @param facet Logical. If TRUE, use faceting; if FALSE, overlay series.
#' @param highlight Character vector. Names of series to highlight (others shown faintly).
#'
#' @return A ggplot object.
#'
#' @details
#' Creates visualizations for batch processing results:
#' - Cycle plot: Shows business cycle components across series
#' - Trend plot: Shows trend components across series
#'
#' Options for faceting or overlay, with highlighting capability.
#'
#' @examples
#' set.seed(456)
#' # Create multi-country dataset
#' n_time <- 80
#' countries <- c("USA", "UK", "Germany", "France", "Japan", "Canada")
#' gdp_data <- matrix(nrow = n_time, ncol = length(countries))
#'
#' for (i in seq_along(countries)) {
#'   # Different growth rates and cycle patterns
#'   growth <- runif(1, 0.3, 1.0)
#'   cycle_freq <- 20 + runif(1, -5, 15)
#'   cycle_amp <- runif(1, 0.5, 2.5)
#'
#'   gdp_data[, i] <- 100 + cumsum(rnorm(n_time, mean = growth / 100, sd = 0.4 / 100)) +
#'     cycle_amp * sin(2 * pi * (1:n_time) / cycle_freq)
#' }
#' colnames(gdp_data) <- countries
#'
#' results <- mhp_batch(gdp_data, max_lambda = 10000)
#'
#' if (require(ggplot2)) {
#'   # Show cycles with faceting
#'   plot_batch(results, show = "cycle", facet = TRUE)
#'
#'   # Show trends overlaid
#'   plot_batch(results, show = "trend", facet = FALSE)
#'
#'   # Highlight specific countries
#'   plot_batch(results, show = "cycle", facet = FALSE, highlight = c("USA", "Germany"))
#'
#'   # Customized plot
#'   p <- plot_batch(results, show = "cycle", facet = TRUE)
#'   p <- p +
#'     ggplot2::labs(
#'       title = "Business Cycle Components: Selected Countries",
#'       subtitle = "Modified HP Filter Decomposition"
#'     ) +
#'     ggplot2::theme(
#'       strip.text = ggplot2::element_text(face = "bold", size = 9),
#'       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
#'     )
#'   print(p)
#' }
#'
#' @export
plot_batch <- function(x, show = c("cycle", "trend"), facet = TRUE, highlight = NULL) {
  show <- match.arg(show)

  plot_data <- x[, .(t, series, value = get(show))]

  lambdas <- attr(x, "lambdas")
  plot_data <- merge(plot_data, lambdas[, .(series, lambda)], by = "series")

  # Create label with lambda
  plot_data$label <- paste0(plot_data$series, " (lambda=", plot_data$lambda, ")")

  if (!is.null(highlight)) {
    # Highlight specified series
    plot_data$highlight <- plot_data$series %in% highlight
    alpha_vals <- ifelse(plot_data$highlight, 1, 0.3)
    linewidth_vals <- ifelse(plot_data$highlight, 1, 0.5)
  } else {
    plot_data$highlight <- TRUE
    alpha_vals <- 1
    linewidth_vals <- 0.8
  }

  if (facet) {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = t, y = value)) +
      ggplot2::geom_line(linewidth = 0.7, alpha = alpha_vals, linewidth = linewidth_vals) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
      ggplot2::facet_wrap(~label, scales = "free_y") +
      ggplot2::labs(
        title = paste0(
          "Modified HP Filter - ",
          ifelse(show == "cycle", "Cyclical", "Trend"), " Components"
        ),
        x = "Time Period", y = ifelse(show == "cycle", "Cycle Value", "Trend Value")
      )
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = t, y = value, color = label,
      alpha = highlight, linewidth = highlight
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
      ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3), guide = "none") +
      ggplot2::scale_linewidth_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = "none") +
      ggplot2::labs(
        title = paste0(
          "Modified HP Filter - ",
          ifelse(show == "cycle", "Cyclical", "Trend"), " Components"
        ),
        x = "Time Period", y = ifelse(show == "cycle", "Cycle Value", "Trend Value"),
        color = "Series (lambda)"
      )
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = ifelse(facet, "none", "bottom"),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2, color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 9)
    )

  return(p)
}


#' Print Method for mhp Objects
#'
#' @description
#' Print a summary of Modified HP filter results.
#'
#' @param x An object of class "mhp" from \code{mhp_filter(as_dt = FALSE)}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the input object.
#'
#' @details
#' Prints a formatted summary including:
#' - Number of observations
#' - Optimal lambda value
#' - GCV criterion value
#' - Cycle statistics (mean, SD, AR1)
#'
#' Suitable for quick inspection of filter results.
#'
#' @examples
#' set.seed(42)
#' y <- cumsum(rnorm(100)) + sin((1:100) * pi / 20)
#' result <- mhp_filter(y, max_lambda = 10000, as_dt = FALSE)
#' print(result)
#'
#' @export
print.mhp <- function(x, ...) {
  cat("Modified HP Filter Result\n")
  cat(strrep("=", 30), "\n")
  cat("Observations:", length(x$original), "\n")
  cat("Optimal lambda:", x$lambda, "\n")
  cat("GCV:", format(x$gcv, digits = 4, scientific = TRUE), "\n")
  cat("\nCycle Statistics:\n")
  cat("  Mean:", format(collapse::fmean(x$cycle), digits = 4), "\n")
  cat("  SD:  ", format(collapse::fsd(x$cycle), digits = 4), "\n")
  cat("  AR1: ", format(.ar1(x$cycle), digits = 4), "\n")
  cat("  Range:", format(diff(range(x$cycle)), digits = 4), "\n")
  invisible(x)
}
