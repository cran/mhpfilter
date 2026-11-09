#-------------------------------------------------------------------------------------------------------------------
# Printing and plots of the results.
#-------------------------------------------------------------------------------------------------------------------

.mhp_palette <- c(series = "#6B6B6B", HP = "#C0392B", `Modified HP` = "#1F4E9D", `Fully modified HP` = "#1E8449")

# the time axis of a result: its ts time when there is one, otherwise 1, ..., T
.mhp_time <- function(x) if (!is.null(x$time)) x$time else seq_along(x$original)

# the name of the filter of a result
.mhp_method <- function(x) {
  if (inherits(x, "mhp_fmhp")) "Fully modified HP" else if (inherits(x, "mhp_mhp")) "Modified HP" else "HP"
}

#' Print a filter result
#'
#' @description
#' \code{print()} of a result of \code{\link{hp_filter}()}, \code{\link{mhp_filter}()} or
#' \code{\link{fmhp_filter}()} (with \code{as_dt = FALSE}) shows the smoothing parameter, the criterion and summaries
#' of the cycle, as in version 0.1.0, and the sources of the method.
#'
#' @param x a result of \code{hp_filter()}, \code{mhp_filter()} or \code{fmhp_filter()} with \code{as_dt = FALSE}.
#' @param ... further arguments (\code{digits}).
#' @return \code{x}, invisibly.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' data(us_macro)
#' m <-
#'   mhp_filter(
#'       x          = log(us_macro$gdp)
#'     , max_lambda = 100000
#'     , as_dt      = FALSE
#'     , log_scale  = FALSE
#'     , criterion  = c("approximate", "exact")
#'     )
#' print(x = m)
#' @export
print.mhp <- function(x, ...) {
  if (inherits(x, "mhp_result")) return(print.mhp_result(x, ...))
  .mhp_print_filter(x)
  invisible(x)
}

.mhp_print_filter <- function(x, digits = 4L, ...) {
  title <- paste(.mhp_method(x), "Filter Result")
  cat(title, "\n", strrep("=", 30), "\n", sep = "")
  cat("Observations:", length(x$original), "\n")
  if (inherits(x, "mhp_fmhp")) {
    cat("Fully modified lambda:", format(x$lambda), " (modified HP lambda:", format(x$lambda_mhp), ")\n")
    cat(sprintf("End-point weights: %s, alpha = %s on %d end values; loss = %.4f\n", x$scheme, format(x$alpha), x$k,
                x$loss))
  } else if (inherits(x, "mhp_mhp")) {
    cat("Optimal lambda:", x$lambda, "\n")
  } else {
    cat("Lambda:", x$lambda, "\n")
  }
  if (!is.null(x$gcv) && !is.na(x$gcv)) cat("GCV:", format(x$gcv, digits = digits, scientific = TRUE), "\n")
  if (isTRUE(x$log_scale)) cat("Filtered on the log scale; cycle = series - trend in levels\n")
  cat("\nCycle Statistics:\n")
  cat("  Mean:", format(mean(x$cycle), digits = digits), "\n")
  cat("  SD:  ", format(stats::sd(x$cycle), digits = digits), "\n")
  cat("  AR1: ", format(.mhp_ar1(x$cycle), digits = digits), "\n")
  cat("  Range:", format(diff(range(x$cycle)), digits = digits), "\n")
  invisible(x)
}
.mhp_print_hp   <- .mhp_print_filter
.mhp_print_mhp  <- .mhp_print_filter
.mhp_print_fmhp <- .mhp_print_filter

#' Plot the decomposition of a series
#'
#' @description
#' \code{autoplot()} (and \code{plot()}) of a filter result draws the series with its trend and, below, the cycle,
#' with the time of a \code{ts} on the horizontal axis. For the modified HP filter \code{which = "gcv"} draws the
#' criterion against \eqn{\lambda}{lambda}; for the fully modified HP filter \code{which = "lambda"} draws the
#' smoothing parameters \eqn{\lambda_t}{lambda_t} and \code{which = "loss"} the end-point loss.
#'
#' @param object a result of \code{\link{hp_filter}()}, \code{\link{mhp_filter}()} or
#'   \code{\link{fmhp_filter}()} with \code{as_dt = FALSE}.
#' @param which \code{"decomposition"} (default), \code{"gcv"} (modified HP), \code{"lambda"} or \code{"loss"}
#'   (fully modified HP).
#' @param ... not used.
#' @return A \code{ggplot} object.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' data(us_macro)
#' f <-
#'   fmhp_filter(
#'       x               = us_macro$gdp
#'     , max_lambda      = 3000
#'     , k_linear        = 15
#'     , k_quadratic     = NULL
#'     , alpha_linear    = seq(from = 0, to = 2000, by = 10)
#'     , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
#'     , schemes         = c("linear", "quadratic")
#'     , form            = "rows"
#'     , lambda          = NULL
#'     , log_scale       = TRUE
#'     , as_dt           = FALSE
#'     )
#' ggplot2::autoplot(object = f)
#' plot(x = f, which = "lambda")
#' @rawNamespace S3method(ggplot2::autoplot, mhp)
autoplot.mhp <- function(object, which = c("decomposition", "gcv", "lambda", "loss"), ...) {
  which <- .mhp_check_choice(which[1L], c("decomposition", "gcv", "lambda", "loss"), "which", "autoplot")
  x  <- object
  tt <- .mhp_time(x)
  if (which == "gcv") {
    if (is.null(x$gcv_path) || !length(x$gcv_path)) .mhp_stop("autoplot", "`which = \"gcv\"` needs a modified HP result.")
    d <- data.table::data.table(lambda = seq_along(x$gcv_path), gcv = x$gcv_path)
    return(ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = lambda, y = gcv)) +
             ggplot2::geom_line(colour = .mhp_palette[["Modified HP"]]) +
             ggplot2::geom_vline(xintercept = x$lambda, linetype = "dashed") +
             ggplot2::scale_x_log10(breaks = scales::breaks_log(n = 6), labels = scales::label_comma()) +
             .mhp_y_cont() +
             ggplot2::labs(x = "lambda (log scale)", y = "criterion",
                           title = sprintf("McDermott's criterion; minimum at lambda = %d", as.integer(x$lambda))) +
             mhp_theme())
  }
  if (which %in% c("lambda", "loss")) {
    if (!inherits(x, "mhp_fmhp")) .mhp_stop("autoplot", "`which = \"", which, "\"` needs a fully modified HP result.")
    if (which == "loss") return(.mhp_plot_loss(hp_loss.default(x)))
    d <- data.table::data.table(t = seq_along(x$lambda_t), lambda = x$lambda_t)
    return(ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = t, y = lambda)) +
             ggplot2::geom_step(colour = .mhp_palette[["Fully modified HP"]]) +
             .mhp_x_cont(integer = TRUE) + .mhp_y_cont() +
             ggplot2::labs(x = if (x$form == "rows") "observation" else "second difference",
                           y = "smoothing parameter",
                           title = sprintf("Smoothing parameters: lambda = %s, %s weights, alpha = %s, k = %d",
                                           format(x$lambda), x$scheme, format(x$alpha), x$k)) +
             mhp_theme())
  }
  meth <- .mhp_method(x)
  d <- data.table::rbindlist(list(
      data.table::data.table(time = tt, value = x$original, line = "series", panel = "Series and trend")
    , data.table::data.table(time = tt, value = x$trend, line = meth, panel = "Series and trend")
    , data.table::data.table(time = tt, value = x$cycle, line = meth, panel = "Cycle")
    ))
  d[, panel := factor(panel, levels = c("Series and trend", "Cycle"))]
  lam <- if (inherits(x, "mhp_fmhp")) sprintf("lambda = %s (%s weights)", format(x$lambda), x$scheme) else
    sprintf("lambda = %s", format(x$lambda))
  ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = time, y = value, colour = line)) +
    ggplot2::geom_hline(data = data.table::data.table(panel = factor("Cycle", levels = levels(d$panel)), y = 0),
                        mapping = ggplot2::aes(yintercept = y), linetype = "dashed", colour = "grey60") +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_wrap(facets = ggplot2::vars(panel), ncol = 1L, scales = "free_y") +
    ggplot2::scale_colour_manual(values = .mhp_palette, name = NULL) +
    .mhp_x_cont() + .mhp_y_cont() +
    ggplot2::labs(x = if (is.null(x$time)) "time period" else "time", y = NULL,
                  title = sprintf("%s filter: %s", meth, lam)) +
    mhp_theme()
}

.mhp_plot_filter <- function(x, ...) autoplot.mhp(x, ...)
.mhp_plot_hp   <- .mhp_plot_filter
.mhp_plot_mhp  <- .mhp_plot_filter
.mhp_plot_fmhp <- .mhp_plot_filter

#' Plot the HP and modified HP filters of one series
#'
#' @description
#' \code{plot_comparison()} draws the series with the trends of the HP filter (at the conventional
#' \eqn{\lambda}{lambda} of its frequency) and of the modified HP filter, and optionally the cycles; with
#' \code{fmhp = TRUE} the fully modified HP filter is added.
#'
#' @inheritParams mhp_compare
#' @param show_cycle draw the cycles too (default \code{FALSE}).
#' @param fmhp add the fully modified HP filter (default \code{FALSE}; it uses \code{max_lambda = 3000} and the other
#'   defaults of \code{\link{fmhp_filter}()} on the scale of the series).
#' @return A \code{ggplot} object.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' data(us_macro)
#' plot_comparison(
#'     x          = log(us_macro$gdp)
#'   , frequency  = "quarterly"
#'   , max_lambda = 100000
#'   , show_cycle = TRUE
#'   , fmhp       = TRUE
#'   )
#' @export
plot_comparison <- function(x, frequency = c("quarterly", "annual", "monthly"), max_lambda = 100000L,
                            show_cycle = FALSE, fmhp = FALSE) {
  fn <- "plot_comparison"
  frequency  <- .mhp_check_choice(frequency[1L], c("quarterly", "annual", "monthly"), "frequency", fn)
  show_cycle <- .mhp_check_flag(show_cycle, "show_cycle", fn)
  fmhp       <- .mhp_check_flag(fmhp, "fmhp", fn)
  fixed <- .mhp_freq_lambda(frequency)
  fits <- list(HP = hp_filter.default(x, lambda = fixed, as_dt = FALSE),
               `Modified HP` = mhp_filter.default(x, max_lambda = max_lambda, as_dt = FALSE))
  if (fmhp) fits$`Fully modified HP` <- fmhp_filter.default(x, log_scale = FALSE, as_dt = FALSE)
  tt  <- .mhp_time(fits$HP)
  lab <- vapply(names(fits), function(m) sprintf("%s (lambda = %s)", m, format(fits[[m]]$lambda)), "")
  d <- data.table::rbindlist(c(
      list(data.table::data.table(time = tt, value = fits$HP$original, line = "series", panel = "Trend"))
    , lapply(names(fits), function(m) data.table::data.table(time = tt, value = fits[[m]]$trend, line = lab[[m]],
                                                             panel = "Trend"))
    , if (show_cycle) lapply(names(fits), function(m) data.table::data.table(time = tt, value = fits[[m]]$cycle,
                                                                             line = lab[[m]], panel = "Cycle"))
    ))
  d[, panel := factor(panel, levels = c("Trend", "Cycle"))]
  cols <- c(series = .mhp_palette[["series"]], stats::setNames(.mhp_palette[names(fits)], lab))
  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = time, y = value, colour = line)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_colour_manual(values = cols, breaks = c("series", lab), name = NULL) +
    .mhp_x_cont() + .mhp_y_cont() +
    ggplot2::labs(x = if (is.null(fits$HP$time)) "time period" else "time", y = NULL,
                  title = sprintf("HP filter (lambda = %s) and data-driven smoothing", format(fixed))) +
    mhp_theme()
  if (show_cycle) p <- p + ggplot2::facet_wrap(facets = ggplot2::vars(panel), ncol = 1L, scales = "free_y")
  p
}

#' Plot the results of many series
#'
#' @description
#' \code{plot_batch()} draws the cycles (or trends) of a result of \code{\link{mhp_batch}()} or
#' \code{\link{fmhp_batch}()}, one panel per series or all in one panel, with the smoothing parameter of each series
#' in its label.
#'
#' @param x a result of \code{mhp_batch()} or \code{fmhp_batch()}.
#' @param show \code{"cycle"} (default) or \code{"trend"}.
#' @param facet one panel per series (default \code{TRUE}).
#' @param highlight names of series drawn in full colour, the others faded (default \code{NULL}: all).
#' @return A \code{ggplot} object.
#' @details \strong{Change from version 0.1.0.} The faceted plot passed \code{linewidth} twice (a warning) and ignored
#'   \code{highlight}; both are corrected.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' data(us_macro)
#' mb <-
#'   mhp_batch(
#'       X          = log(as.matrix(us_macro[, c("gdp", "consumption", "investment")]))
#'     , max_lambda = 100000
#'     )
#' plot_batch(
#'     x         = mb
#'   , show      = "cycle"
#'   , facet     = TRUE
#'   , highlight = NULL
#'   )
#' @export
plot_batch <- function(x, show = c("cycle", "trend"), facet = TRUE, highlight = NULL) {
  fn <- "plot_batch"
  show  <- .mhp_check_choice(show[1L], c("cycle", "trend"), "show", fn)
  facet <- .mhp_check_flag(facet, "facet", fn)
  lam <- attr(x, "lambdas")
  if (!data.table::is.data.table(x) || is.null(lam)) .mhp_stop(fn, "`x` must be a result of mhp_batch() or fmhp_batch().")
  d <- data.table::copy(x)[, .(t, series, value = get(show))]
  d <- merge(d, lam[, .(series, lambda)], by = "series", sort = FALSE)
  d[, label := sprintf("%s (lambda = %s)", series, format(lambda))]
  d[, on := if (is.null(highlight)) TRUE else series %in% highlight]
  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = t, y = value)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.3)
  p <- if (facet) {
    p + ggplot2::geom_line(mapping = ggplot2::aes(alpha = on), linewidth = 0.6, colour = .mhp_palette[["Modified HP"]]) +
      ggplot2::facet_wrap(facets = ggplot2::vars(label), scales = "free_y")
  } else {
    p + ggplot2::geom_line(mapping = ggplot2::aes(colour = label, alpha = on), linewidth = 0.7) +
      ggplot2::labs(colour = "series (lambda)")
  }
  p + ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
    .mhp_x_cont(integer = TRUE) + .mhp_y_cont() +
    ggplot2::labs(x = "time period", y = if (show == "cycle") "cycle" else "trend",
                  title = sprintf("%s components", if (show == "cycle") "Cyclical" else "Trend")) +
    mhp_theme(legend_position = if (facet) "none" else "bottom")
}

# ---- diagnostics -----------------------------------------------------------------------------------------------------

.mhp_print_weights <- function(x, digits = 4L, ...) {
  cat(attr(x, "label"), "\n\nWeights of the rows", paste(x$rows, collapse = ", "), "(first 8 columns):\n")
  print(round(x$weights[x$rows, seq_len(min(8L, ncol(x$weights))), drop = FALSE], digits))
  invisible(x)
}

.mhp_plot_weights <- function(x, ...) {
  T <- ncol(x$weights)
  d <- data.table::rbindlist(lapply(x$rows, function(r) {
    data.table::data.table(row = factor(sprintf("t = %d", r), levels = sprintf("t = %d", x$rows)), j = seq_len(T),
                           weight = x$weights[r, ])
  }))
  ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = j, y = weight, colour = row)) +
    ggplot2::geom_line() +
    .mhp_x_cont(integer = TRUE) + .mhp_y_cont() +
    ggplot2::labs(x = "observation j", y = "weight of x_j in the trend at t", colour = NULL,
                  title = "Weighting vectors of the trend estimates") +
    mhp_theme()
}

.mhp_print_gain <- function(x, digits = 4L, ...) {
  cat(attr(x, "label"), "\n\nGain at the frequencies 0, pi/4, pi/2 and pi of the rows", paste(x$rows, collapse = ", "),
      ":\n")
  th <- sort(unique(x$gain$theta))
  pick <- vapply(c(0, pi / 4, pi / 2, pi), function(v) th[which.min(abs(th - v))], 0)
  w <- data.table::dcast(x$gain[t %in% x$rows & theta %in% pick], t ~ theta, value.var = "gain")
  print(w, digits = digits)
  invisible(x)
}

.mhp_plot_gain <- function(x, ...) {
  d <- x$gain[t %in% x$rows]
  d[, row := factor(sprintf("t = %d", t), levels = sprintf("t = %d", x$rows))]
  ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = theta, y = gain, colour = row)) +
    ggplot2::geom_line() +
    .mhp_x_cont() + .mhp_y_cont() +
    ggplot2::labs(x = "frequency (radians)", y = "gain", colour = NULL, title = "Gain functions of the trend estimates") +
    mhp_theme()
}

.mhp_print_loss <- function(x, digits = 4L, ...) {
  cat(attr(x, "label"), "\n\nLoss by time (first and last five):\n")
  n <- nrow(x$loss)
  print(x$loss[unique(c(seq_len(min(5L, n)), seq.int(max(1L, n - 4L), n)))], digits = digits)
  invisible(x)
}

.mhp_plot_loss <- function(x, ...) {
  ggplot2::ggplot(data = x$loss, mapping = ggplot2::aes(x = t, y = loss)) +
    ggplot2::geom_col(fill = .mhp_palette[["Modified HP"]], width = 0.8) +
    .mhp_x_cont(integer = TRUE) + .mhp_y_cont(bars = TRUE) +
    ggplot2::labs(x = "time", y = "loss l(t)", title = sprintf("End-point loss: total L = %.4f", x$total)) +
    mhp_theme()
}

.mhp_print_simulation <- function(x, digits = 3L, ...) {
  cat(attr(x, "label"), "\n\nAverage RMSE of the cycle (whole series, middle 80%, ends 10% each side):\n")
  print(x$rmse[, .(rmse = mean(rmse), rmse_middle = mean(rmse_middle), rmse_ends = mean(rmse_ends)), by = method],
        digits = digits)
  cat("\nShare of replicates in which the fully modified HP filter has the smaller RMSE:\n")
  print(x$power[, .(vs_HP = mean(fmhp_better_hp), vs_modified_HP = mean(fmhp_better_mhp))], digits = digits)
  invisible(x)
}

.mhp_plot_simulation <- function(x, ...) {
  d <- data.table::melt(x$rmse, id.vars = c("model", "sd_ratio", "ar1", "ar2", "method"),
                        measure.vars = c("rmse_middle", "rmse_ends"), variable.name = "part", value.name = "value")
  d[, part := factor(part, levels = c("rmse_middle", "rmse_ends"), labels = c("middle 80%", "ends 20%"))]
  ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = factor(model), y = value, fill = method)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::facet_wrap(facets = ggplot2::vars(part), ncol = 1L) +
    ggplot2::scale_fill_manual(values = .mhp_palette, name = NULL) +
    .mhp_y_cont(bars = TRUE) +
    ggplot2::labs(x = "design", y = "average RMSE of the cycle", title = "Power comparison on simulated series") +
    mhp_theme()
}

utils::globalVariables(c("lambda", "gcv", "value", "line", "panel", "series", "label", "on", "j", "weight", "row",
                         "theta", "gain", "loss", "part", "fmhp_better_hp", "fmhp_better_mhp", "method", "y"))
