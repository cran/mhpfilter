#-------------------------------------------------------------------------------------------------------------------
# Many series at once, and the comparison of the filters.
#-------------------------------------------------------------------------------------------------------------------

# the columns of a matrix or data frame as complete series (leading and trailing NA trimmed)
.mhp_columns <- function(X, fn) {
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) .mhp_stop(fn, "`X` must be a matrix or a data frame, one series per column.")
  if (!is.numeric(X)) .mhp_stop(fn, "`X` must be numeric.")
  nms <- colnames(X)
  if (is.null(nms)) nms <- paste0("V", seq_len(ncol(X)))
  cols <- lapply(seq_len(ncol(X)), function(j) {
    v  <- X[, j]
    ok <- which(!is.na(v))
    if (!length(ok)) .mhp_stop(fn, "series \"", nms[j], "\" has no values.")
    if (any(is.na(v[min(ok):max(ok)]))) {
      .mhp_stop(fn, "series \"", nms[j], "\" has missing values inside it; only leading and trailing ones are allowed.")
    }
    list(t = min(ok):max(ok), x = v[min(ok):max(ok)])
  })
  names(cols) <- nms
  cols
}

# the frequency of the data: the conventional HP lambda
.mhp_freq_lambda <- function(frequency) c(quarterly = 1600, annual = 100, monthly = 129600)[[frequency]]

# first-order autocorrelation of a cycle (no mean correction, as in version 0.1.0)
.mhp_ar1 <- function(x) {
  n <- length(x)
  if (n < 3L) return(NA_real_)
  sum(x[-n] * x[-1L]) / sum(x[-n]^2)
}

#' Modified HP filter of many series
#'
#' @description
#' \code{mhp_batch()} applies \code{\link{mhp_filter}()} to every column of a matrix or data frame and returns one
#' long table, with the smoothing parameter of each series in an attribute. Series of different lengths can share
#' the matrix: leading and trailing missing values are dropped series by series.
#'
#' @param X a numeric matrix or data frame, one series per column; missing values only at the start or the end.
#' @inheritParams mhp_filter
#' @return A \code{data.table} with the columns \code{series}, \code{t}, \code{original}, \code{trend},
#'   \code{cycle} and the attribute \code{lambdas}, a \code{data.table} with \code{series}, \code{lambda} and
#'   \code{gcv} (as in version 0.1.0).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_filter}}, \code{\link{fmhp_batch}}, \code{\link{batch_compare}}, \code{\link{plot_batch}}
#' @examples
#' data(us_macro)
#' X <- log(as.matrix(us_macro[, c("gdp", "consumption", "investment")]))
#' mb <-
#'   mhp_batch(
#'       X          = X
#'     , max_lambda = 100000
#'     )
#' attr(x = mb, which = "lambdas")
#' @export
mhp_batch <- function(X, max_lambda = 100000L) UseMethod("mhp_batch")

#' @rdname mhp_batch
#' @export
mhp_batch.default <- function(X, max_lambda = 100000L) {
  fn <- "mhp_batch"
  cols <- .mhp_columns(X, fn)
  max_lambda <- .mhp_check_number(max_lambda, "max_lambda", fn, min = 1, integer = TRUE)
  res <- lapply(names(cols), function(nm) {
    s <- cols[[nm]]
    if (length(s$x) < 5L) .mhp_stop(fn, "series \"", nm, "\" is too short (at least 5 observations).")
    lam <- .mhp_lambda(s$x, max_lambda)
    tr  <- .mhp_trend(s$x, lam$lambda)
    list(d = data.table::data.table(series = nm, t = s$t, original = s$x, trend = tr, cycle = s$x - tr),
         l = data.table::data.table(series = nm, lambda = as.integer(lam$lambda), gcv = lam$gcv))
  })
  out <- data.table::rbindlist(lapply(res, `[[`, "d"))
  data.table::setattr(out, "lambdas", data.table::rbindlist(lapply(res, `[[`, "l")))
  out
}

#' Fully modified HP filter of many series
#'
#' @description
#' \code{fmhp_batch()} applies \code{\link{fmhp_filter}()} to every column of a matrix or data frame, as
#' \code{\link{mhp_batch}()} does for the modified HP filter.
#'
#' @param X a numeric matrix or data frame, one series per column; missing values only at the start or the end.
#' @inheritParams fmhp_filter
#' @return A \code{data.table} with the columns \code{series}, \code{t}, \code{original}, \code{trend},
#'   \code{cycle} and the attribute \code{lambdas}, a \code{data.table} with \code{series}, \code{lambda},
#'   \code{gcv}, \code{lambda_mhp}, \code{scheme}, \code{alpha}, \code{k} and \code{loss}.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{fmhp_filter}}, \code{\link{mhp_batch}}, \code{\link{plot_batch}}
#' @examples
#' data(us_macro)
#' X <- as.matrix(us_macro[, c("gdp", "consumption", "investment")])
#' fb <-
#'   fmhp_batch(
#'       X               = X
#'     , max_lambda      = 3000
#'     , k_linear        = 15
#'     , k_quadratic     = NULL
#'     , alpha_linear    = seq(from = 0, to = 2000, by = 10)
#'     , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
#'     , schemes         = c("linear", "quadratic")
#'     , form            = "rows"
#'     , lambda          = NULL
#'     , log_scale       = TRUE
#'     )
#' attr(x = fb, which = "lambdas")
#' @export
fmhp_batch <- function(X, max_lambda = 3000L, k_linear = 15L, k_quadratic = NULL,
                       alpha_linear = seq(0, 2000, 10), alpha_quadratic = seq(0, 2000, 5),
                       schemes = c("linear", "quadratic"), form = c("rows", "differences"), lambda = NULL,
                       log_scale = TRUE) {
  UseMethod("fmhp_batch")
}

#' @rdname fmhp_batch
#' @export
fmhp_batch.default <- function(X, max_lambda = 3000L, k_linear = 15L, k_quadratic = NULL,
                               alpha_linear = seq(0, 2000, 10), alpha_quadratic = seq(0, 2000, 5),
                               schemes = c("linear", "quadratic"), form = c("rows", "differences"), lambda = NULL,
                               log_scale = TRUE) {
  fn <- "fmhp_batch"
  cols <- .mhp_columns(X, fn)
  res <- lapply(names(cols), function(nm) {
    s <- cols[[nm]]
    f <- fmhp_filter.default(s$x, max_lambda = max_lambda, k_linear = k_linear, k_quadratic = k_quadratic,
                             alpha_linear = alpha_linear, alpha_quadratic = alpha_quadratic, schemes = schemes,
                             form = form, lambda = lambda, log_scale = log_scale, as_dt = FALSE)
    list(d = data.table::data.table(series = nm, t = s$t, original = s$x, trend = f$trend, cycle = f$cycle),
         l = data.table::data.table(series = nm, lambda = f$lambda, gcv = f$gcv, lambda_mhp = f$lambda_mhp,
                                    scheme = f$scheme, alpha = f$alpha, k = f$k, loss = f$loss))
  })
  out <- data.table::rbindlist(lapply(res, `[[`, "d"))
  data.table::setattr(out, "lambdas", data.table::rbindlist(lapply(res, `[[`, "l")))
  out
}

#' Compare the HP and the modified HP filter
#'
#' @description
#' \code{mhp_compare()} filters one series with the HP filter at the conventional \eqn{\lambda}{lambda} of its
#' frequency and with the modified HP filter, and compares the cycles; \code{batch_compare()} does the same for
#' every column of a matrix.
#'
#' @details
#' The conventional values are \eqn{\lambda = 1600}{lambda = 1600} (quarterly), 100 (annual) and 129600 (monthly,
#' Ravn and Uhlig 2002). \code{ar1} is the first-order autocorrelation of the cycle, \eqn{\sum_t c_t c_{t+1} /
#' \sum_t c_t^2}{sum_t c_t c_(t+1) / sum_t c_t^2}.
#'
#' @inheritParams mhp_filter
#' @param X a numeric matrix or data frame, one series per column.
#' @param frequency \code{"quarterly"} (default), \code{"annual"} or \code{"monthly"}.
#' @return \code{mhp_compare()}: a \code{data.table} with one row per method (\code{HP}, \code{Modified HP}) and
#'   the columns \code{method}, \code{lambda}, \code{cycle_sd}, \code{cycle_mean}, \code{ar1}, \code{cycle_range},
#'   \code{gcv}. \code{batch_compare()}: a \code{data.table} with one row per series and the columns
#'   \code{series}, \code{hp_lambda}, \code{mhp_lambda}, \code{hp_cycle_sd}, \code{mhp_cycle_sd}, \code{sd_diff},
#'   \code{hp_ar1}, \code{mhp_ar1}, \code{ar1_diff}, \code{relative_sd}.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{fmhp_compare}}, \code{\link{plot_comparison}}
#' @examples
#' data(us_macro)
#' cmp <-
#'   mhp_compare(
#'       x          = log(us_macro$gdp)
#'     , frequency  = "quarterly"
#'     , max_lambda = 100000
#'     )
#' cmp
#' bc <-
#'   batch_compare(
#'       X          = log(as.matrix(us_macro[, c("gdp", "consumption", "investment")]))
#'     , frequency  = "quarterly"
#'     , max_lambda = 100000
#'     )
#' bc
#' @export
mhp_compare <- function(x, frequency = c("quarterly", "annual", "monthly"), max_lambda = 100000L) {
  UseMethod("mhp_compare")
}

#' @rdname mhp_compare
#' @export
mhp_compare.default <- function(x, frequency = c("quarterly", "annual", "monthly"), max_lambda = 100000L) {
  fn <- "mhp_compare"
  frequency <- .mhp_check_choice(frequency[1L], c("quarterly", "annual", "monthly"), "frequency", fn)
  fixed <- .mhp_freq_lambda(frequency)
  hp  <- hp_filter.default(x, lambda = fixed, as_dt = FALSE)
  mhp <- mhp_filter.default(x, max_lambda = max_lambda, as_dt = FALSE)
  data.table::data.table(
      method      = c("HP", "Modified HP")
    , lambda      = c(fixed, mhp$lambda)
    , cycle_sd    = c(stats::sd(hp$cycle), stats::sd(mhp$cycle))
    , cycle_mean  = c(mean(hp$cycle), mean(mhp$cycle))
    , ar1         = c(.mhp_ar1(hp$cycle), .mhp_ar1(mhp$cycle))
    , cycle_range = c(diff(range(hp$cycle)), diff(range(mhp$cycle)))
    , gcv         = c(NA_real_, mhp$gcv)
    )
}

#' @rdname mhp_compare
#' @export
batch_compare <- function(X, frequency = c("quarterly", "annual", "monthly"), max_lambda = 100000L) {
  UseMethod("batch_compare")
}

#' @rdname mhp_compare
#' @export
batch_compare.default <- function(X, frequency = c("quarterly", "annual", "monthly"), max_lambda = 100000L) {
  fn <- "batch_compare"
  frequency <- .mhp_check_choice(frequency[1L], c("quarterly", "annual", "monthly"), "frequency", fn)
  fixed <- .mhp_freq_lambda(frequency)
  cols  <- .mhp_columns(X, fn)
  max_lambda <- .mhp_check_number(max_lambda, "max_lambda", fn, min = 1, integer = TRUE)
  data.table::rbindlist(lapply(names(cols), function(nm) {
    x   <- cols[[nm]]$x
    hpc <- x - .mhp_trend(x, fixed)
    lam <- .mhp_lambda(x, max_lambda)
    mc  <- x - .mhp_trend(x, lam$lambda)
    hs  <- stats::sd(hpc)
    ms  <- stats::sd(mc)
    data.table::data.table(
        series       = nm
      , hp_lambda    = fixed
      , mhp_lambda   = as.integer(lam$lambda)
      , hp_cycle_sd  = hs
      , mhp_cycle_sd = ms
      , sd_diff      = ms - hs
      , hp_ar1       = .mhp_ar1(hpc)
      , mhp_ar1      = .mhp_ar1(mc)
      , ar1_diff     = .mhp_ar1(mc) - .mhp_ar1(hpc)
      , relative_sd  = if (hs > 0) ms / hs else NA_real_
      )
  }))
}

#' Compare the HP, modified HP and fully modified HP filters
#'
#' @description
#' \code{fmhp_compare()} filters one series with the three filters and compares their cycles and their end-point
#' loss (Hanif et al. 2017, Tables 3a and 3b): the HP filter at the conventional \eqn{\lambda}{lambda} of the
#' frequency, the modified HP filter and the fully modified HP filter.
#'
#' @details
#' Besides the moments of \code{\link{mhp_compare}()}, the table gives the total loss \eqn{L}{L} of each filter
#' (\code{\link{hp_loss}()}), its standard deviation of the cycle at the ends (the first and last
#' \code{end_share} of the observations) and in the middle. A filter without end-point bias has a loss near zero and
#' about the same cycle variability at the ends as in the middle.
#'
#' @inheritParams fmhp_filter
#' @param frequency \code{"quarterly"} (default), \code{"annual"} or \code{"monthly"}.
#' @param end_share share of the observations counted as ends on each side (default 0.1, as in Table 2 of the
#'   paper).
#' @return A \code{data.table} with one row per filter (\code{HP}, \code{Modified HP}, \code{Fully modified HP}) and
#'   the columns \code{method}, \code{lambda}, \code{cycle_sd}, \code{cycle_mean}, \code{ar1}, \code{cycle_range},
#'   \code{gcv}, \code{loss}, \code{sd_ends}, \code{sd_middle}.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{fmhp_filter}}, \code{\link{mhp_compare}}, \code{\link{hp_loss}}
#' @examples
#' data(us_macro)
#' fc <-
#'   fmhp_compare(
#'       x          = us_macro$gdp
#'     , frequency  = "quarterly"
#'     , max_lambda = 3000
#'     , log_scale  = TRUE
#'     , end_share  = 0.1
#'     )
#' fc
#' @export
fmhp_compare <- function(x, frequency = c("quarterly", "annual", "monthly"), max_lambda = 3000L, log_scale = TRUE,
                         end_share = 0.1) {
  UseMethod("fmhp_compare")
}

#' @rdname fmhp_compare
#' @export
fmhp_compare.default <- function(x, frequency = c("quarterly", "annual", "monthly"), max_lambda = 3000L,
                                 log_scale = TRUE, end_share = 0.1) {
  fn <- "fmhp_compare"
  frequency <- .mhp_check_choice(frequency[1L], c("quarterly", "annual", "monthly"), "frequency", fn)
  end_share <- .mhp_check_number(end_share, "end_share", fn, min = 0.01, max = 0.45)
  fixed <- .mhp_freq_lambda(frequency)
  hp  <- hp_filter.default(x, lambda = fixed, as_dt = FALSE, log_scale = log_scale)
  mhp <- mhp_filter.default(x, max_lambda = max_lambda, as_dt = FALSE, log_scale = log_scale)
  fm  <- fmhp_filter.default(x, max_lambda = max_lambda, log_scale = log_scale, as_dt = FALSE)
  T  <- length(hp$original)
  ne <- max(1L, as.integer(.mhp_round(end_share * T)))
  ends <- c(seq_len(ne), seq.int(T - ne + 1L, T))
  cyc  <- function(f) if (log_scale) f$cycle_log else f$cycle
  row <- function(nm, f, lam_t, gcv) {
    cc <- cyc(f)
    data.table::data.table(
        method      = nm
      , lambda      = f$lambda
      , cycle_sd    = stats::sd(cc)
      , cycle_mean  = mean(cc)
      , ar1         = .mhp_ar1(cc)
      , cycle_range = diff(range(cc))
      , gcv         = gcv
      , loss        = sum(hp_loss.default(T, lambda = lam_t, form = fm$form)$loss$loss)
      , sd_ends     = stats::sd(cc[ends])
      , sd_middle   = stats::sd(cc[-ends])
      )
  }
  data.table::rbindlist(list(
      row("HP", hp, fixed, NA_real_)
    , row("Modified HP", mhp, mhp$lambda, mhp$gcv)
    , row("Fully modified HP", fm, fm$lambda_t, fm$gcv)
    ))
}
