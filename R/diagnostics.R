#-------------------------------------------------------------------------------------------------------------------
# Diagnostics of HP-type filters: smoothing schedules, filter weights, gains and the end-point loss of
# Bloechl (2014), as used by Hanif, Iqbal and Choudhary (2017).
#-------------------------------------------------------------------------------------------------------------------

# T and the smoothing parameters of a call: a length or a result, a scalar or a vector of lambdas
.mhp_diag_input <- function(x, lambda, form, fn) {
  if (inherits(x, "mhp_result")) {
    T <- length(x$original)
    lam <- if (!is.null(x$lambda_t)) x$lambda_t else x$lambda
    form <- if (!is.null(x$form)) x$form else "rows"
  } else {
    T <- .mhp_check_number(x, "x", fn, min = 5, integer = TRUE)
    form <- .mhp_check_choice(form[1L], c("rows", "differences"), "form", fn)
    lam <- .mhp_check_number(lambda, "lambda", fn, min = .Machine$double.eps, scalar = FALSE)
  }
  n <- if (form == "differences") T - 2L else T
  if (length(lam) == 1L) lam <- rep(lam, n)
  if (length(lam) != n) .mhp_stop(fn, "`lambda` must have length 1 or ", n, " (", form, " form, T = ", T, ").")
  list(T = as.integer(T), lambda = as.numeric(lam), form = form)
}

#' Smoothing parameters with raised end values
#'
#' @description
#' \code{hp_schedule()} gives the smoothing parameter of every observation (or second difference) under the
#' end-point schemes of Hanif, Iqbal and Choudhary (2017, eqs. 9a and 9b): \eqn{\lambda}{lambda} in the middle and
#' \eqn{\lambda + \alpha j^{i}}{lambda + alpha j^i} for the \eqn{j}{j}-th of the \eqn{k}{k} values at each end,
#' counted from the middle (\eqn{j = k}{j = k} at the first and last observation).
#'
#' @param T length of the series.
#' @param lambda the base smoothing parameter.
#' @param alpha the weight \eqn{\alpha \ge 0}{alpha >= 0}.
#' @param k number of raised values at each end.
#' @param power \eqn{i}{i}: 1 (linear, default) or 2 (quadratic); any positive number is accepted.
#' @param form \code{"rows"} (default: \eqn{T}{T} values, one per observation, as in the published code) or
#'   \code{"differences"} (\eqn{T - 2}{T - 2} values, one per second difference).
#' @return A numeric vector of smoothing parameters.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{fmhp_filter}}, \code{\link{hp_loss}}
#' @examples
#' hp_schedule(
#'     T      = 50
#'   , lambda = 100
#'   , alpha  = 10
#'   , k      = 20
#'   , power  = 1
#'   , form   = "rows"
#'   )
#' @export
hp_schedule <- function(T, lambda, alpha, k, power = 1, form = c("rows", "differences")) {
  fn <- "hp_schedule"
  T <- .mhp_check_number(T, "T", fn, min = 5, integer = TRUE)
  lambda <- .mhp_check_number(lambda, "lambda", fn, min = .Machine$double.eps)
  alpha  <- .mhp_check_number(alpha, "alpha", fn, min = 0)
  k      <- .mhp_check_number(k, "k", fn, min = 0, integer = TRUE)
  power  <- .mhp_check_number(power, "power", fn, min = .Machine$double.eps)
  form   <- .mhp_check_choice(form[1L], c("rows", "differences"), "form", fn)
  if (2L * k > (if (form == "differences") T - 2L else T)) .mhp_stop(fn, "`k` is too large for T = ", T, ".")
  .mhp_schedule(lambda, alpha, k, power, T, form)
}

#' Weights of an HP-type filter
#'
#' @description
#' \code{hp_weights()} gives the matrix \eqn{\mathbf{B}}{B} of the trend \eqn{\hat{\mathbf{g}} =
#' \mathbf{B}\mathbf{x}}{g = B x}: row \eqn{t}{t} holds the weights of the estimate at time \eqn{t}{t}. Symmetric in
#' the middle of the series, the rows become asymmetric near the ends, the source of the end-point bias (Hanif et
#' al. 2017, Figures 1 and 1a).
#'
#' @param x the length \eqn{T}{T} of a series, or a result of \code{\link{hp_filter}()},
#'   \code{\link{mhp_filter}()} or \code{\link{fmhp_filter}()} (with \code{as_dt = FALSE}).
#' @param lambda a smoothing parameter, or one per observation (\code{form = "rows"}) or per second difference
#'   (\code{form = "differences"}); ignored for a result.
#' @param form \code{"rows"} (default) or \code{"differences"}; see \code{\link{fmhp_filter}()}.
#' @param rows the rows drawn by \code{plot()} (default: the first, third, two middle and last two odd ones, as in
#'   the paper's figures).
#' @return An object of class \code{c("mhp_weights", "mhp_result", "list")}: \code{weights} (the
#'   \eqn{T \times T}{T x T} matrix), \code{lambda_t}, \code{form}, \code{rows}. \code{plot()} draws the chosen rows.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{hp_gain}}, \code{\link{hp_loss}}
#' @examples
#' w <-
#'   hp_weights(
#'       x      = 50
#'     , lambda = 100
#'     , form   = "rows"
#'     , rows   = NULL
#'     )
#' round(x = w$weights[1:3, 1:6], digits = 3)
#' plot(x = w)
#' @export
hp_weights <- function(x, lambda = 1600, form = c("rows", "differences"), rows = NULL) UseMethod("hp_weights")

#' @rdname hp_weights
#' @export
hp_weights.default <- function(x, lambda = 1600, form = c("rows", "differences"), rows = NULL) {
  fn <- "hp_weights"
  d  <- .mhp_diag_input(x, lambda, form, fn)
  rows <- .mhp_rows(rows, d$T, fn)
  .mhp_result(list(weights = .mhp_weights_cpp(d$T, d$lambda, .mhp_form_code(d$form)), lambda_t = d$lambda,
                   form = d$form, rows = rows), cls = "weights", call = match.call(),
              label = sprintf("Filter weights, T = %d", d$T))
}

#' Gain functions of an HP-type filter
#'
#' @description
#' \code{hp_gain()} gives the gain of the estimate at every time \eqn{t}{t} as a function of the frequency
#' (Hanif et al. 2017, eq. 6, Figures 2 and 2b): the factor by which the filter scales an oscillation of that
#' frequency. In the middle of a series the gain of the trend falls to zero at high frequencies; at the ends it does
#' not, so the trend there keeps part of the cycle.
#'
#' @details
#' With \eqn{b_{tj}}{b_tj} the weights of \code{\link{hp_weights}()}, \eqn{P_t(\omega) = |\sum_j b_{tj}
#' e^{i\omega(j - t)}|}{P_t(omega) = |sum_j b_tj exp(i omega (j - t))|}. It is computed from two solves of the
#' smoothing system per frequency.
#'
#' @inheritParams hp_weights
#' @param theta the frequencies (default 101 values from 0 to \eqn{\pi}{pi}).
#' @return An object of class \code{c("mhp_gain", "mhp_result", "list")}: \code{gain} (a \code{data.table} with
#'   \code{t}, \code{theta}, \code{gain} for every time and frequency), \code{lambda_t}, \code{form}, \code{rows}.
#'   \code{plot()} draws the gains of the chosen rows.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{hp_loss}}, \code{\link{hp_weights}}
#' @examples
#' g <-
#'   hp_gain(
#'       x      = 50
#'     , lambda = 100
#'     , form   = "rows"
#'     , theta  = seq(from = 0, to = pi, length.out = 101)
#'     , rows   = NULL
#'     )
#' plot(x = g)
#' @export
hp_gain <- function(x, lambda = 1600, form = c("rows", "differences"), theta = seq(0, pi, length.out = 101),
                    rows = NULL) {
  UseMethod("hp_gain")
}

#' @rdname hp_gain
#' @export
hp_gain.default <- function(x, lambda = 1600, form = c("rows", "differences"), theta = seq(0, pi, length.out = 101),
                            rows = NULL) {
  fn <- "hp_gain"
  d  <- .mhp_diag_input(x, lambda, form, fn)
  theta <- .mhp_check_number(theta, "theta", fn, min = 0, max = pi, scalar = FALSE)
  rows  <- .mhp_rows(rows, d$T, fn)
  G <- .mhp_gain_cpp(d$T, d$lambda, theta, .mhp_form_code(d$form))
  gd <- data.table::data.table(t = rep(seq_len(d$T), times = length(theta)), theta = rep(theta, each = d$T),
                               gain = as.vector(G))
  .mhp_result(list(gain = gd, lambda_t = d$lambda, form = d$form, rows = rows), cls = "gain", call = match.call(),
              label = sprintf("Gain functions, T = %d", d$T))
}

#' End-point loss of an HP-type filter
#'
#' @description
#' \code{hp_loss()} measures the end-point bias of a filter by the loss of Bloechl (2014) used by Hanif, Iqbal and
#' Choudhary (2017): for every time \eqn{t}{t}, the squared distance of the gain of its estimate from the gain at
#' the centre of the series. The loss is zero in the middle and grows towards the ends; the fully modified HP filter
#' chooses its end-point weights to make the total small.
#'
#' @details
#' \deqn{l(t) = \theta\sum_{\omega}\left[P_c(\omega) - P_t(\omega)\right]^2, \qquad L = \sum_{t=1}^{T} l(t),}{l(t) =
#'   theta sum_omega (P_c(omega) - P_t(omega))^2, L = sum_t l(t),}
#' with the gains \eqn{P_t}{P_t} of \code{\link{hp_gain}()}, \eqn{\omega = 0, 0.1, \dots, 3.1}{omega = 0, 0.1, ...,
#' 3.1}, \eqn{\theta = 0.1}{theta = 0.1} (their spacing) and \eqn{c = \mathrm{round}(T/2)}{c = round(T/2)}
#' (Hanif et al. 2017, eqs. 7 and 8, and their MATLAB code). The loss depends on \eqn{T}{T} and the smoothing
#' parameters only, not on the data: for the HP filter with \eqn{\lambda = 100}{lambda = 100} it is 1.731 to 1.735
#' for 41 to 55 observations, the values of Table 3a of the paper.
#'
#' @inheritParams hp_weights
#' @param theta the frequencies (default \eqn{0, 0.1, \dots, 3.1}{0, 0.1, ..., 3.1}).
#' @param weight the factor \eqn{\theta}{theta} (default 0.1).
#' @return An object of class \code{c("mhp_loss", "mhp_result", "list")}: \code{loss} (a \code{data.table} with
#'   \code{t} and \code{loss}), \code{total} (\eqn{L}{L}), \code{lambda_t}, \code{form}. \code{plot()} draws
#'   \eqn{l(t)}{l(t)} against \eqn{t}{t} (Figures 3 and 3b of the paper).
#' @references
#' Bloechl, A. (2014). Reducing the excess variability of the Hodrick-Prescott filter by flexible penalization.
#' \emph{Discussion Paper} 2014-1, University of Munich.
#'
#' Hanif, M. N., Iqbal, J. and Choudhary, M. A. (2017). Fully modified HP filter. \emph{SBP Working Paper Series},
#' No. 88, State Bank of Pakistan.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{fmhp_filter}}, \code{\link{hp_gain}}, \code{\link{fmhp_compare}}
#' @examples
#' hl <-
#'   hp_loss(
#'       x      = 55
#'     , lambda = 100
#'     , form   = "rows"
#'     , theta  = seq(from = 0, to = 3.1, by = 0.1)
#'     , weight = 0.1
#'     )
#' hl$total
#' plot(x = hl)
#' @export
hp_loss <- function(x, lambda = 1600, form = c("rows", "differences"), theta = seq(0, 3.1, 0.1), weight = 0.1) {
  UseMethod("hp_loss")
}

#' @rdname hp_loss
#' @export
hp_loss.default <- function(x, lambda = 1600, form = c("rows", "differences"), theta = seq(0, 3.1, 0.1),
                            weight = 0.1) {
  fn <- "hp_loss"
  d  <- .mhp_diag_input(x, lambda, form, fn)
  theta  <- .mhp_check_number(theta, "theta", fn, min = 0, max = pi, scalar = FALSE)
  weight <- .mhp_check_number(weight, "weight", fn, min = 0)
  l <- drop(.mhp_loss_cpp(d$T, d$lambda, theta, .mhp_form_code(d$form), .mhp_centre(d$T), weight))
  .mhp_result(list(loss = data.table::data.table(t = seq_len(d$T), loss = l), total = sum(l), lambda_t = d$lambda,
                   form = d$form), cls = "loss", call = match.call(),
              label = sprintf("End-point loss, T = %d: L = %.4f", d$T, sum(l)))
}

# the rows shown by plots: the paper's choice (1, 3, the two middle ones, T - 3, T - 1) by default
.mhp_rows <- function(rows, T, fn) {
  if (is.null(rows)) return(unique(as.integer(c(1, 3, floor(T / 2), floor(T / 2) + 1, T - 3, T - 1))))
  .mhp_check_number(rows, "rows", fn, min = 1, max = T, integer = TRUE, scalar = FALSE)
}
