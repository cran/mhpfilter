#-------------------------------------------------------------------------------------------------------------------
# The three filters: HP with a fixed lambda, modified HP (lambda by McDermott's criterion) and fully modified HP
# (Hanif, Iqbal and Choudhary 2017: endogenous lambda and endogenous end-point weights).
#-------------------------------------------------------------------------------------------------------------------

# a series as a numeric vector, with its time when it is a ts
.mhp_series <- function(x, fn, min_n = 5L) {
  if (!is.numeric(x)) .mhp_stop(fn, "`x` must be a numeric vector (or a ts).")
  tm <- if (stats::is.ts(x)) as.numeric(stats::time(x)) else NULL
  fq <- if (stats::is.ts(x)) stats::frequency(x) else NULL
  x <- as.numeric(x)
  if (anyNA(x)) .mhp_stop(fn, "`x` has missing values; filter the complete stretch of the series.")
  if (length(x) < min_n) .mhp_stop(fn, "the series is too short: ", length(x), " observations, at least ", min_n,
                                   " are needed.")
  list(x = x, time = tm, frequency = fq)
}

# the series on the scale of the filter (logarithms when log_scale is TRUE)
.mhp_scale <- function(x, log_scale, fn) {
  if (!log_scale) return(x)
  if (any(x <= 0)) .mhp_stop(fn, "`log_scale = TRUE` needs a positive series.")
  log(x)
}

# trend and cycle on the scale of the original series; with logarithms the trend is exp(trend of log x) and the
# cycle x - trend, as in the published code of Hanif et al. (2017); the log-scale components are kept too
.mhp_parts <- function(x, z_trend, log_scale) {
  if (!log_scale) return(list(trend = z_trend, cycle = x - z_trend))
  tr <- exp(z_trend)
  list(trend = tr, cycle = x - tr, trend_log = z_trend, cycle_log = log(x) - z_trend)
}

# the result: a data.table with the attributes of version 0.1.0 (as_dt = TRUE), or the classed list
.mhp_output <- function(s, parts, lst, attrs, as_dt, cls, call, label) {
  if (as_dt) {
    out <- data.table::data.table(original = s$x, trend = parts$trend, cycle = parts$cycle)
    if (!is.null(s$time)) out[, time := s$time]
    for (nm in names(attrs)) data.table::setattr(out, nm, attrs[[nm]])
    return(out)
  }
  res <- .mhp_result(c(list(original = s$x, trend = parts$trend, cycle = parts$cycle), lst,
                       parts[setdiff(names(parts), c("trend", "cycle"))],
                       list(time = s$time, frequency = s$frequency)),
                     cls = cls, label = label, call = call)
  class(res) <- append(class(res), "mhp", after = 1L)
  res
}

#' Hodrick-Prescott filter with a fixed smoothing parameter
#'
#' @description
#' \code{hp_filter()} splits a time series into a smooth trend and a cycle with the filter of Hodrick and Prescott
#' (1997) and a smoothing parameter \eqn{\lambda}{lambda} chosen by the user: 1600 for quarterly data, 100 for annual
#' data and 129600 for monthly data are the conventional values (Ravn and Uhlig 2002).
#'
#' @details
#' The trend \eqn{\mathbf{g} = (g_1, \dots, g_T)^\top}{g} of the series \eqn{\mathbf{x}}{x} minimises
#' \deqn{\sum_{t=1}^{T}(x_t - g_t)^2 + \lambda\sum_{t=2}^{T-1}\left[(g_{t+1} - g_t) - (g_t - g_{t-1})\right]^2,}{sum_t
#'   (x_t - g_t)^2 + lambda sum_t ((g_(t+1) - g_t) - (g_t - g_(t-1)))^2,}
#' so that \eqn{\hat{\mathbf{g}} = (\mathbf{I} + \lambda\mathbf{A})^{-1}\mathbf{x} = \mathbf{B}\mathbf{x}}{g = (I +
#' lambda A)^(-1) x = B x} with \eqn{\mathbf{A} = \mathbf{K}^\top\mathbf{K}}{A = K'K} and \eqn{\mathbf{K}}{K} the
#' \eqn{(T - 2) \times T}{(T - 2) x T} matrix of second differences (rows \eqn{(1, -2, 1)}{(1, -2, 1)}). The cycle is
#' \eqn{\mathbf{c} = \mathbf{x} - \hat{\mathbf{g}}}{c = x - g}. The system is pentadiagonal, symmetric and positive
#' definite and is solved in \eqn{O(T)}{O(T)} operations.
#'
#' With \code{log_scale = TRUE} the filter is applied to \eqn{\log x_t}{log x_t}; the trend is returned as
#' \eqn{\exp(\hat{g}_t)}{exp(g_t)} and the cycle as \eqn{x_t - \exp(\hat{g}_t)}{x_t - exp(g_t)}, and the components
#' on the log scale are added to the list result.
#'
#' @param x a numeric vector or a \code{ts} object, without missing values; at least 5 observations.
#' @param lambda the smoothing parameter \eqn{\lambda > 0}{lambda > 0} (default 1600, for quarterly data).
#' @param as_dt \code{TRUE} (default) returns a \code{data.table}; \code{FALSE} a list.
#' @param log_scale filter the logarithm of the series (default \code{FALSE}, as in version 0.1.0).
#' @return With \code{as_dt = TRUE}, a \code{data.table} with the columns \code{original}, \code{trend},
#'   \code{cycle} (and \code{time} for a \code{ts}) and the attribute \code{lambda}. With \code{as_dt = FALSE}, a list
#'   of class \code{c("mhp_hp", "mhp", "mhp_result", "list")} with \code{original}, \code{trend}, \code{cycle},
#'   \code{lambda}, then \code{log_scale}, \code{trend_log} and \code{cycle_log} (with \code{log_scale = TRUE}) and
#'   \code{time} and \code{frequency} (for a \code{ts}). Methods: \code{print()}, \code{plot()},
#'   \code{ggplot2::autoplot()}, \code{\link{mhp_interpret}()}.
#' @references
#' Hodrick, R. J. and Prescott, E. C. (1997). Postwar U.S. business cycles: an empirical investigation.
#' \emph{Journal of Money, Credit and Banking}, \strong{29}(1), 1--16. \doi{10.2307/2953682}
#'
#' Ravn, M. O. and Uhlig, H. (2002). On adjusting the Hodrick-Prescott filter for the frequency of observations.
#' \emph{The Review of Economics and Statistics}, \strong{84}(2), 371--376. \doi{10.1162/003465302317411604}
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_filter}}, \code{\link{fmhp_filter}}, \code{\link{mhp_compare}}
#' @examples
#' data(us_macro)
#' gdp <- ts(data = us_macro$gdp, start = c(1959, 1), frequency = 4)
#' hp <-
#'   hp_filter(
#'       x         = log(gdp)
#'     , lambda    = 1600
#'     , as_dt     = TRUE
#'     , log_scale = FALSE
#'     )
#' hp
#' attr(x = hp, which = "lambda")
#' @export
hp_filter <- function(x, lambda = 1600, as_dt = TRUE, log_scale = FALSE) UseMethod("hp_filter")

#' @rdname hp_filter
#' @export
hp_filter.default <- function(x, lambda = 1600, as_dt = TRUE, log_scale = FALSE) {
  fn <- "hp_filter"
  s  <- .mhp_series(x, fn)
  lambda    <- .mhp_check_number(lambda, "lambda", fn, min = .Machine$double.eps)
  as_dt     <- .mhp_check_flag(as_dt, "as_dt", fn)
  log_scale <- .mhp_check_flag(log_scale, "log_scale", fn)
  z  <- .mhp_scale(s$x, log_scale, fn)
  parts <- .mhp_parts(s$x, .mhp_trend(z, lambda), log_scale)
  .mhp_output(s, parts, list(lambda = lambda, log_scale = log_scale), list(lambda = lambda), as_dt, "hp",
              match.call(), sprintf("HP filter, lambda = %s", format(lambda)))
}

#' Modified Hodrick-Prescott filter: the smoothing parameter chosen from the data
#'
#' @description
#' \code{mhp_filter()} applies the modified HP filter of McDermott (1997), studied by Choudhary, Hanif and Iqbal
#' (2014): the smoothing parameter \eqn{\lambda}{lambda} is the one of \eqn{1, 2, \dots,}{1, 2, ...}
#' \code{max_lambda} that minimises a generalised cross-validation criterion, so that every series gets its own
#' \eqn{\lambda}{lambda} instead of the conventional 1600 or 100.
#'
#' @details
#' For each candidate \eqn{\lambda}{lambda} the trend \eqn{\hat{\mathbf{g}}(\lambda)}{g(lambda)} of
#' \code{\link{hp_filter}()} is computed and
#' \deqn{\mathrm{GCV}(\lambda) = \frac{1}{T}\left(1 + \frac{2T}{\lambda}\right)\sum_{t=1}^{T}\left(x_t -
#'   \hat{g}_t(\lambda)\right)^2}{GCV(lambda) = (1/T) (1 + 2T/lambda) sum_t (x_t - g_t(lambda))^2}
#' (Choudhary et al. 2014, eq. 6; Hanif et al. 2017, eq. 4); \eqn{\hat\lambda}{lambda} minimises it. This is
#' Silverman's (1984) approximation of generalised cross-validation (Craven and Wahba 1979),
#' \deqn{\mathrm{GCV}(\lambda) = \frac{T^{-1}\sum_{t}(x_t - \hat{g}_t(\lambda))^2}{\left(1 -
#'   T^{-1}\mathrm{tr}\,\mathbf{B}(\lambda)\right)^2}}{GCV(lambda) = (1/T) sum_t (x_t - g_t(lambda))^2 / (1 - tr
#'   B(lambda) / T)^2}
#' (Choudhary et al. 2014, eq. 5), which \code{criterion = "exact"} uses: the eigenvalues
#' \eqn{\mu_i}{mu_i} of \eqn{\mathbf{A}}{A}, computed once, give \eqn{\mathrm{tr}\,\mathbf{B}(\lambda) =
#' \sum_i (1 + \lambda\mu_i)^{-1}}{tr B(lambda) = sum_i 1/(1 + lambda mu_i)} and the residual sum of squares for
#' every \eqn{\lambda}{lambda} in \eqn{O(T)}{O(T)} operations (the decomposition costs \eqn{O(T^3)}{O(T^3)}, a
#' fraction of a second for a few hundred observations). Each trend costs
#' \eqn{O(T)}{O(T)} operations, so the search over 100000 values takes about a second for 200 quarterly observations.
#'
#' The search is that of the authors' MATLAB code (\code{modifiedhpfilternew.m}, Choudhary et al. 2014), which
#' omits the factor \eqn{1/T}{1/T} (it does not change the minimum) and works on the series as given; filter the
#' logarithm of a series (as in the papers) with \code{log_scale = TRUE} or by passing \code{log(x)}.
#'
#' \strong{Changes from version 0.1.0.} The criterion is unchanged (the help page of 0.1.0 printed the factor as
#' \eqn{1 + 2/(T\lambda)}{1 + 2/(T lambda)}, but the code used \eqn{1 + 2T/\lambda}{1 + 2T/lambda}); every trend is now
#' a banded solve instead of a dense one, so results agree with 0.1.0 to rounding error at a fraction of the time.
#'
#' @inheritParams hp_filter
#' @param max_lambda the largest \eqn{\lambda}{lambda} of the search over \eqn{1, 2, \dots,}{1, 2, ...}
#'   \code{max_lambda} (default 100000).
#' @param criterion \code{"approximate"} (default: eq. 6, as in version 0.1.0 and the published code) or
#'   \code{"exact"} (eq. 5).
#' @return With \code{as_dt = TRUE}, a \code{data.table} with the columns \code{original}, \code{trend},
#'   \code{cycle} (and \code{time} for a \code{ts}) and the attributes \code{lambda} and \code{gcv}. With
#'   \code{as_dt = FALSE}, a list of class \code{c("mhp_mhp", "mhp", "mhp_result", "list")} with \code{original},
#'   \code{trend}, \code{cycle}, \code{lambda}, \code{gcv} as in version 0.1.0, then \code{gcv_path} (the criterion
#'   for every \eqn{\lambda}{lambda} of the search), \code{log_scale}, \code{criterion}, \code{trend_log} and
#'   \code{cycle_log} (with
#'   \code{log_scale = TRUE}), \code{time} and \code{frequency} (for a \code{ts}). Methods: \code{print()},
#'   \code{plot()},
#'   \code{ggplot2::autoplot()}, \code{\link{mhp_interpret}()}.
#' @references
#' Choudhary, M. A., Hanif, M. N. and Iqbal, J. (2014). On smoothing macroeconomic time series using the modified HP
#' filter. \emph{Applied Economics}, \strong{46}(19), 2205--2214. \doi{10.1080/00036846.2014.894631}
#'
#' Craven, P. and Wahba, G. (1979). Smoothing noisy data with spline functions. \emph{Numerische Mathematik},
#' \strong{31}, 377--403. \doi{10.1007/BF01404567}
#'
#' Silverman, B. W. (1984). A fast and efficient cross-validation method for smoothing parameter choice in spline
#' regression. \emph{Journal of the American Statistical Association}, \strong{79}(387), 584--589.
#' \doi{10.1080/01621459.1984.10478084}
#'
#' McDermott, C. J. (1997). An automatic method for choosing the smoothing parameter in the HP filter. Unpublished,
#' International Monetary Fund, Washington DC; described in Coe, D. T. and McDermott, C. J. (1997). Does the gap
#' model work in Asia? \emph{IMF Staff Papers}, \strong{44}(1), 59--80. \doi{10.2307/3867497}
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{hp_filter}}, \code{\link{fmhp_filter}}, \code{\link{mhp_batch}}, \code{\link{get_lambda}}
#' @examples
#' data(us_macro)
#' gdp <- ts(data = us_macro$gdp, start = c(1959, 1), frequency = 4)
#' mhp <-
#'   mhp_filter(
#'       x          = log(gdp)
#'     , max_lambda = 100000
#'     , as_dt      = FALSE
#'     , log_scale  = FALSE
#'     , criterion  = "approximate"
#'     )
#' mhp
#' plot(x = mhp)
#' @export
mhp_filter <- function(x, max_lambda = 100000L, as_dt = TRUE, log_scale = FALSE,
                       criterion = c("approximate", "exact")) {
  UseMethod("mhp_filter")
}

#' @rdname mhp_filter
#' @export
mhp_filter.default <- function(x, max_lambda = 100000L, as_dt = TRUE, log_scale = FALSE,
                               criterion = c("approximate", "exact")) {
  fn <- "mhp_filter"
  criterion  <- .mhp_check_choice(criterion[1L], c("approximate", "exact"), "criterion", fn)
  s  <- .mhp_series(x, fn)
  max_lambda <- .mhp_check_number(max_lambda, "max_lambda", fn, min = 1, integer = TRUE)
  as_dt      <- .mhp_check_flag(as_dt, "as_dt", fn)
  log_scale  <- .mhp_check_flag(log_scale, "log_scale", fn)
  z   <- .mhp_scale(s$x, log_scale, fn)
  lam <- if (criterion == "approximate") .mhp_lambda(z, max_lambda) else .mhp_lambda_exact(z, max_lambda)
  if (lam$lambda == 1L && max_lambda > 1L) {
    .mhp_warn(fn, "the criterion is smallest at lambda = 1, the lower end of the search: the trend follows the ",
              "data.", if (criterion == "exact") paste(" Exact GCV often does this for trending series; the",
                                                         "approximate criterion is the published one."))
  }
  parts <- .mhp_parts(s$x, .mhp_trend(z, lam$lambda), log_scale)
  .mhp_output(s, parts, list(lambda = lam$lambda, gcv = lam$gcv, gcv_path = lam$path, log_scale = log_scale,
                             criterion = criterion),
              list(lambda = lam$lambda, gcv = lam$gcv), as_dt, "mhp", match.call(),
              sprintf("Modified HP filter, lambda = %d", as.integer(lam$lambda)))
}

#' Fully modified Hodrick-Prescott filter
#'
#' @description
#' \code{fmhp_filter()} applies the fully modified HP (FMHP) filter of Hanif, Iqbal and Choudhary (2017). It removes
#' the two weaknesses of the HP filter at once: the smoothing parameter is chosen from the data (as in the modified
#' HP filter of McDermott 1997), and the end-point bias is reduced by raising the smoothing parameter at the first
#' and last \eqn{k}{k} observations, with weights chosen to minimise the loss of Bloechl (2014), the distance of the
#' gain of every estimate from the gain at the centre of the series.
#'
#' @details
#' \strong{Gain and loss.} Row \eqn{t}{t} of the smoother \eqn{\mathbf{B}}{B} holds the weights of
#' \eqn{\hat{g}_t}{g_t}; its gain at frequency \eqn{\omega}{omega} is
#' \deqn{P_t(\omega) = \left|\sum_{j=1}^{T} b_{tj}\,e^{i\omega(j - t)}\right|}{P_t(omega) = |sum_j b_tj exp(i omega
#'   (j - t))|}
#' (Hanif et al. 2017, eq. 6), and the loss of the estimate is \eqn{l(t) = \theta\sum_{\omega}[P_c(\omega) -
#' P_t(\omega)]^2}{l(t) = theta sum_omega (P_c(omega) - P_t(omega))^2} over \eqn{\omega = 0, 0.1, \dots,
#' 3.1}{omega = 0, 0.1, ..., 3.1} with \eqn{\theta = 0.1}{theta = 0.1} and \eqn{c}{c} the centre of the series
#' (eq. 7); the total loss is \eqn{L = \sum_t l(t)}{L = sum_t l(t)} (eq. 8). See \code{\link{hp_loss}()}.
#'
#' \strong{End-point weights.} The smoothing parameter of observation \eqn{t}{t} is \eqn{\lambda_t = \lambda}{lambda_t
#' = lambda} in the middle and \eqn{\lambda + \alpha j^{i}}{lambda + alpha j^i} for the \eqn{j}{j}-th observation from
#' the middle among the last (and the first) \eqn{k}{k}, \eqn{j = 1, \dots, k}{j = 1, ..., k}, linear
#' (\eqn{i = 1}{i = 1}) or quadratic (\eqn{i = 2}{i = 2}) (eqs. 9a and 9b). See \code{\link{hp_schedule}()}.
#'
#' \strong{The procedure} (Section 5 of the paper):
#' \enumerate{
#'   \item \eqn{\lambda^{\mathrm{MHP}}}{lambda_MHP} of the modified HP filter (\code{\link{mhp_filter}()}), searched
#'     over \eqn{1, \dots,}{1, ...,} \code{max_lambda};
#'   \item for each scheme (linear and quadratic), the weight \eqn{\alpha}{alpha} of the grid that minimises the total
#'     loss \eqn{L}{L} at \eqn{\lambda^{\mathrm{MHP}}}{lambda_MHP};
#'   \item with that weight, \eqn{\lambda^{\mathrm{FMHP}}}{lambda_FMHP} minimising McDermott's criterion again;
#'   \item the scheme whose loss in step 2 is smaller gives the trend and the cycle.
#' }
#' The defaults are those of the published MATLAB code (Hanif 2017): \code{max_lambda = 3000}; linear weights on
#' \eqn{k = 15}{k = 15} end values with \eqn{\alpha \in \{0, 10, \dots, 2000\}}{alpha in 0, 10, ..., 2000};
#' quadratic weights on \eqn{k = \mathrm{round}(\min(22, T/2 - 5))}{k = round(min(22, T/2 - 5))} end values with
#' \eqn{\alpha \in \{0, 5, \dots, 2000\}}{alpha in 0, 5, ..., 2000}; logarithms of the series. The working paper
#' states \eqn{k = 20}{k = 20} in the text; set \code{k_linear} and \code{k_quadratic} to use it.
#'
#' \strong{Form of the weights.} The published code multiplies row \eqn{t}{t} of \eqn{\mathbf{A}}{A} by
#' \eqn{\lambda_t}{lambda_t}, \eqn{\hat{\mathbf{g}} = (\mathbf{I} + \mathrm{diag}(\lambda_t)\mathbf{A})^{-1}
#' \mathbf{x}}{g = (I + diag(lambda_t) A)^(-1) x} (\code{form = "rows"}, the default). The penalised least-squares
#' problem with a separate \eqn{\lambda_r}{lambda_r} for each second difference gives instead \eqn{\hat{\mathbf{g}} =
#' (\mathbf{I} + \mathbf{K}^\top\mathrm{diag}(\lambda_r)\mathbf{K})^{-1}\mathbf{x}}{g = (I + K' diag(lambda_r)
#' K)^(-1) x}, a symmetric smoother (\code{form = "differences"}, the scheme of eq. 9 written on the
#' \eqn{T - 2}{T - 2} differences). Both are solved in \eqn{O(T)}{O(T)} operations; the gains need two solves per
#' frequency and the smoother itself is never formed.
#'
#' With \code{lambda} given, the smoothing parameter is fixed (steps 1 and 3 are skipped): the HP filter with the
#' endogenous weights of the paper (Table 3a, column "HP (our scheme)").
#'
#' @inheritParams hp_filter
#' @param max_lambda the largest \eqn{\lambda}{lambda} of the searches over \eqn{1, 2, \dots,}{1, 2, ...}
#'   \code{max_lambda} (default 3000, as in the published code).
#' @param k_linear number of end values with linear weights (default 15).
#' @param k_quadratic number of end values with quadratic weights (default \code{NULL}:
#'   \eqn{\mathrm{round}(\min(22, T/2 - 5))}{round(min(22, T/2 - 5))}).
#' @param alpha_linear,alpha_quadratic the grids of the weight \eqn{\alpha}{alpha} (defaults
#'   \code{seq(0, 2000, 10)} and \code{seq(0, 2000, 5)}).
#' @param schemes the schemes compared: \code{c("linear", "quadratic")} (default) or one of them.
#' @param form \code{"rows"} (default, the published code) or \code{"differences"}; see Details.
#' @param lambda \code{NULL} (default: chosen from the data) or a fixed smoothing parameter.
#' @param log_scale filter the logarithm of the series (default \code{TRUE}, as in the published code).
#' @return With \code{as_dt = TRUE}, a \code{data.table} with the columns \code{original}, \code{trend},
#'   \code{cycle} (and \code{time} for a \code{ts}) and the attributes \code{lambda}, \code{gcv}, \code{lambda_mhp},
#'   \code{scheme}, \code{alpha}, \code{k}, \code{loss} and \code{lambda_t}. With \code{as_dt = FALSE}, a list of
#'   class \code{c("mhp_fmhp", "mhp", "mhp_result", "list")}:
#' \describe{
#'   \item{\code{original}, \code{trend}, \code{cycle}}{the series, its trend and its cycle (on the scale of the
#'     series).}
#'   \item{\code{lambda}, \code{gcv}}{\eqn{\lambda^{\mathrm{FMHP}}}{lambda_FMHP} and its criterion.}
#'   \item{\code{lambda_mhp}}{\eqn{\lambda^{\mathrm{MHP}}}{lambda_MHP} of step 1.}
#'   \item{\code{scheme}, \code{alpha}, \code{k}}{the chosen scheme, its weight and its number of end values.}
#'   \item{\code{loss}}{the total loss \eqn{L}{L} of the chosen scheme at \eqn{\lambda^{\mathrm{MHP}}}{lambda_MHP}
#'     (the criterion of step 4).}
#'   \item{\code{lambda_t}}{the smoothing parameters of the trend.}
#'   \item{\code{schemes}}{\code{data.table}, one row per scheme: \code{scheme}, \code{k}, \code{alpha},
#'     \code{loss}, \code{lambda}, \code{gcv}.}
#'   \item{\code{form}, \code{log_scale}}{the settings; then \code{trend_log}, \code{cycle_log} (with
#'     \code{log_scale = TRUE}), \code{time} and \code{frequency} (for a \code{ts}).}
#' }
#' Methods: \code{print()}, \code{plot()}, \code{ggplot2::autoplot()}, \code{\link{hp_loss}()},
#' \code{\link{mhp_interpret}()}.
#' @references
#' Hanif, M. N., Iqbal, J. and Choudhary, M. A. (2017). Fully modified HP filter. \emph{SBP Working Paper Series},
#' No. 88, State Bank of Pakistan.
#'
#' Hanif, M. N. (2017). Fully modified HP filter function (MATLAB code). MATLAB Central File Exchange, 63198.
#'
#' Bloechl, A. (2014). Reducing the excess variability of the Hodrick-Prescott filter by flexible penalization.
#' \emph{Discussion Paper} 2014-1, University of Munich.
#'
#' McDermott, C. J. (1997). An automatic method for choosing the smoothing parameter in the HP filter. Unpublished,
#' International Monetary Fund, Washington DC.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com}), from the MATLAB code of Muhammad Nadim Hanif
#' @seealso \code{\link{mhp_filter}}, \code{\link{fmhp_batch}}, \code{\link{fmhp_compare}}, \code{\link{hp_loss}}
#' @examples
#' data(us_macro)
#' gdp <- ts(data = us_macro$gdp, start = c(1959, 1), frequency = 4)
#' fm <-
#'   fmhp_filter(
#'       x               = gdp
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
#' fm
#' fm$schemes
#' plot(x = fm)
#' @export
fmhp_filter <- function(x, max_lambda = 3000L, k_linear = 15L, k_quadratic = NULL,
                        alpha_linear = seq(0, 2000, 10), alpha_quadratic = seq(0, 2000, 5),
                        schemes = c("linear", "quadratic"), form = c("rows", "differences"), lambda = NULL,
                        log_scale = TRUE, as_dt = TRUE) {
  UseMethod("fmhp_filter")
}

#' @rdname fmhp_filter
#' @export
fmhp_filter.default <- function(x, max_lambda = 3000L, k_linear = 15L, k_quadratic = NULL,
                                alpha_linear = seq(0, 2000, 10), alpha_quadratic = seq(0, 2000, 5),
                                schemes = c("linear", "quadratic"), form = c("rows", "differences"), lambda = NULL,
                                log_scale = TRUE, as_dt = TRUE) {
  fn <- "fmhp_filter"
  s  <- .mhp_series(x, fn)
  set <- .mhp_fmhp_settings(fn, length(s$x), max_lambda, k_linear, k_quadratic, alpha_linear, alpha_quadratic,
                            schemes, form, lambda)
  as_dt     <- .mhp_check_flag(as_dt, "as_dt", fn)
  log_scale <- .mhp_check_flag(log_scale, "log_scale", fn)
  z   <- .mhp_scale(s$x, log_scale, fn)
  fit <- .mhp_fmhp(z, set$max_lambda, set$k_linear, set$k_quadratic, set$alpha_linear, set$alpha_quadratic,
                   set$schemes, set$form, set$lambda)
  f   <- fit$fit
  parts <- .mhp_parts(s$x, f$trend, log_scale)
  tab <- data.table::rbindlist(lapply(names(fit$schemes), function(nm) {
    v <- fit$schemes[[nm]]
    data.table::data.table(scheme = nm, k = v$k, alpha = v$alpha, loss = v$loss, lambda = v$lambda, gcv = v$gcv)
  }))
  lst <- list(lambda = f$lambda, gcv = f$gcv, lambda_mhp = fit$lambda_mhp, scheme = fit$scheme, alpha = f$alpha,
              k = f$k, loss = f$loss, lambda_t = f$lambda_t, schemes = tab, form = set$form, log_scale = log_scale)
  .mhp_output(s, parts, lst, lst[c("lambda", "gcv", "lambda_mhp", "scheme", "alpha", "k", "loss", "lambda_t")],
              as_dt, "fmhp", match.call(),
              sprintf("Fully modified HP filter, lambda = %s, %s weights (alpha = %s, k = %d)", format(f$lambda),
                      fit$scheme, format(f$alpha), f$k))
}

# validated settings of the FMHP filter
.mhp_fmhp_settings <- function(fn, T, max_lambda, k_linear, k_quadratic, alpha_linear, alpha_quadratic, schemes,
                               form, lambda) {
  max_lambda <- .mhp_check_number(max_lambda, "max_lambda", fn, min = 1, integer = TRUE)
  schemes <- unique(vapply(schemes, function(v) .mhp_check_choice(v, c("linear", "quadratic"), "schemes", fn), ""))
  form    <- .mhp_check_choice(form[1L], c("rows", "differences"), "form", fn)
  n <- if (form == "differences") T - 2L else T
  k_linear <- .mhp_check_number(k_linear, "k_linear", fn, min = 1, integer = TRUE)
  if (is.null(k_quadratic)) k_quadratic <- as.integer(.mhp_round(min(22, T / 2 - 5)))
  k_quadratic <- .mhp_check_number(k_quadratic, "k_quadratic", fn, min = 1, integer = TRUE)
  for (v in c(if ("linear" %in% schemes) k_linear, if ("quadratic" %in% schemes) k_quadratic)) {
    if (2L * v > n) .mhp_stop(fn, "the series is too short for ", v, " end values on each side: ", T,
                              " observations (Hanif et al. 2017 ask for more than 40).")
  }
  alpha_linear    <- .mhp_check_number(alpha_linear, "alpha_linear", fn, min = 0, scalar = FALSE)
  alpha_quadratic <- .mhp_check_number(alpha_quadratic, "alpha_quadratic", fn, min = 0, scalar = FALSE)
  if (!is.null(lambda)) lambda <- .mhp_check_number(lambda, "lambda", fn, min = .Machine$double.eps)
  list(max_lambda = max_lambda, k_linear = k_linear, k_quadratic = k_quadratic, alpha_linear = alpha_linear,
       alpha_quadratic = alpha_quadratic, schemes = schemes, form = form, lambda = lambda)
}

utils::globalVariables("time")
