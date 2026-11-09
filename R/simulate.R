#-------------------------------------------------------------------------------------------------------------------
# Simulation of trend-plus-cycle series (Hanif et al. 2017, eqs. 10a and 10b) and the power comparison of the
# filters (their Tables 1 and 2).
#-------------------------------------------------------------------------------------------------------------------

#' The 30 simulation designs of Hanif, Iqbal and Choudhary (2017)
#'
#' @description
#' \code{mhp_designs()} lists the 30 combinations of Table 1 of the paper: five ratios
#' \eqn{\sigma_\varepsilon / \sigma_\delta}{sigma_e / sigma_d} of the standard deviations of the trend and cycle
#' shocks (10, 5, 2, 1, 0.5) crossed with six AR(2) cycles \eqn{(\phi_1, \phi_2)}{(phi_1, phi_2)}: (0.9, 0.01),
#' (1.2, -0.25), (1.2, -0.4), (1.2, -0.55), (1.2, -0.75) and (0.8, 0). The designs of Choudhary, Hanif and Iqbal
#' (2014, Table 1) have the same cycles and the ratios 10, 5, 1, 0.5 and 0.01.
#'
#' @param paper \code{"2017"} (default, Hanif et al.) or \code{"2014"} (Choudhary et al.).
#' @return A \code{data.table} with the columns \code{model} (1 to 30, as in the paper), \code{sd_ratio},
#'   \code{ar1}, \code{ar2}.
#' @references
#' Choudhary, M. A., Hanif, M. N. and Iqbal, J. (2014). On smoothing macroeconomic time series using the modified HP
#' filter. \emph{Applied Economics}, \strong{46}(19), 2205--2214. \doi{10.1080/00036846.2014.894631}
#'
#' Hanif, M. N., Iqbal, J. and Choudhary, M. A. (2017). Fully modified HP filter. \emph{SBP Working Paper Series},
#' No. 88, State Bank of Pakistan.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_simulate}}, \code{\link{fmhp_simulation}}
#' @examples
#' mhp_designs(
#'     paper = "2017"
#'   )
#' @export
mhp_designs <- function(paper = c("2017", "2014")) {
  paper <- .mhp_check_choice(as.character(paper[1L]), c("2017", "2014"), "paper", "mhp_designs")
  ar <- data.table::data.table(ar1 = c(0.9, 1.2, 1.2, 1.2, 1.2), ar2 = c(0.01, -0.25, -0.4, -0.55, -0.75))
  r  <- if (paper == "2017") c(10, 5, 2, 1, 0.5) else c(10, 5, 1, 0.5, 0.01)
  d  <- data.table::rbindlist(c(lapply(r, function(v) cbind(sd_ratio = v, ar)),
                                list(data.table::data.table(sd_ratio = r, ar1 = 0.8, ar2 = 0))))
  cbind(model = seq_len(nrow(d)), d)
}

#' Simulate a series with a known trend and cycle
#'
#' @description
#' \code{mhp_simulate()} generates a series \eqn{x_t = g_t + c_t}{x_t = g_t + c_t} from the data generating process
#' of Hanif, Iqbal and Choudhary (2017, eqs. 10a and 10b), after Harvey and Jaeger (1993) and Guay and St-Amant
#' (2005): a random walk trend with drift and an AR(2) cycle,
#' \deqn{g_t = \mu + \tau_t + g_{t-1} + \varepsilon_t, \qquad c_t = \phi_1 c_{t-1} + \phi_2 c_{t-2} + \delta_t,}{g_t =
#'   mu + tau_t + g_(t-1) + e_t, c_t = phi_1 c_(t-1) + phi_2 c_(t-2) + d_t,}
#' with \eqn{\varepsilon_t \sim N(0, \sigma_\varepsilon^2)}{e_t ~ N(0, sigma_e^2)} and \eqn{\delta_t \sim N(0,
#' \sigma_\delta^2)}{d_t ~ N(0, sigma_d^2)}. Quarterly series can be aggregated to annual ones as in the paper.
#'
#' @details
#' The paper does not give the drift \eqn{\mu}{mu} or the form of the nonlinear trend term \eqn{\tau_t}{tau_t}. Here
#' \eqn{\tau_t = 0}{tau_t = 0} for \code{trend = "linear"}, and \eqn{\tau_t = \mu\sin(2\pi t/T)}{tau_t = mu sin(2 pi
#' t / T)} for \code{trend = "nonlinear"}, a drift that rises and falls once over the sample. The cycle starts from a
#' burn-in of 100 values. Aggregation to annual data takes every fourth value (\code{"systematic"}), the sum of four
#' consecutive values (\code{"sum"}) or their average (\code{"average"}), for the series and both components.
#'
#' @param T number of (quarterly) observations (default 200, as in the paper).
#' @param sd_ratio \eqn{\sigma_\varepsilon / \sigma_\delta}{sigma_e / sigma_d} (default 1).
#' @param ar \eqn{(\phi_1, \phi_2)}{(phi_1, phi_2)} of the cycle, stationary (default \code{c(1.2, -0.4)}).
#' @param sd_cycle \eqn{\sigma_\delta}{sigma_d} (default 1).
#' @param drift \eqn{\mu}{mu} (default 0.5).
#' @param trend \code{"linear"} (default) or \code{"nonlinear"}.
#' @param aggregate \code{"none"} (default), \code{"systematic"}, \code{"sum"} or \code{"average"}.
#' @param seed seed of the random numbers (\code{NULL}: the current state).
#' @return A \code{data.table} with the columns \code{t}, \code{x}, \code{trend}, \code{cycle}.
#' @references
#' Guay, A. and St-Amant, P. (2005). Do the Hodrick-Prescott and Baxter-King filters provide a good approximation of
#' business cycles? \emph{Annales d'Economie et de Statistique}, \strong{77}, 133--155. \doi{10.2307/20079119}
#'
#' Harvey, A. C. and Jaeger, A. (1993). Detrending, stylized facts and the business cycle. \emph{Journal of Applied
#' Econometrics}, \strong{8}(3), 231--247. \doi{10.1002/jae.3950080302}
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_designs}}, \code{\link{fmhp_simulation}}
#' @examples
#' s <-
#'   mhp_simulate(
#'       T         = 200
#'     , sd_ratio  = 1
#'     , ar        = c(1.2, -0.4)
#'     , sd_cycle  = 1
#'     , drift     = 0.5
#'     , trend     = "linear"
#'     , aggregate = "none"
#'     , seed      = 1
#'     )
#' s
#' @export
mhp_simulate <- function(T = 200L, sd_ratio = 1, ar = c(1.2, -0.4), sd_cycle = 1, drift = 0.5,
                         trend = c("linear", "nonlinear"), aggregate = c("none", "systematic", "sum", "average"),
                         seed = NULL) {
  fn <- "mhp_simulate"
  T <- .mhp_check_number(T, "T", fn, min = 8, integer = TRUE)
  sd_ratio <- .mhp_check_number(sd_ratio, "sd_ratio", fn, min = 0)
  ar <- .mhp_check_number(ar, "ar", fn, scalar = FALSE)
  if (length(ar) != 2L || !(ar[1L] + ar[2L] < 1 && ar[2L] - ar[1L] < 1 && abs(ar[2L]) < 1)) {
    .mhp_stop(fn, "`ar` must be two coefficients of a stationary AR(2) process.")
  }
  sd_cycle  <- .mhp_check_number(sd_cycle, "sd_cycle", fn, min = 0)
  drift     <- .mhp_check_number(drift, "drift", fn)
  trend     <- .mhp_check_choice(trend[1L], c("linear", "nonlinear"), "trend", fn)
  aggregate <- .mhp_check_choice(aggregate[1L], c("none", "systematic", "sum", "average"), "aggregate", fn)
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old,
                                                                                   envir = globalenv()))
    set.seed(seed = seed)
  }
  tt  <- seq_len(T)
  tau <- if (trend == "linear") 0 else drift * sin(2 * pi * tt / T)
  g   <- cumsum(drift + tau + stats::rnorm(T, sd = sd_ratio * sd_cycle))
  cc  <- as.numeric(stats::filter(stats::rnorm(T + 100L, sd = sd_cycle), ar, method = "recursive"))[-seq_len(100L)]
  if (aggregate != "none") {
    n  <- T %/% 4L
    id <- rep(seq_len(n), each = 4L)[seq_len(4L * n)]
    agg <- switch(aggregate,
                  systematic = function(v) v[seq(4L, 4L * n, by = 4L)],
                  sum        = function(v) as.numeric(tapply(v[seq_len(4L * n)], id, sum)),
                  average    = function(v) as.numeric(tapply(v[seq_len(4L * n)], id, mean)))
    g  <- agg(g)
    cc <- agg(cc)
  }
  data.table::data.table(t = seq_along(g), x = g + cc, trend = g, cycle = cc)
}

#' Power comparison of the filters on simulated series
#'
#' @description
#' \code{fmhp_simulation()} repeats the simulation study of Hanif, Iqbal and Choudhary (2017, Tables 1 and 2): for
#' each design it simulates series with \code{\link{mhp_simulate}()}, filters them with the HP, modified HP and fully
#' modified HP filters, and compares the root mean square error of the estimated cycles against the true one, over
#' the whole series, its middle and its ends.
#'
#' @details
#' The RMSE of a filter is \eqn{\{T^{-1}\sum_t(\hat{c}_t - c_t)^2\}^{1/2}}{sqrt(mean((c_hat - c)^2))}; the middle is
#' the central 80\% of the observations and the ends the first and last 10\%. \code{fmhp_better} is the share of
#' replicates in which the FMHP filter has a smaller RMSE than the HP filter (Table 1 of the paper reports it in
#' percent). The series are filtered on their own scale (\code{log_scale = FALSE}).
#'
#' @param designs designs as in \code{\link{mhp_designs}()} (default: all 30).
#' @param n_rep replicates per design (default 100; the paper used 1000).
#' @param T number of quarterly observations (default 200).
#' @param trend,aggregate as in \code{\link{mhp_simulate}()}.
#' @param lambda the HP smoothing parameter (default \code{NULL}: 1600, or 100 for aggregated series).
#' @param max_lambda the largest \eqn{\lambda}{lambda} of the searches (default 3000).
#' @param seed seed of the random numbers (\code{NULL}: the current state).
#' @param form the form of the end-point weights of the fully modified HP filter (default \code{"rows"}); see
#'   \code{\link{fmhp_filter}()}.
#' @return An object of class \code{c("mhp_simulation", "mhp_result", "list")}: \code{rmse} (a \code{data.table}
#'   with one row per design and filter: \code{model}, \code{sd_ratio}, \code{ar1}, \code{ar2}, \code{method},
#'   \code{rmse}, \code{rmse_middle}, \code{rmse_ends}), \code{power} (one row per design: the share of replicates in
#'   which FMHP beats HP and MHP) and \code{settings}. \code{plot()} draws the RMSE by design.
#' @references
#' Hanif, M. N., Iqbal, J. and Choudhary, M. A. (2017). Fully modified HP filter. \emph{SBP Working Paper Series},
#' No. 88, State Bank of Pakistan.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_simulate}}, \code{\link{fmhp_filter}}
#' @examples
#' sim <-
#'   fmhp_simulation(
#'       designs    = mhp_designs()[c(3, 13, 23)]
#'     , n_rep      = 2
#'     , T          = 120
#'     , trend      = "linear"
#'     , aggregate  = "none"
#'     , lambda     = NULL
#'     , max_lambda = 3000
#'     , seed       = 1
#'     , form       = "rows"
#'     )
#' sim$power
#' @export
fmhp_simulation <- function(designs = mhp_designs(), n_rep = 100L, T = 200L, trend = c("linear", "nonlinear"),
                            aggregate = c("none", "systematic", "sum", "average"), lambda = NULL,
                            max_lambda = 3000L, seed = NULL, form = c("rows", "differences")) {
  fn <- "fmhp_simulation"
  if (!is.data.frame(designs) || !all(c("sd_ratio", "ar1", "ar2") %in% names(designs))) {
    .mhp_stop(fn, "`designs` must be a table with the columns sd_ratio, ar1 and ar2 (see mhp_designs()).")
  }
  designs <- data.table::as.data.table(designs)
  if (!"model" %in% names(designs)) designs[, model := seq_len(.N)]
  n_rep <- .mhp_check_number(n_rep, "n_rep", fn, min = 1, integer = TRUE)
  trend     <- .mhp_check_choice(trend[1L], c("linear", "nonlinear"), "trend", fn)
  aggregate <- .mhp_check_choice(aggregate[1L], c("none", "systematic", "sum", "average"), "aggregate", fn)
  form      <- .mhp_check_choice(form[1L], c("rows", "differences"), "form", fn)
  hp_lambda <- if (is.null(lambda)) (if (aggregate == "none") 1600 else 100) else
    .mhp_check_number(lambda, "lambda", fn, min = .Machine$double.eps)
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (is.null(old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", old,
                                                                                   envir = globalenv()))
    set.seed(seed = seed)
  }
  rmse <- function(e, i) sqrt(mean(e[i]^2))
  one <- function(d) {
    s  <- mhp_simulate(T = T, sd_ratio = d$sd_ratio, ar = c(d$ar1, d$ar2), trend = trend, aggregate = aggregate)
    n  <- nrow(s)
    ne <- max(1L, as.integer(.mhp_round(0.1 * n)))
    ends <- c(seq_len(ne), seq.int(n - ne + 1L, n))
    mid  <- setdiff(seq_len(n), ends)
    cyc <- list(
        HP                  = s$x - .mhp_trend(s$x, hp_lambda)
      , `Modified HP`       = s$x - .mhp_trend(s$x, .mhp_lambda(s$x, max_lambda)$lambda)
      , `Fully modified HP` = fmhp_filter.default(s$x, max_lambda = max_lambda, form = form, log_scale = FALSE,
                                                  as_dt = FALSE)$cycle
      )
    data.table::rbindlist(lapply(names(cyc), function(m) {
      e <- cyc[[m]] - s$cycle
      data.table::data.table(method = m, rmse = rmse(e, seq_len(n)), rmse_middle = rmse(e, mid),
                             rmse_ends = rmse(e, ends))
    }))
  }
  reps <- data.table::rbindlist(lapply(seq_len(nrow(designs)), function(i) {
    d <- designs[i]
    data.table::rbindlist(lapply(seq_len(n_rep), function(r) cbind(d, rep = r, one(d))))
  }))
  by <- c("model", "sd_ratio", "ar1", "ar2")
  rm_tab <- reps[, .(rmse = mean(rmse), rmse_middle = mean(rmse_middle), rmse_ends = mean(rmse_ends)),
                 by = c(by, "method")]
  wide <- data.table::dcast(reps, model + rep ~ method, value.var = "rmse")
  pw <- wide[, .(fmhp_better_hp = mean(`Fully modified HP` < HP), fmhp_better_mhp = mean(`Fully modified HP` <
                   `Modified HP`)), by = model]
  pw <- merge(designs[, c("model", "sd_ratio", "ar1", "ar2"), with = FALSE], pw, by = "model")
  .mhp_result(list(rmse = rm_tab, power = pw,
                   settings = list(n_rep = n_rep, T = T, trend = trend, aggregate = aggregate, lambda = hp_lambda,
                                   max_lambda = max_lambda, form = form)),
              cls = "simulation", call = match.call(),
              label = sprintf("Power comparison: %d designs, %d replicates, %s trend, aggregation %s", nrow(designs),
                              n_rep, trend, aggregate))
}

utils::globalVariables(c("model", "rmse", "rmse_middle", "rmse_ends", "HP", "Fully modified HP", "Modified HP",
                         ".", ".N"))
