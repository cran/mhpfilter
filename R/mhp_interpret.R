#' Plain-language interpretation of a result
#'
#' @description
#' \code{mhp_interpret()} turns a result of the package into a short report: a headline, findings, recommendations
#' and cautions, computed from its numbers, with the sources of the method. It covers the filters
#' (\code{\link{hp_filter}()}, \code{\link{mhp_filter}()}, \code{\link{fmhp_filter}()}, as lists or as
#' \code{data.table}s), the end-point loss (\code{\link{hp_loss}()}) and the power comparison
#' (\code{\link{fmhp_simulation}()}).
#'
#' @details
#' For a filter the report compares the chosen smoothing parameter with the conventional one of the frequency (1600
#' quarterly, 100 annual, 129600 monthly; known when the series is a \code{ts}), names the largest booms and
#' recessions of the cycle, and warns when a search ended at its limit or the series is short. For the fully
#' modified HP filter it also gives the reduction of the end-point loss against the HP filter with the same
#' \eqn{\lambda}{lambda}.
#'
#' @param x a result of a function of the package.
#' @param top number of largest positive and negative cycle values named (default 3).
#' @param ... unused.
#' @param style \code{"text"} (default) or \code{"markdown"}.
#' @param width line width of the text (default \code{getOption("width")}).
#' @return An object of class \code{"mhp_interpretation"}: a list with \code{analysis}, \code{title},
#'   \code{headline}, \code{findings}, \code{recommendations}, \code{cautions}, \code{facts} (the numbers behind the
#'   sentences), \code{guide} (how to read the method) and \code{sources}. \code{format()} gives it as text or
#'   Markdown.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{mhp_cite}}, \code{\link{mhp_app}}
#' @examples
#' data(us_macro)
#' gdp <- ts(data = us_macro$gdp, start = c(1959, 1), frequency = 4)
#' f <-
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
#' mhp_interpret(
#'     x   = f
#'   , top = 3
#'   )
#' @export
mhp_interpret <- function(x, top = 3L, ...) UseMethod("mhp_interpret")

#' @rdname mhp_interpret
#' @export
mhp_interpret.default <- function(x, top = 3L, ...) {
  fn  <- "mhp_interpret"
  top <- .mhp_check_number(top, "top", fn, min = 1, integer = TRUE)
  x <- .mhp_as_result(x, fn)
  cls <- .mhp_cls(x)
  f <- get0(paste0(".mhp_int_", cls), envir = asNamespace("mhpfilter"), inherits = FALSE)
  if (is.null(f)) .mhp_stop(fn, "no interpretation is defined for ", cls, " results.")
  out <- f(x, list(top = top))
  out$analysis <- cls
  out$guide    <- .mhp_guides[[cls]]
  out$sources  <- unname(vapply(.mhp_refs_list()[unique(.mhp_sources_of(x))], `[[`, "", "text"))
  structure(out[c("analysis", "title", "headline", "findings", "recommendations", "cautions", "facts", "guide",
                  "sources")], class = "mhp_interpretation")
}

#' @rdname mhp_interpret
#' @export
print.mhp_interpretation <- function(x, ...) {
  cat(format(x, style = "text"), sep = "\n")
  invisible(x)
}

#' @rdname mhp_interpret
#' @export
format.mhp_interpretation <- function(x, style = c("text", "markdown"), width = getOption("width", 80L), ...) {
  style <- .mhp_check_choice(style[1L], c("text", "markdown"), "style", "format")
  wrap <- function(s, prefix) {
    if (!length(s)) return(character(0))
    unlist(lapply(s, function(z) {
      w <- strwrap(z, width = max(40L, width - 2L) - nchar(prefix))
      c(paste0(prefix, w[1L]), if (length(w) > 1L) paste0(strrep(" ", nchar(prefix)), w[-1L]))
    }))
  }
  sec <- function(h, s) if (length(s)) c("", if (style == "markdown") paste("###", h) else paste0(h, ":"),
                                          wrap(s, "- ")) else character(0)
  c(if (style == "markdown") paste("##", x$title) else x$title,
    "", wrap(x$headline, ""),
    sec("Findings", x$findings), sec("Recommendations", x$recommendations), sec("Cautions", x$cautions),
    sec("Sources", x$sources))
}

# a data.table result of a filter as the list result (its attributes hold lambda and the rest)
.mhp_as_result <- function(x, fn) {
  if (inherits(x, "mhp_result")) return(x)
  if (data.table::is.data.table(x) && all(c("original", "trend", "cycle") %in% names(x)) &&
      !is.null(attr(x, "lambda"))) {
    a <- attributes(x)
    cls <- if (!is.null(a$scheme)) "fmhp" else if (!is.null(a$gcv)) "mhp" else "hp"
    lst <- c(list(original = x$original, trend = x$trend, cycle = x$cycle),
             a[intersect(names(a), c("lambda", "gcv", "lambda_mhp", "scheme", "alpha", "k", "loss", "lambda_t"))],
             list(time = if ("time" %in% names(x)) x$time else NULL))
    res <- .mhp_result(lst, cls = cls)
    class(res) <- append(class(res), "mhp", after = 1L)
    return(res)
  }
  .mhp_stop(fn, "`x` must be a result of a function of the mhpfilter package.")
}

.mhp_n <- function(v, d = 4L) vapply(v, function(z) format(signif(z, d), big.mark = ",", trim = TRUE,
                                                            scientific = FALSE), "")

# times of the largest positive and negative cycle values
.mhp_extremes <- function(x, top) {
  tt <- .mhp_time(x)
  lab <- function(i) if (is.null(x$time)) as.character(i) else {
    f <- if (is.null(x$frequency)) 1 else x$frequency
    if (f == 4) sprintf("%dQ%d", floor(tt[i] + 1e-8), as.integer(round((tt[i] %% 1) * 4)) + 1L) else
      format(round(tt[i], 2))
  }
  o <- order(x$cycle, decreasing = TRUE)
  list(up = vapply(utils::head(o, top), lab, ""), down = vapply(utils::head(rev(o), top), lab, ""))
}

.mhp_conventional <- function(x) {
  f <- x$frequency
  if (is.null(f)) return(NULL)
  switch(as.character(f), `4` = 1600, `1` = 100, `12` = 129600, NULL)
}

.mhp_int_filter <- function(x, ctx) {
  meth <- .mhp_method(x)
  T  <- length(x$original)
  cy <- if (isTRUE(x$log_scale)) x$cycle_log else x$cycle
  ext <- .mhp_extremes(x, ctx$top)
  conv <- .mhp_conventional(x)
  cmp <- if (is.null(conv) || inherits(x, "mhp_hp")) "" else
    sprintf(", %s the conventional %s for this frequency", if (x$lambda < conv) "below" else if (x$lambda > conv)
      "above" else "equal to", .mhp_n(conv))
  head <- switch(.mhp_cls(x),
    hp   = sprintf("The HP filter with lambda = %s gives a cycle with standard deviation %s and first-order
                   autocorrelation %.2f.", .mhp_n(x$lambda), .mhp_n(stats::sd(cy), 3), .mhp_ar1(cy)),
    mhp  = sprintf("The modified HP filter chose lambda = %s from the data%s: %s.", .mhp_n(x$lambda), cmp,
                   if (!is.null(conv) && x$lambda < conv) "the trend follows the series more closely and the cycle is smaller" else
                     if (!is.null(conv) && x$lambda > conv) "the trend is smoother and the cycle larger" else
                       "every series gets its own degree of smoothing"),
    fmhp = sprintf("The fully modified HP filter chose lambda = %s (the modified HP filter alone: %s) and %s end-point
                   weights on %d values at each end, which cut the end-point loss of the HP filter with the same
                   lambda by %.0f%%.", .mhp_n(x$lambda), .mhp_n(x$lambda_mhp), x$scheme, x$k,
                   100 * (1 - x$loss / sum(.mhp_loss_cpp(T, rep(x$lambda_mhp, length(x$lambda_t)), .mhp_theta(),
                                                         1L, .mhp_centre(T), 0.1)))))
  head <- gsub("\\s+", " ", head)
  find <- c(
      sprintf("Cycle%s: standard deviation %s, first-order autocorrelation %.2f, range %s.",
              if (isTRUE(x$log_scale)) " (log scale, about the share of the trend)" else "", .mhp_n(stats::sd(cy), 3),
              .mhp_ar1(cy), .mhp_n(diff(range(cy)), 3))
    , sprintf("Largest positive cycle values (booms) at %s; largest negative ones (recessions) at %s.",
              paste(ext$up, collapse = ", "), paste(ext$down, collapse = ", "))
    , if (inherits(x, "mhp_fmhp")) sprintf("Linear weights: loss %.3f (alpha = %s); quadratic weights: loss %.3f (alpha =
                                          %s); the %s scheme was kept.", .mhp_sch(x, "linear", "loss"),
                                          .mhp_sch(x, "linear", "alpha"), .mhp_sch(x, "quadratic", "loss"),
                                          .mhp_sch(x, "quadratic", "alpha"), x$scheme)
    )
  rec <- c(
      if (inherits(x, "mhp_hp")) "Let the data choose lambda with mhp_filter(), and reduce the end-point bias with fmhp_filter()."
    , if (inherits(x, "mhp_mhp")) "The end values of the cycle carry the end-point bias of the HP filter; fmhp_filter() reduces it."
    , "Compare the filters on this series with fmhp_compare() (moments of the cycles and end-point loss)."
    )
  cau <- c(
      if (inherits(x, "mhp_mhp") && length(x$gcv_path) && x$lambda == length(x$gcv_path))
        "The criterion is smallest at max_lambda, the end of the search: raise max_lambda."
    , if (inherits(x, "mhp_fmhp") && x$lambda >= 3000 && x$lambda == max(x$lambda, 3000))
        "lambda may sit at the end of the search (max_lambda = 3000 by default): check with a larger max_lambda."
    , if (T < 40) sprintf("Only %d observations: the end values make up much of the series.", T)
    , if (!isTRUE(x$log_scale) && all(x$original > 0) && max(x$original) / min(x$original) > 5)
        "The series grows by a large factor; filter its logarithm (log_scale = TRUE) so that the cycle is a percentage."
    , "The last values of the cycle are revised as new observations arrive: read them with care."
    )
  list(title = paste(meth, "filter"), headline = head, findings = find, recommendations = rec,
       cautions = gsub("\\s+", " ", cau),
       facts = list(lambda = x$lambda, T = T, cycle_sd = stats::sd(cy), ar1 = .mhp_ar1(cy)))
}
.mhp_int_hp   <- .mhp_int_filter
.mhp_int_mhp  <- .mhp_int_filter
.mhp_int_fmhp <- .mhp_int_filter

.mhp_sch <- function(x, s, v) {
  r <- x$schemes[x$schemes$scheme == s]
  if (!nrow(r)) return(NA)
  r[[v]]
}

.mhp_int_loss <- function(x, ctx) {
  T <- nrow(x$loss)
  ne <- max(1L, as.integer(.mhp_round(0.1 * T)))
  ends <- c(seq_len(ne), seq.int(T - ne + 1L, T))
  sh <- sum(x$loss$loss[ends]) / x$total
  list(title = "End-point loss",
       headline = sprintf("The total end-point loss is %.4f; %.0f%% of it comes from the first and last %d estimates.",
                          x$total, 100 * sh, ne),
       findings = sprintf("Largest loss %.4f at t = %d; the loss in the middle is %.2g.", max(x$loss$loss),
                          x$loss$t[which.max(x$loss$loss)], x$loss$loss[.mhp_centre(T)]),
       recommendations = "Smaller is better: the fully modified HP filter chooses its end-point weights to reduce this loss.",
       cautions = "The loss depends only on the length of the series and the smoothing parameters, not on the data.",
       facts = list(total = x$total, share_ends = sh))
}

.mhp_int_weights <- function(x, ctx) {
  W <- x$weights
  T <- nrow(W)
  c0 <- .mhp_centre(T)
  rng <- function(i) diff(range(W[i, ]))
  list(title = "Filter weights",
       headline = sprintf("The estimate at t = 1 puts weight %.3f on the first observation, against %.3f on the
                          observation itself in the middle of the series (t = %d).", W[1L, 1L], W[c0, c0], c0),
       findings = sprintf("Range of the weights: %.3f at t = 1, %.3f at t = %d, %.3f at t = %d.", rng(1L), rng(c0), c0,
                          rng(T), T),
       recommendations = "Asymmetric weights at the ends are the source of the end-point bias; see hp_loss().",
       cautions = character(0), facts = list(w11 = W[1L, 1L], wcc = W[c0, c0]))
}

.mhp_int_gain <- function(x, ctx) {
  g  <- x$gain
  hi <- max(g$theta)
  T  <- max(g$t)
  c0 <- .mhp_centre(T)
  at <- function(tt) g[t == tt & theta == hi, gain]
  list(title = "Gain functions",
       headline = sprintf("At the highest frequency the trend lets through %.3f of an oscillation at t = 1, against
                          %.3f in the middle of the series.", at(1L), at(c0)),
       findings = sprintf("Gain at frequency %.2f: %.3f (t = 1), %.3f (t = %d), %.3f (t = %d).", hi, at(1L), at(c0), c0,
                          at(T), T),
       recommendations = "A trend that passes high frequencies at the ends keeps part of the cycle there.",
       cautions = character(0), facts = list(gain_end = at(1L), gain_middle = at(c0)))
}

.mhp_int_simulation <- function(x, ctx) {
  m <- x$rmse[, .(rmse = mean(rmse), rmse_ends = mean(rmse_ends)), by = method]
  best <- m$method[which.min(m$rmse)]
  list(title = "Power comparison",
       headline = sprintf("Over %d designs and %d replicates each, the %s filter has the smallest average RMSE of the
                          cycle (%s).", nrow(x$power), x$settings$n_rep, best, .mhp_n(min(m$rmse), 3)),
       findings = c(sprintf("%s: average RMSE %s (ends %s).", m$method, .mhp_n(m$rmse, 3), .mhp_n(m$rmse_ends, 3)),
                    sprintf("The fully modified HP filter beats the HP filter in %.0f%% of the replicates and the
                            modified HP filter in %.0f%%.", 100 * mean(x$power$fmhp_better_hp),
                            100 * mean(x$power$fmhp_better_mhp))),
       recommendations = "Increase n_rep (the paper used 1000) before drawing firm conclusions.",
       cautions = "The drift and the nonlinear trend of the simulated series are choices of the package; the paper does not state them.",
       facts = list(rmse = m))
}

.mhp_guides <- list(
    hp   = paste("The HP filter fits a smooth trend through the series; lambda sets how smooth it is (larger means",
                 "smoother). The cycle is the series minus the trend. On the log scale the cycle is close to the",
                 "percentage deviation from the trend.")
  , mhp  = paste("The modified HP filter tries every lambda from 1 to max_lambda and keeps the one with the smallest",
                 "cross-validation criterion: the smoothing that best predicts each observation from the others.")
  , fmhp = paste("The fully modified HP filter also raises lambda at the first and last observations, where the HP",
                 "filter has too little information and its trend follows the series too closely. The weights are",
                 "chosen to make the filter at the ends behave like the filter in the middle (the end-point loss).")
  , loss = paste("For each estimate the loss compares its gain function, the share of each frequency it lets",
                 "through, with that of the estimate in the middle of the series.")
  , weights = paste("Row t of the weight matrix gives the weight of every observation in the trend at time t.")
  , gain = paste("The gain of the trend at a frequency is the share of an oscillation of that frequency that the",
                 "trend keeps; a good trend keeps low frequencies and removes high ones.")
  , simulation = paste("Series with a known trend and cycle are filtered; the root mean square error of the",
                       "estimated cycle measures each filter, over the whole series, the middle and the ends.")
  )

utils::globalVariables(c("method", "rmse", "rmse_ends", "t", "theta", "gain"))
