#' Extract the smoothing parameter or the criterion of a result
#'
#' @description
#' \code{get_lambda()} returns the smoothing parameter and \code{get_gcv()} the value of McDermott's criterion of a
#' result of \code{\link{hp_filter}()}, \code{\link{mhp_filter}()} or \code{\link{fmhp_filter}()}, whether it is a
#' \code{data.table} (\code{as_dt = TRUE}) or a list (\code{as_dt = FALSE}).
#'
#' @param x a filter result.
#' @return \code{get_lambda()}: the smoothing parameter; \code{get_gcv()}: the criterion at that value.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' data(us_macro)
#' m <-
#'   mhp_filter(
#'       x          = log(us_macro$gdp)
#'     , max_lambda = 100000
#'     , as_dt      = TRUE
#'     , log_scale  = FALSE
#'     , criterion  = c("approximate", "exact")
#'     )
#' get_lambda(x = m)
#' get_gcv(x = m)
#' @export
get_lambda <- function(x) UseMethod("get_lambda")

#' @rdname get_lambda
#' @export
get_lambda.default <- function(x) {
  lam <- attr(x, "lambda")
  if (is.null(lam) && is.list(x)) lam <- x$lambda
  if (is.null(lam)) .mhp_stop("get_lambda", "no lambda found in `x`.")
  lam
}

#' @rdname get_lambda
#' @export
get_gcv <- function(x) UseMethod("get_gcv")

#' @rdname get_lambda
#' @export
get_gcv.default <- function(x) {
  gcv <- attr(x, "gcv")
  if (is.null(gcv) && is.list(x)) gcv <- x$gcv
  if (is.null(gcv)) .mhp_stop("get_gcv", "no GCV found in `x`.")
  gcv
}
