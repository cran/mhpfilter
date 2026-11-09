#' mhpfilter: Modified and Fully Modified Hodrick-Prescott Filters
#'
#' @description
#' \pkg{mhpfilter} splits a macroeconomic time series into a trend and a cycle with the filter of Hodrick and
#' Prescott (1997) and with two versions whose settings come from the data: the modified HP filter of McDermott
#' (1997), which chooses the smoothing parameter of each series by generalised cross-validation (Choudhary, Hanif
#' and Iqbal 2014), and the fully modified HP filter of Hanif, Iqbal and Choudhary (2017), which also reduces the
#' bias of the HP filter at the ends of the series by raising the smoothing parameter there, with weights that
#' minimise the end-point loss of Bloechl (2014).
#'
#' @details
#' \strong{Functions}, in the order of an analysis:
#' \itemize{
#'   \item \code{\link{hp_filter}}, \code{\link{mhp_filter}}, \code{\link{fmhp_filter}}: the three filters.
#'   \item \code{\link{mhp_batch}}, \code{\link{fmhp_batch}}: many series at once.
#'   \item \code{\link{mhp_compare}}, \code{\link{batch_compare}}, \code{\link{fmhp_compare}}: the filters
#'     compared, by the moments of the cycles and the end-point loss.
#'   \item \code{\link{hp_weights}}, \code{\link{hp_gain}}, \code{\link{hp_loss}}, \code{\link{hp_schedule}}: the
#'     weights, gains and end-point loss of any HP-type filter.
#'   \item \code{\link{mhp_designs}}, \code{\link{mhp_simulate}}, \code{\link{fmhp_simulation}}: simulated series and
#'     the power comparison of the paper.
#'   \item \code{\link{autoplot.mhp}}, \code{\link{plot_comparison}}, \code{\link{plot_batch}}: graphics;
#'     \code{\link{get_lambda}}, \code{\link{get_gcv}}: accessors.
#'   \item \code{\link{mhp_interpret}}, \code{\link{mhp_cite}}: a plain-language interpretation and the references
#'     of a result; \code{\link{mhp_app}}: the built-in application.
#' }
#' Every filter is an S3 generic with a \code{.default} method. With \code{as_dt = TRUE} (the default) the filters
#' return a \code{data.table}, as in version 0.1.0; with \code{as_dt = FALSE} a list with a class and the methods of
#' \code{\link{mhp-methods}}. Utilities: \code{\link{mhp_theme}}, \code{\link{mhp_save}}, \code{\link{mhp_notation}},
#' \code{\link{mhp_reference}}.
#'
#' \strong{Data.} \code{\link{us_macro}}: quarterly real GDP, consumption and investment of the United States,
#' 1959 to 2009.
#'
#' \strong{Vignettes.} Start with \code{vignette("mhpfilter_01_getting_started", package = "mhpfilter")};
#' \code{browseVignettes("mhpfilter")} lists all of them, in HTML and PDF.
#'
#' @section Notation:
#' Scalars are upper-case letters in normal face, indices lower-case italic, vectors lower-case bold and matrices
#' upper-case bold; Greek letters follow the same rule. The full table is \code{mhp_notation()}.
#' \tabular{ll}{
#'   \eqn{T}{T} \tab number of observations \cr
#'   \eqn{\mathbf{x}}{x}, \eqn{\mathbf{g}}{g}, \eqn{\mathbf{c}}{c} \tab series, trend and cycle \cr
#'   \eqn{\mathbf{K}}{K}, \eqn{\mathbf{A} = \mathbf{K}^\top\mathbf{K}}{A = K'K} \tab second differences and the
#'     penalty matrix \cr
#'   \eqn{\mathbf{B}}{B} \tab smoother (filter weights) \cr
#'   \eqn{\lambda}{lambda}, \eqn{\lambda_t}{lambda_t} \tab smoothing parameter(s) \cr
#'   \eqn{\alpha}{alpha}, \eqn{k}{k} \tab end-point weight and number of end values \cr
#'   \eqn{P_t(\omega)}{P_t(omega)}, \eqn{l(t)}{l(t)}, \eqn{L}{L} \tab gain, loss and total loss \cr
#' }
#'
#' @references
#' Bloechl, A. (2014). Reducing the excess variability of the Hodrick-Prescott filter by flexible penalization.
#' \emph{Discussion Paper} 2014-1, University of Munich.
#'
#' Choudhary, M. A., Hanif, M. N. and Iqbal, J. (2014). On smoothing macroeconomic time series using the modified HP
#' filter. \emph{Applied Economics}, \strong{46}(19), 2205--2214. \doi{10.1080/00036846.2014.894631}
#'
#' Hanif, M. N., Iqbal, J. and Choudhary, M. A. (2017). Fully modified HP filter. \emph{SBP Working Paper Series},
#' No. 88, State Bank of Pakistan.
#'
#' Hodrick, R. J. and Prescott, E. C. (1997). Postwar U.S. business cycles: an empirical investigation.
#' \emph{Journal of Money, Credit and Banking}, \strong{29}(1), 1--16. \doi{10.2307/2953682}
#'
#' McDermott, C. J. (1997). An automatic method for choosing the smoothing parameter in the HP filter. Unpublished,
#' International Monetary Fund, Washington DC.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com}), ORCID 0000-0002-5923-1714.
#' @seealso \code{\link{mhp-methods}}, \code{\link{mhp_notation}}
#' @importFrom data.table data.table setDT copy as.data.table := .N .SD
#' @importFrom stats sd
#' @useDynLib mhpfilter, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("term", "value", "computed", "agrees", "digits", "symbol", "meaning", "argument"))
