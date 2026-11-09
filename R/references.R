#-------------------------------------------------------------------------------------------------------------------
# The sources of every method: one registry gives the short citation printed under each result, the references of
# the interpretations and the application, mhp_cite(), inst/CITATION and inst/REFERENCES.bib (written from it by
# tools/write-citation.R).
#-------------------------------------------------------------------------------------------------------------------

.mhp_ref <- function(key, short, text, ...) {
  list(key = key, short = short, text = text, bib = utils::bibentry(key = key, ...))
}

.mhp_p <- function(given, family) utils::person(given = given, family = family)

.mhp_refs_list <- function() {
  p <- .mhp_p
  refs <- list(
    .mhp_ref("HodrickPrescott1997", "Hodrick and Prescott (1997)",
             paste("Hodrick, R. J. and Prescott, E. C. (1997). Postwar U.S. business cycles: an empirical",
                   "investigation. Journal of Money, Credit and Banking, 29(1), 1-16. doi:10.2307/2953682"),
             bibtype = "Article", title = "Postwar {U.S.} Business Cycles: An Empirical Investigation",
             author = c(p(c("Robert", "J."), "Hodrick"), p(c("Edward", "C."), "Prescott")),
             journal = "Journal of Money, Credit and Banking", year = "1997", volume = "29", number = "1",
             pages = "1--16", doi = "10.2307/2953682"),
    .mhp_ref("RavnUhlig2002", "Ravn and Uhlig (2002)",
             paste("Ravn, M. O. and Uhlig, H. (2002). On adjusting the Hodrick-Prescott filter for the frequency of",
                   "observations. The Review of Economics and Statistics, 84(2), 371-376.",
                   "doi:10.1162/003465302317411604"),
             bibtype = "Article",
             title = "On Adjusting the {Hodrick-Prescott} Filter for the Frequency of Observations",
             author = c(p(c("Morten", "O."), "Ravn"), p("Harald", "Uhlig")),
             journal = "The Review of Economics and Statistics", year = "2002", volume = "84", number = "2",
             pages = "371--376", doi = "10.1162/003465302317411604"),
    .mhp_ref("McDermott1997", "McDermott (1997)",
             paste("McDermott, C. J. (1997). An automatic method for choosing the smoothing parameter in the HP",
                   "filter. Unpublished, International Monetary Fund, Washington DC."),
             bibtype = "Unpublished",
             title = "An Automatic Method for Choosing the Smoothing Parameter in the {HP} Filter",
             author = p(c("C.", "John"), "McDermott"), note = "International Monetary Fund, Washington DC",
             year = "1997"),
    .mhp_ref("CoeMcDermott1997", "Coe and McDermott (1997)",
             paste("Coe, D. T. and McDermott, C. J. (1997). Does the gap model work in Asia? IMF Staff Papers, 44(1),",
                   "59-80. doi:10.2307/3867497"),
             bibtype = "Article", title = "Does the Gap Model Work in {Asia}?",
             author = c(p(c("David", "T."), "Coe"), p(c("C.", "John"), "McDermott")), journal = "IMF Staff Papers",
             year = "1997", volume = "44", number = "1", pages = "59--80", doi = "10.2307/3867497"),
    .mhp_ref("Choudhary2014", "Choudhary et al. (2014)",
             paste("Choudhary, M. A., Hanif, M. N. and Iqbal, J. (2014). On smoothing macroeconomic time series",
                   "using the modified HP filter. Applied Economics, 46(19), 2205-2214.",
                   "doi:10.1080/00036846.2014.894631"),
             bibtype = "Article",
             title = "On Smoothing Macroeconomic Time Series Using the Modified {HP} Filter",
             author = c(p(c("M.", "Ali"), "Choudhary"), p(c("M.", "Nadim"), "Hanif"), p("Javed", "Iqbal")),
             journal = "Applied Economics", year = "2014", volume = "46", number = "19", pages = "2205--2214",
             doi = "10.1080/00036846.2014.894631"),
    .mhp_ref("Choudhary2014code", "Choudhary et al. (2014, MATLAB code)",
             paste("Choudhary, M. A., Hanif, M. N. and Iqbal, J. (2014). modifiedhpfilternew.m: MATLAB code of the",
                   "modified HP filter of Choudhary, Hanif and Iqbal (2014)."),
             bibtype = "Misc", title = "{modifiedhpfilternew.m}: {MATLAB} Code of the Modified {HP} Filter",
             author = c(p(c("M.", "Ali"), "Choudhary"), p(c("M.", "Nadim"), "Hanif"), p("Javed", "Iqbal")),
             year = "2014"),
    .mhp_ref("CravenWahba1979", "Craven and Wahba (1979)",
             paste("Craven, P. and Wahba, G. (1979). Smoothing noisy data with spline functions. Numerische",
                   "Mathematik, 31, 377-403. doi:10.1007/BF01404567"),
             bibtype = "Article", title = "Smoothing Noisy Data with Spline Functions",
             author = c(p("Peter", "Craven"), p("Grace", "Wahba")), journal = "Numerische Mathematik",
             year = "1979", volume = "31", pages = "377--403", doi = "10.1007/BF01404567"),
    .mhp_ref("Silverman1984", "Silverman (1984)",
             paste("Silverman, B. W. (1984). A fast and efficient cross-validation method for smoothing parameter",
                   "choice in spline regression. Journal of the American Statistical Association, 79(387), 584-589.",
                   "doi:10.1080/01621459.1984.10478084"),
             bibtype = "Article",
             title = "A Fast and Efficient Cross-Validation Method for Smoothing Parameter Choice in Spline Regression",
             author = p(c("Bernard", "W."), "Silverman"), journal = "Journal of the American Statistical Association",
             year = "1984", volume = "79", number = "387", pages = "584--589",
             doi = "10.1080/01621459.1984.10478084"),
    .mhp_ref("Bloechl2014", "Bloechl (2014)",
             paste("Bloechl, A. (2014). Reducing the excess variability of the Hodrick-Prescott filter by flexible",
                   "penalization. Discussion Paper 2014-1, University of Munich."),
             bibtype = "TechReport",
             title = "Reducing the Excess Variability of the {Hodrick-Prescott} Filter by Flexible Penalization",
             author = p("Andreas", "Bloechl"), institution = "University of Munich", type = "Discussion Paper",
             number = "2014-1", year = "2014"),
    .mhp_ref("Hanif2017", "Hanif et al. (2017)",
             paste("Hanif, M. N., Iqbal, J. and Choudhary, M. A. (2017). Fully modified HP filter. SBP Working Paper",
                   "Series, No. 88, State Bank of Pakistan."),
             bibtype = "TechReport", title = "Fully Modified {HP} Filter",
             author = c(p(c("M.", "Nadim"), "Hanif"), p("Javed", "Iqbal"), p(c("M.", "Ali"), "Choudhary")),
             institution = "State Bank of Pakistan", type = "SBP Working Paper Series", number = "88",
             year = "2017"),
    .mhp_ref("Hanif2017code", "Hanif (2017)",
             paste("Hanif, M. N. (2017). Fully modified HP filter function (MATLAB code). MATLAB Central File",
                   "Exchange, 63198."),
             bibtype = "Misc", title = "Fully Modified {HP} Filter Function ({MATLAB} code)",
             author = p(c("M.", "Nadim"), "Hanif"), howpublished = "MATLAB Central File Exchange, 63198",
             year = "2017"),
    .mhp_ref("HarveyJaeger1993", "Harvey and Jaeger (1993)",
             paste("Harvey, A. C. and Jaeger, A. (1993). Detrending, stylized facts and the business cycle. Journal",
                   "of Applied Econometrics, 8(3), 231-247. doi:10.1002/jae.3950080302"),
             bibtype = "Article", title = "Detrending, Stylized Facts and the Business Cycle",
             author = c(p(c("Andrew", "C."), "Harvey"), p("Albert", "Jaeger")),
             journal = "Journal of Applied Econometrics", year = "1993", volume = "8", number = "3",
             pages = "231--247", doi = "10.1002/jae.3950080302"),
    .mhp_ref("GuayStAmant2005", "Guay and St-Amant (2005)",
             paste("Guay, A. and St-Amant, P. (2005). Do the Hodrick-Prescott and Baxter-King filters provide a",
                   "good approximation of business cycles? Annales d'Economie et de Statistique, 77, 133-155.",
                   "doi:10.2307/20079119"),
             bibtype = "Article",
             title = paste("Do the {Hodrick-Prescott} and {Baxter-King} Filters Provide a Good Approximation of",
                           "Business Cycles?"),
             author = c(p("Alain", "Guay"), p("Pierre", "St-Amant")), journal = "Annales d'Economie et de Statistique",
             year = "2005", volume = "77", pages = "133--155", doi = "10.2307/20079119")
  )
  stats::setNames(refs, vapply(refs, `[[`, "", "key"))
}

# the sources of a result
.mhp_sources_of <- function(x) {
  cls <- .mhp_cls(x)
  ex  <- identical(x$criterion, "exact")
  switch(cls,
    hp         = c("HodrickPrescott1997", "RavnUhlig2002"),
    mhp        = c("HodrickPrescott1997", "McDermott1997", "CoeMcDermott1997", "Choudhary2014", "Choudhary2014code",
                   if (ex) c("CravenWahba1979", "Silverman1984")),
    fmhp       = c("Hanif2017", "Hanif2017code", "Bloechl2014", "McDermott1997", "HodrickPrescott1997"),
    weights    = c("HodrickPrescott1997", "Hanif2017"),
    gain       = c("Bloechl2014", "Hanif2017"),
    loss       = c("Bloechl2014", "Hanif2017"),
    simulation = c("Hanif2017", "Choudhary2014", "HarveyJaeger1993", "GuayStAmant2005"),
    character(0))
}

# "Sources: Hodrick and Prescott (1997); ..." for the end of a print
.mhp_sources_line <- function(x) {
  k <- unique(.mhp_sources_of(x))
  if (!length(k)) return(invisible(NULL))
  r <- .mhp_refs_list()[k]
  txt <- strwrap(paste0("Sources: ", paste(vapply(r, `[[`, "", "short"), collapse = "; "),
                        ". Full references: mhp_cite(x)."), width = getOption("width", 80L))
  cat("\n", paste(txt, collapse = "\n"), "\n", sep = "")
  invisible(NULL)
}

#' References of a result or of the package
#'
#' @description
#' \code{mhp_cite()} gives the references of the methods behind a result of the package, or the references of every
#' method in the package, as text or BibTeX. \code{citation("mhpfilter")} cites the package and its main methods.
#'
#' @param x a result of a function of the package (with \code{as_dt = FALSE} for the filters), or \code{NULL}
#'   (default) for every reference of the package.
#' @param style \code{"text"} (default) or \code{"bibtex"}.
#' @return An object of class \code{bibentry} (printed in the chosen style).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' mhp_cite(
#'     x     = NULL
#'   , style = "text"
#'   )
#' data(us_macro)
#' f <-
#'   mhp_filter(
#'       x          = log(us_macro$gdp)
#'     , max_lambda = 100000
#'     , as_dt      = FALSE
#'     , log_scale  = FALSE
#'     , criterion  = "approximate"
#'     )
#' mhp_cite(
#'     x     = f
#'   , style = "bibtex"
#'   )
#' @export
mhp_cite <- function(x = NULL, style = c("text", "bibtex")) UseMethod("mhp_cite")

#' @rdname mhp_cite
#' @export
mhp_cite.default <- function(x = NULL, style = c("text", "bibtex")) {
  style <- .mhp_check_choice(style[1L], c("text", "bibtex"), "style", "mhp_cite")
  refs <- .mhp_refs_list()
  keys <- if (is.null(x)) names(refs) else {
    if (!inherits(x, "mhp_result")) .mhp_stop("mhp_cite", "`x` must be a result of the package (as_dt = FALSE) or NULL.")
    unique(.mhp_sources_of(x))
  }
  out <- do.call(c, lapply(refs[keys], `[[`, "bib"))
  structure(out, mhp_style = style, class = c("mhp_cite", class(out)))
}

#' @export
print.mhp_cite <- function(x, ...) {
  style <- attr(x, "mhp_style")
  y <- x
  class(y) <- setdiff(class(y), "mhp_cite")
  if (!length(y)) {
    cat("No references.\n")
    return(invisible(x))
  }
  if (identical(style, "bibtex")) print(y, style = "Bibtex") else {
    refs <- .mhp_refs_list()
    keys <- vapply(unclass(y), function(e) attr(e, "key"), "")
    cat(paste(vapply(refs[keys], `[[`, "", "text"), collapse = "\n\n"), "\n")
  }
  invisible(x)
}
