#-------------------------------------------------------------------------------------------------------------------
# Result objects (common layer; synced from SKILL.qmd). A result is the named list the function has always
# returned (same component names, so old code keeps working) with a class and three attributes: the call, a
# one-line label and meta data. Methods: print, summary, format, plot, as.data.table, as.data.frame.
#-------------------------------------------------------------------------------------------------------------------

# the call of a result as the user wrote it: the generic, not its .default method
.mhp_clean_call <- function(call) {
  if (is.call(call) && is.name(call[[1L]])) call[[1L]] <- as.name(sub("\\.default$", "", as.character(call[[1L]])))
  call
}

.mhp_result <- function(x, cls, label = "", call = NULL, meta = list()) {
  stopifnot(is.list(x), !is.null(names(x)))
  attr(x, "call")  <- .mhp_clean_call(call)
  attr(x, "label") <- label
  attr(x, "meta")  <- utils::modifyList(
      list(
          package = "mhpfilter"
        , version = as.character(utils::packageVersion("mhpfilter"))
        , time    = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
    , meta
    )
  class(x) <- c(paste0("mhp_", cls), "mhp_result", "list")
  x
}

.mhp_cls <- function(x) sub("^mhp_", "", class(x)[1L])

# a component as a data.table: matrices and data frames keep their row names in a first column `term`
.mhp_as_table <- function(v) {
  if (data.table::is.data.table(v)) return(data.table::copy(v))
  if (is.data.frame(v) || is.matrix(v)) {
    rn <- rownames(v)
    d  <- data.table::as.data.table(as.data.frame(v, check.names = FALSE))
    if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(d))))) d <- cbind(term = rn, d)
    return(d)
  }
  if (is.atomic(v) && !is.null(names(v))) return(data.table::data.table(term = names(v), value = unname(v)))
  data.table::data.table(value = v)
}

#' Methods for mhpfilter results
#'
#' @description
#' The analysis functions of \pkg{mhpfilter} return the named list they have always returned, now with the class
#' \code{c("mhp_<analysis>", "mhp_result", "list")}. Components are read as before (\code{x$ANOVA},
#' \code{x[["Means"]]}); the methods below print, summarise and plot the result and turn any component into a
#' \code{data.table}.
#' @param x,object a mhpfilter result.
#' @param component name or number of the component to convert (default: the first one).
#' @param keep.rownames,row.names,optional as in \code{data.table::as.data.table()} and \code{as.data.frame()};
#'   row names of a matrix or data frame component are always kept, in a first column \code{term}.
#' @param digits significant digits of printed numbers (default 4).
#' @param ... passed on (to \code{print()} of the components, or to the plotting function for \code{plot()}).
#' @return \code{print()} returns \code{x} invisibly; \code{summary()} an object of class
#'   \code{summary.mhp_result}; \code{format()} a character string; \code{as.data.table()} and
#'   \code{as.data.frame()} the chosen component as a table; \code{plot()} a \code{ggplot} object.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @name mhp-methods
NULL

#' @rdname mhp-methods
#' @export
print.mhp_result <- function(x, digits = 4L, ...) {
  f <- paste0(".mhp_print_", .mhp_cls(x))
  ns <- asNamespace("mhpfilter")
  # the package may print the sources of the method under every result (.mhp_sources_line, R/references.R)
  src <- get0(".mhp_sources_line", envir = ns, inherits = FALSE)
  if (exists(f, envir = ns, inherits = FALSE)) {
    get(f, envir = ns)(x, digits = digits, ...)
    if (!is.null(src)) src(x)
    return(invisible(x))
  }
  lab <- attr(x, "label")
  cat(sprintf("<%s>%s\n", class(x)[1L], if (length(lab) && nzchar(lab)) paste0(" ", lab) else ""))
  for (nm in names(x)) {
    cat("\n$", nm, "\n", sep = "")
    v <- x[[nm]]
    if (is.numeric(v) && is.null(dim(v))) print(signif(v, digits), ...) else print(v, digits = digits, ...)
  }
  if (!is.null(src)) src(x)
  invisible(x)
}

#' @rdname mhp-methods
#' @export
summary.mhp_result <- function(object, ...) {
  f <- paste0(".mhp_summary_", .mhp_cls(object))
  ns <- asNamespace("mhpfilter")
  if (exists(f, envir = ns, inherits = FALSE)) return(get(f, envir = ns)(object, ...))
  structure(
      list(
          class      = class(object)[1L]
        , label      = attr(object, "label")
        , components = vapply(object, function(v) {
            d <- dim(v)
            if (is.null(d)) sprintf("%s [%d]", class(v)[1L], length(v)) else
              sprintf("%s [%s]", class(v)[1L], paste(d, collapse = " x "))
          }, "")
        , meta       = attr(object, "meta")
        )
    , class = "summary.mhp_result"
    )
}

#' @rdname mhp-methods
#' @export
print.summary.mhp_result <- function(x, ...) {
  cat(sprintf("%s%s\n", x$class, if (length(x$label) && nzchar(x$label)) paste0(": ", x$label) else ""))
  cat(paste0("  $", format(names(x$components)), "  ", x$components), sep = "\n")
  if (length(x$meta)) cat(sprintf("computed by %s %s at %s\n", x$meta$package, x$meta$version, x$meta$time))
  invisible(x)
}

#' @rdname mhp-methods
#' @export
format.mhp_result <- function(x, ...) {
  lab <- attr(x, "label")
  sprintf("<%s>%s: %s", class(x)[1L], if (length(lab) && nzchar(lab)) paste0(" ", lab) else "",
          paste(names(x), collapse = ", "))
}

#' @rdname mhp-methods
#' @exportS3Method data.table::as.data.table
as.data.table.mhp_result <- function(x, keep.rownames = FALSE, ..., component = 1L) {
  if (is.character(component) && !component %in% names(x)) {
    .mhp_stop("as.data.table", "no component \"", component, "\"; components: ", paste(names(x), collapse = ", "),
                  ".")
  }
  .mhp_as_table(x[[component]])
}

#' @rdname mhp-methods
#' @export
as.data.frame.mhp_result <- function(x, row.names = NULL, optional = FALSE, ..., component = 1L) {
  as.data.frame(as.data.table.mhp_result(x, component = component), row.names = row.names, optional = optional)
}

#' @rdname mhp-methods
#' @export
plot.mhp_result <- function(x, ...) {
  f <- paste0(".mhp_plot_", .mhp_cls(x))
  ns <- asNamespace("mhpfilter")
  if (!exists(f, envir = ns, inherits = FALSE)) {
    .mhp_stop("plot", "no plot is defined for ", class(x)[1L], " results.")
  }
  get(f, envir = ns)(x, ...)
}
