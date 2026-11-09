#-------------------------------------------------------------------------------------------------------------------
# Argument checks and column capture (common layer; synced from SKILL.qmd: edit there, then re-sync).
#-------------------------------------------------------------------------------------------------------------------

.mhp_stop <- function(fn, ...) stop(paste0(fn, "(): ", ...), call. = FALSE)

.mhp_warn <- function(fn, ...) warning(paste0(fn, "(): ", ...), call. = FALSE)

# a number (or numbers) within [min, max]; whole numbers when integer = TRUE; length 1 when scalar = TRUE
.mhp_check_number <- function(x, name, fn, min = -Inf, max = Inf, integer = FALSE, scalar = TRUE) {
  ok <- is.numeric(x) && length(x) > 0L && !anyNA(x) && all(x >= min & x <= max)
  if (ok && integer)   ok <- all(abs(x - round(x)) < 1e-9)
  if (ok && scalar)    ok <- length(x) == 1L
  if (!ok) {
    rng <- if (is.finite(min) && is.finite(max)) sprintf(" in [%s, %s]", min, max) else
      if (is.finite(min)) sprintf(" >= %s", min) else if (is.finite(max)) sprintf(" <= %s", max) else ""
    .mhp_stop(fn, "`", name, "` must be ", if (scalar) "a single " else "", if (integer) "whole number" else
                    "number", if (scalar) "" else "s", rng, ".")
  }
  if (integer) as.integer(round(x)) else x
}

.mhp_check_flag <- function(x, name, fn) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) .mhp_stop(fn, "`", name, "` must be TRUE or FALSE.")
  x
}

# one of a set of choices; numeric codes (a legacy interface such as Method = 1:4) are accepted when `codes` is TRUE
.mhp_check_choice <- function(x, choices, name, fn, codes = FALSE) {
  if (codes && is.numeric(x) && length(x) == 1L && x %in% seq_along(choices)) return(choices[x])
  if (is.character(x) && length(x) == 1L && x %in% choices) return(x)
  .mhp_stop(fn, "`", name, "` must be one of ", paste0("\"", choices, "\"", collapse = ", "),
                if (codes) paste0(" (or its number 1 to ", length(choices), ")") else "", ".")
}

.mhp_check_class <- function(x, cls, name, fn) {
  if (!inherits(x, cls)) .mhp_stop(fn, "`", name, "` must be a result of this package (class ", cls, ").")
  x
}

.mhp_check_data <- function(data, fn) {
  if (!is.data.frame(data)) .mhp_stop(fn, "`data` must be a data frame (or data.table).")
  if (!nrow(data)) .mhp_stop(fn, "`data` has no rows.")
  data
}

# A column given as a bare name (y = Yield), a string (y = "Yield") or a vector of length nrow(data). Called as
#   y <- .mhp_column(data, substitute(y), "y", fn, parent.frame())
# and returns list(value = <the column>, name = <its name>).
.mhp_column <- function(data, expr, name, fn, env = parent.frame(2L)) {
  if (is.symbol(expr) && as.character(expr) %in% names(data)) {
    return(list(value = data[[as.character(expr)]], name = as.character(expr)))
  }
  if (is.character(expr) && length(expr) == 1L) {
    if (!expr %in% names(data)) .mhp_stop(fn, "`", name, "`: no column \"", expr, "\" in `data`.")
    return(list(value = data[[expr]], name = expr))
  }
  v <- tryCatch(eval(expr, envir = data, enclos = env), error = function(e) NULL)
  if (is.character(v) && length(v) == 1L && v %in% names(data)) return(list(value = data[[v]], name = v))
  if (is.null(v) || length(v) != nrow(data)) {
    .mhp_stop(fn, "`", name, "` must name a column of `data` (bare or quoted) or be a vector of length ",
                  nrow(data), ".")
  }
  list(value = v, name = paste(deparse(expr), collapse = ""))
}

.mhp_test_level <- function() as.integer(Sys.getenv("MHPFILTER_TEST_LEVEL", "1"))
