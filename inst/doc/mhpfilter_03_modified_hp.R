## ----setup, include = FALSE---------------------------------------------------
# HTML figures are drawn as PNG and kept as WebP: lossless, or lossy at quality 90 when that is less than half the
# size. The PDF figures stay vector graphics. Any failure of magick (not installed, no WebP, or a broken
# installation whose system libraries are missing) keeps the PNG
webp <- function(path) {
  if (knitr::is_latex_output() || !grepl(pattern = "[.]png$", x = path)) return(path)
  tryCatch(
      {
        if (!requireNamespace("magick", quietly = TRUE) || !isTRUE(magick::magick_config()$webp)) return(path)
        img <- magick::image_read(path)
        out <- sub(pattern = "[.]png$", replacement = ".webp", x = path)
        tmp <- tempfile(fileext = ".webp")
        magick::image_write(
            image   = img
          , path    = out
          , format  = "webp"
          , quality = 100
          , defines = c(`webp:lossless` = "true")
          )
        magick::image_write(image = img, path = tmp, format = "webp", quality = 90)
        if (file.size(tmp) < file.size(out) / 2) file.copy(from = tmp, to = out, overwrite = TRUE)
        unlink(c(path, tmp))
        out
      }
    , error = function(e) path
    )
}
knitr::opts_chunk$set(collapse = FALSE, comment = "", fig.width = 7, fig.height = 4.2,
                      dev = if (knitr::is_latex_output()) "cairo_pdf" else "ragg_png", dpi = 96,
                      fig.process = webp)
if (requireNamespace("mhpfilter", quietly = TRUE)) library(mhpfilter) else
  pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)


## ----gdp----------------------------------------------------------------------
data(us_macro)
fit <-
  mhp_filter(
      x          = us_macro$gdp
    , max_lambda = 100000
    , as_dt      = FALSE
    , log_scale  = TRUE
    , criterion  = "approximate"
    )
fit


## ----gdp-gcv------------------------------------------------------------------
ggplot2::autoplot(
    object = fit
  , which  = "gcv"
  )


## ----gdp-decomp, fig.height = 5-----------------------------------------------
ggplot2::autoplot(
    object = fit
  , which  = "decomposition"
  )


## ----batch--------------------------------------------------------------------
X <- log(as.matrix(us_macro[, c("gdp", "consumption", "investment")]))
b <-
  mhp_batch(
      X          = X
    , max_lambda = 100000
    )
lam <- attr(x = b, which = "lambdas")
lam[, paper := c(941, 841, 488)]
lam


## ----batch-plot, fig.height = 5-----------------------------------------------
plot_batch(
    x         = b
  , show      = "cycle"
  , facet     = TRUE
  , highlight = NULL
  )


## ----compare------------------------------------------------------------------
mhp_compare(
    x          = log(us_macro$gdp)
  , frequency  = "quarterly"
  , max_lambda = 100000
  )


## ----compare-plot, fig.height = 5---------------------------------------------
plot_comparison(
    x          = log(us_macro$gdp)
  , frequency  = "quarterly"
  , max_lambda = 100000
  , show_cycle = TRUE
  , fmhp       = FALSE
  )


## ----stability----------------------------------------------------------------
ends <- seq(from = 120, to = nrow(us_macro), by = 8)
data.table::data.table(
    T      = ends
  , lambda = vapply(
        X         = ends
      , FUN       = function(n) {
          get_lambda(
              x = mhp_filter(
                  x          = us_macro$gdp[seq_len(n)]
                , max_lambda = 100000
                , as_dt      = FALSE
                , log_scale  = TRUE
                , criterion  = "approximate"
                )
            )
        }
      , FUN.VALUE = numeric(length = 1)
      )
  )


## ----designs------------------------------------------------------------------
mhp_designs(
    paper = "2014"
  )

