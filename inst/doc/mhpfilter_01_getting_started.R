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


## ----data---------------------------------------------------------------------
data(us_macro)
head(us_macro)
gdp <- ts(data = us_macro$gdp, start = c(1959, 1), frequency = 4)


## ----hp-----------------------------------------------------------------------
hp <-
  hp_filter(
      x         = log(gdp)
    , lambda    = 1600
    , as_dt     = TRUE
    , log_scale = FALSE
    )
hp


## ----mhp----------------------------------------------------------------------
mhp <-
  mhp_filter(
      x          = log(gdp)
    , max_lambda = 100000
    , as_dt      = FALSE
    , log_scale  = FALSE
    , criterion  = "approximate"
    )
mhp


## ----mhp-plot, fig.height = 5-------------------------------------------------
plot(x = mhp)


## ----mhp-gcv------------------------------------------------------------------
plot(x = mhp, which = "gcv")


## ----fmhp---------------------------------------------------------------------
fm <-
  fmhp_filter(
      x               = gdp
    , max_lambda      = 3000
    , k_linear        = 15
    , k_quadratic     = NULL
    , alpha_linear    = seq(from = 0, to = 2000, by = 10)
    , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
    , schemes         = c("linear", "quadratic")
    , form            = "rows"
    , lambda          = NULL
    , log_scale       = TRUE
    , as_dt           = FALSE
    )
fm
fm$schemes


## ----fmhp-lambda--------------------------------------------------------------
plot(x = fm, which = "lambda")


## ----compare------------------------------------------------------------------
cmp <-
  fmhp_compare(
      x          = gdp
    , frequency  = "quarterly"
    , max_lambda = 3000
    , log_scale  = TRUE
    , end_share  = 0.1
    )
cmp


## ----compare-plot, fig.height = 5.5-------------------------------------------
plot_comparison(
    x          = log(gdp)
  , frequency  = "quarterly"
  , max_lambda = 100000
  , show_cycle = TRUE
  , fmhp       = TRUE
  )


## ----batch--------------------------------------------------------------------
X <- as.matrix(us_macro[, c("gdp", "consumption", "investment")])
fb <-
  fmhp_batch(
      X               = X
    , max_lambda      = 3000
    , k_linear        = 15
    , k_quadratic     = NULL
    , alpha_linear    = seq(from = 0, to = 2000, by = 10)
    , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
    , schemes         = c("linear", "quadratic")
    , form            = "rows"
    , lambda          = NULL
    , log_scale       = TRUE
    )
attr(x = fb, which = "lambdas")


## ----batch-plot, fig.height = 4.5---------------------------------------------
plot_batch(
    x         = fb
  , show      = "cycle"
  , facet     = TRUE
  , highlight = NULL
  )


## ----interpret----------------------------------------------------------------
mhp_interpret(
    x   = fm
  , top = 3
  )

