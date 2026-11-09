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


## ----launch, eval = FALSE-----------------------------------------------------
# mhp_app(
#     launch.browser = TRUE
#   )


## ----shot-filter, echo = FALSE, out.width = "100%"----------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-filter.jpg" else "figures/app-filter.webp")


## ----shot-plot, echo = FALSE, out.width = "100%"------------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-plot.jpg" else "figures/app-plot.webp")


## ----shot-compare, echo = FALSE, out.width = "100%"---------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-compare.jpg" else "figures/app-compare.webp")


## ----shot-diagnostics, echo = FALSE, out.width = "100%"-----------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-diagnostics.jpg" else "figures/app-diagnostics.webp")


## ----code---------------------------------------------------------------------
settings <-
  list(
      source       = "example"
    , freq         = "4"
    , col          = "gdp"
    , start        = 1959
    , start_period = 1
    , logs         = TRUE
    , method       = "fmhp"
    , max_lambda   = 3000
    , k_linear     = 15
    , k_quadratic  = NA
    , schemes      = c("linear", "quadratic")
    , form         = "rows"
    )
cat(mhpfilter:::.mhp_app_code(input = settings))


## ----deploy, eval = FALSE-----------------------------------------------------
# # app.R on a Shiny server
# mhpfilter::mhp_app_object()

