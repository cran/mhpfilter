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


## ----criteria-----------------------------------------------------------------
data(us_macro)
x <- log(us_macro$gdp)
ap <-
  mhp_filter(
      x          = x
    , max_lambda = 20000
    , as_dt      = FALSE
    , log_scale  = FALSE
    , criterion  = "approximate"
    )
ex <- suppressWarnings(
  mhp_filter(
      x          = x
    , max_lambda = 20000
    , as_dt      = FALSE
    , log_scale  = FALSE
    , criterion  = "exact"
    )
  )
c(approximate = ap$lambda, exact = ex$lambda)


## ----criteria-plot, fig.height = 4--------------------------------------------
d <- data.table::data.table(
    lambda    = rep(seq_len(length.out = 20000), 2)
  , criterion = c(ap$gcv_path / min(ap$gcv_path), ex$gcv_path / min(ex$gcv_path))
  , type      = rep(c("approximate (eq. 6)", "exact GCV (eq. 5)"), each = 20000)
  )
ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = lambda, y = criterion, colour = type)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_log10(breaks = scales::breaks_log(n = 6), labels = scales::label_comma()) +
  ggplot2::labs(x = "lambda (log scale)", y = "criterion / its minimum", colour = NULL) +
  mhp_theme(
      base_size       = 11
    , base_family     = "serif"
    , ticks_inward    = TRUE
    , legend_position = "bottom"
    )


## ----weights------------------------------------------------------------------
w <-
  hp_weights(
      x      = 50
    , lambda = 100
    , form   = "rows"
    , rows   = NULL
    )
plot(x = w)


## ----gain---------------------------------------------------------------------
g <-
  hp_gain(
      x      = 50
    , lambda = 100
    , form   = "rows"
    , theta  = seq(from = 0, to = pi, length.out = 101)
    , rows   = NULL
    )
plot(x = g)


## ----loss---------------------------------------------------------------------
hl <-
  hp_loss(
      x      = 50
    , lambda = 100
    , form   = "rows"
    , theta  = seq(from = 0, to = 3.1, by = 0.1)
    , weight = 0.1
    )
plot(x = hl)


## ----schedule-----------------------------------------------------------------
hp_schedule(
    T      = 50
  , lambda = 100
  , alpha  = 10
  , k      = 20
  , power  = 1
  , form   = "rows"
  )


## ----forms--------------------------------------------------------------------
y <- us_macro$gdp
rows <-
  fmhp_filter(
      x               = y
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
diffs <-
  fmhp_filter(
      x               = y
    , max_lambda      = 3000
    , k_linear        = 15
    , k_quadratic     = NULL
    , alpha_linear    = seq(from = 0, to = 2000, by = 10)
    , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
    , schemes         = c("linear", "quadratic")
    , form            = "differences"
    , lambda          = NULL
    , log_scale       = TRUE
    , as_dt           = FALSE
    )
rbind(
    rows        = rows$schemes[scheme == rows$scheme]
  , differences = diffs$schemes[scheme == diffs$scheme]
  )

