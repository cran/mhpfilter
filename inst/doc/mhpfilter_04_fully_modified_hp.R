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


## ----reference----------------------------------------------------------------
ref <-
  mhp_reference(
      source   = "Hanif"
    , evaluate = TRUE
    , level    = 1
    )
ref[, .(id, value, computed = round(x = computed, digits = 3), agrees)]
ref[agrees == FALSE, note]


## ----weights------------------------------------------------------------------
w_hp <-
  hp_weights(
      x      = 50
    , lambda = 100
    , form   = "rows"
    , rows   = NULL
    )
w_q <-
  hp_weights(
      x      = 50
    , lambda = hp_schedule(
          T      = 50
        , lambda = 100
        , alpha  = 10
        , k      = 20
        , power  = 2
        , form   = "rows"
        )
    , form   = "rows"
    , rows   = NULL
    )
c(hp = diff(range(w_hp$weights[1, ])), quadratic = diff(range(w_q$weights[1, ])))


## ----weights-plot, fig.height = 5---------------------------------------------
plot(x = w_hp)
plot(x = w_q)


## ----gain, fig.height = 5-----------------------------------------------------
plot(
  x = hp_gain(
      x      = 50
    , lambda = 100
    , form   = "rows"
    , theta  = seq(from = 0, to = pi, length.out = 101)
    , rows   = NULL
    )
  )
plot(
  x = hp_gain(
      x      = 50
    , lambda = w_q$lambda_t
    , form   = "rows"
    , theta  = seq(from = 0, to = pi, length.out = 101)
    , rows   = NULL
    )
  )


## ----loss---------------------------------------------------------------------
l_hp <-
  hp_loss(
      x      = 55
    , lambda = 100
    , form   = "rows"
    , theta  = seq(from = 0, to = 3.1, by = 0.1)
    , weight = 0.1
    )
l_q <-
  hp_loss(
      x      = 55
    , lambda = hp_schedule(
          T      = 55
        , lambda = 100
        , alpha  = 10
        , k      = 20
        , power  = 2
        , form   = "rows"
        )
    , form   = "rows"
    , theta  = seq(from = 0, to = 3.1, by = 0.1)
    , weight = 0.1
    )
c(hp = l_hp$total, quadratic = l_q$total)


## ----loss-plot----------------------------------------------------------------
plot(x = l_hp)
plot(x = l_q)


## ----fmhp---------------------------------------------------------------------
fit <-
  fmhp_filter(
      x               = us_macro$gdp
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
fit


## ----fmhp-final-loss----------------------------------------------------------
hp_loss(
    x      = length(us_macro$gdp)
  , lambda = fit$lambda_t
  , form   = "rows"
  , theta  = seq(from = 0, to = 3.1, by = 0.1)
  , weight = 0.1
  )$total


## ----fmhp-loss----------------------------------------------------------------
ggplot2::autoplot(
    object = fit
  , which  = "loss"
  )


## ----fmhp-batch---------------------------------------------------------------
fb <-
  fmhp_batch(
      X               = as.matrix(us_macro[, c("gdp", "consumption", "investment")])
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

