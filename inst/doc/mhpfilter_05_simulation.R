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


## ----read---------------------------------------------------------------------
rmse  <- data.table::fread(system.file("extdata", "simulation_rmse.csv", package = "mhpfilter"))
power <- data.table::fread(system.file("extdata", "simulation_power.csv", package = "mhpfilter"))
n_rep <- 20


## ----designs------------------------------------------------------------------
mhp_designs(
    paper = "2017"
  )


## ----one, fig.height = 4------------------------------------------------------
s <-
  mhp_simulate(
      T         = 200
    , sd_ratio  = 1
    , ar        = c(1.2, -0.4)
    , sd_cycle  = 1
    , drift     = 0.5
    , trend     = "linear"
    , aggregate = "none"
    , seed      = 1
    )
s


## ----small--------------------------------------------------------------------
sim <-
  fmhp_simulation(
      designs    = mhp_designs(
          paper = c("2017", "2014")
        )[c(3, 13, 23)]
    , n_rep      = 5
    , T          = 200
    , trend      = "linear"
    , aggregate  = "none"
    , lambda     = NULL
    , max_lambda = 3000
    , seed       = 1
    , form       = "rows"
    )
sim$power


## ----share--------------------------------------------------------------------
power[, .(
    fmhp_better_hp  = round(x = mean(fmhp_better_hp), digits = 2)
  , fmhp_better_mhp = round(x = mean(fmhp_better_mhp), digits = 2)
  , designs_won_hp  = sum(fmhp_better_hp > 0.5)
  ), by = .(aggregate, trend, form)]


## ----ratio--------------------------------------------------------------------
power[aggregate == "none" & trend == "linear" & form == "rows",
      .(
          fmhp_better_hp  = round(x = mean(fmhp_better_hp), digits = 2)
        , fmhp_better_mhp = round(x = mean(fmhp_better_mhp), digits = 2)
        ),
      by = sd_ratio]


## ----rmse---------------------------------------------------------------------
rmse[aggregate == "none" & trend == "linear" & form == "rows",
     .(
         rmse   = round(x = mean(rmse), digits = 3)
       , middle = round(x = mean(rmse_middle), digits = 3)
       , ends   = round(x = mean(rmse_ends), digits = 3)
       ),
     by = method]


## ----rmse-plot, fig.height = 5------------------------------------------------
d <- rmse[form == "rows", .(rmse_ends = mean(rmse_ends)), by = .(aggregate, trend, method, sd_ratio)]
ggplot2::ggplot(
    data    = d
  , mapping = ggplot2::aes(x = factor(sd_ratio), y = rmse_ends, colour = method, group = method)
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_grid(
      rows   = ggplot2::vars(aggregate)
    , cols   = ggplot2::vars(trend)
    , scales = "free_y"
    ) +
  ggplot2::labs(x = "ratio of the standard deviations of the trend and cycle shocks",
                y = "RMSE of the cycle at the ends", colour = NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")


## ----summary, include = FALSE-------------------------------------------------
q <- rmse[aggregate == "none" & trend == "linear" & form == "rows", .(
    ends = mean(rmse_ends)
  , mid  = mean(rmse_middle)
  ),
          by = method]
share_q <- round(100 * power[aggregate == "none" & trend == "linear" & form == "rows", mean(fmhp_better_hp)])
share_a <- round(100 * power[aggregate == "average" & trend == "linear" & form == "rows", mean(fmhp_better_hp)])
won     <- power[, sum(fmhp_better_hp > 0.5)]


## ----rerun, eval = FALSE------------------------------------------------------
# # from the package folder; 100 replicates take about three hours
# system2(command = "Rscript", args = c("tools/simulation-study.R", "100"))

