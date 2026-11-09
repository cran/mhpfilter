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


## ----dense--------------------------------------------------------------------
x <- log(us_macro$gdp)
T <- length(x)
K <- diff(diag(T), differences = 2)
A <- crossprod(K)
fit <-
  hp_filter(
      x         = x
    , lambda    = 1600
    , as_dt     = FALSE
    , log_scale = FALSE
    )
max(abs(solve(a = diag(T) + 1600 * A, b = x) - fit$trend))


## ----dense-weights------------------------------------------------------------
for (form in c("rows", "differences")) {
  f <-
    fmhp_filter(
        x               = x
      , max_lambda      = 3000L
      , k_linear        = 15L
      , k_quadratic     = NULL
      , alpha_linear    = seq(from = 0, to = 2000, by = 10)
      , alpha_quadratic = seq(from = 0, to = 2000, by = 5)
      , schemes         = c("linear", "quadratic")
      , form            = form
      , lambda          = NULL
      , log_scale       = FALSE
      , as_dt           = FALSE
      )
  M <- if (form == "rows") diag(T) + diag(f$lambda_t) %*% A else diag(T) + t(K) %*% diag(f$lambda_t) %*% K
  cat(form, ":", max(abs(solve(a = M, b = x) - f$trend)), "\n")
}


## ----dense-gcv----------------------------------------------------------------
lam <- 683
B <- solve(diag(T) + lam * A)
gcv_dense <- mean((x - B %*% x)^2) / (1 - sum(diag(B)) / T)^2
ex <- suppressWarnings(
  mhp_filter(
      x          = x
    , max_lambda = 2000
    , as_dt      = FALSE
    , log_scale  = FALSE
    , criterion  = "exact"
    )
  )
c(dense = gcv_dense, package = ex$gcv_path[lam])


## ----published----------------------------------------------------------------
ref <-
  mhp_reference(
      source   = NULL
    , evaluate = TRUE
    , level    = 1
    )
ref[, .(id, value, computed = round(x = computed, digits = 3), agrees)]


## ----bench--------------------------------------------------------------------
b <- data.table::fread(system.file("extdata", "bench.csv", package = "mhpfilter"))
w <- data.table::dcast(data = b, formula = case ~ version, value.var = "median_ms")
v <- sort(unique(b$version))
w[, speedup := round(get(v[1]) / get(v[2]))]
w

