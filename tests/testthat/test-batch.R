data(us_macro, package = "mhpfilter")
X <- log(as.matrix(us_macro[, c("gdp", "consumption", "investment")]))

test_that("batch filters agree with the single-series filters and accept series of different lengths", {
  mb <- mhp_batch(X, max_lambda = 3000)
  expect_identical(names(mb), c("series", "t", "original", "trend", "cycle"))
  lam <- attr(mb, "lambdas")
  expect_identical(lam$lambda[1L], attr(mhp_filter(X[, 1L], max_lambda = 3000), "lambda"))
  Y <- X
  Y[1:10, 2L] <- NA
  Y[200:203, 3L] <- NA
  mb2 <- mhp_batch(Y, max_lambda = 3000)
  expect_identical(mb2[series == "consumption", range(t)], c(11L, 203L))
  expect_identical(attr(mb2, "lambdas")$lambda[2L],
                   attr(mhp_filter(Y[11:203, 2L], max_lambda = 3000), "lambda"))
  Y[50, 1L] <- NA
  expect_error(mhp_batch(Y), "inside")
  fb <- fmhp_batch(exp(X[, 1:2]))
  expect_identical(attr(fb, "lambdas")$lambda[1L], fmhp_filter(exp(X[, 1L]), as_dt = FALSE)$lambda)
  expect_s3_class(plot_batch(mb, highlight = "gdp"), "ggplot")
  expect_s3_class(plot_batch(fb, show = "trend", facet = FALSE), "ggplot")
  expect_error(plot_batch(X), "mhp_batch")
})

test_that("comparisons", {
  cm <- mhp_compare(X[, 1L], frequency = "annual", max_lambda = 3000)
  expect_identical(cm$lambda[1L], 100)
  expect_identical(names(cm), c("method", "lambda", "cycle_sd", "cycle_mean", "ar1", "cycle_range", "gcv"))
  bc <- batch_compare(X, frequency = "monthly", max_lambda = 3000)
  expect_identical(bc$hp_lambda, rep(129600, 3))
  fc <- fmhp_compare(exp(X[, 1L]))
  expect_identical(fc$method, c("HP", "Modified HP", "Fully modified HP"))
  expect_lt(fc$loss[3L], fc$loss[1L])
})
