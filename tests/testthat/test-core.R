# the compiled cores against dense R computations and the port of the published MATLAB code
ns <- asNamespace("mhpfilter")
Kd <- function(T) diff(diag(T), differences = 2L)

test_that("trends solve the HP systems of both forms", {
  set.seed(1)
  T <- 37
  x <- cumsum(rnorm(T))
  A <- crossprod(Kd(T))
  lam_t <- ns$.mhp_schedule(50, 7, 6L, 2, T, "rows")
  expect_equal(ns$.mhp_trend(x, 80), drop(solve(diag(T) + 80 * A, x)), tolerance = 1e-10)
  expect_equal(ns$.mhp_trend(x, lam_t, "rows"), drop(solve(diag(T) + lam_t * A, x)), tolerance = 1e-10)
  lam_r <- ns$.mhp_schedule(50, 7, 6L, 2, T, "differences")
  expect_length(lam_r, T - 2L)
  expect_equal(ns$.mhp_trend(x, lam_r, "differences"),
               drop(solve(diag(T) + crossprod(Kd(T), lam_r * Kd(T)), x)), tolerance = 1e-10)
  # both forms agree for a constant lambda; weights sum to one in every row
  expect_equal(ns$.mhp_trend(x, 80, "differences"), ns$.mhp_trend(x, 80, "rows"), tolerance = 1e-10)
  W <- hp_weights(x = T, lambda = lam_t, form = "rows")$weights
  expect_equal(W, solve(diag(T) + lam_t * A), tolerance = 1e-10)
  expect_equal(rowSums(W), rep(1, T), tolerance = 1e-10)
})

test_that("the criteria and the gains agree with direct computations", {
  set.seed(2)
  T <- 40
  x <- cumsum(rnorm(T, 0.3))
  A <- crossprod(Kd(T))
  crit <- function(l) { r <- x - solve(diag(T) + l * A, x); sum(r^2) / T * (1 + 2 * T / l) }
  g <- ns$.mhp_lambda(x, 400)
  expect_equal(g$path[c(1, 17, 400)], vapply(c(1, 17, 400), crit, 0), tolerance = 1e-9)
  expect_identical(g$lambda, which.min(vapply(1:400, crit, 0)))
  exact <- function(l) { B <- solve(diag(T) + l * A); r <- x - B %*% x; mean(r^2) / (1 - sum(diag(B)) / T)^2 }
  e <- ns$.mhp_lambda_exact(x, 50)
  expect_equal(e$path[c(1, 9, 50)], vapply(c(1, 9, 50), exact, 0), tolerance = 1e-8)
  B <- solve(diag(T) + 100 * A)
  th <- c(0, 0.7, 2.5)
  d <- outer(seq_len(T), seq_len(T), function(i, j) j - i)
  G <- vapply(th, function(w) sqrt(rowSums(B * cos(w * d))^2 + rowSums(B * sin(w * d))^2), numeric(T))
  gn <- hp_gain(x = T, lambda = 100, theta = th)$gain
  expect_equal(matrix(gn$gain, T), G, tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(G[, 1L], rep(1, T), tolerance = 1e-10)
})

test_that("the fully modified HP filter reproduces the published MATLAB code", {
  set.seed(3)
  n <- 48
  y <- exp(cumsum(c(4, rnorm(n - 1, 0.02, 0.01))) + stats::arima.sim(list(ar = 0.7), n, sd = 0.01))
  ref <- fmhp_matlab(y, maxlam = 300)
  new <- fmhp_filter(y, max_lambda = 300, as_dt = FALSE)
  expect_identical(new$lambda_mhp, ref$lambda_mhp)
  lin <- new$schemes[scheme == "linear"]
  qua <- new$schemes[scheme == "quadratic"]
  expect_equal(c(lin$loss, lin$alpha, lin$lambda), c(ref$linear$minloss, ref$linear$alpha, ref$linear$lambda),
               tolerance = 1e-10)
  expect_equal(c(qua$loss, qua$alpha, qua$lambda),
               c(ref$quadratic$minloss, ref$quadratic$alpha, ref$quadratic$lambda), tolerance = 1e-10)
  expect_equal(new$trend, as.numeric(ref$trend), tolerance = 1e-8)
  expect_equal(new$cycle, as.numeric(ref$cycle), tolerance = 1e-8)
})

test_that("the centre of the loss rounds halves up, as MATLAB does", {
  expect_identical(ns$.mhp_centre(41), 21L)
  expect_identical(ns$.mhp_centre(40), 20L)
  expect_identical(ns$.mhp_round(c(20.5, 21.5, 2.4)), c(21, 22, 2))
})

test_that("the modified HP filter reproduces the published MATLAB code of Choudhary et al. (2014)", {
  x <- log(mhpfilter::us_macro$gdp[1:80])
  ref <- mhp_matlab(x, maxlam = 2000)
  new <- mhp_filter(x, max_lambda = 2000, as_dt = FALSE)
  expect_identical(new$lambda, ref$lambda)
  expect_equal(new$trend, ref$trend, tolerance = 1e-9)
  expect_equal(new$cycle, ref$cycle, tolerance = 1e-9)
})
