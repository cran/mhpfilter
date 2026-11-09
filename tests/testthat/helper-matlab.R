# A plain-R port of fmhpfilter_Hanif_et_al_2017.m (MATLAB File Exchange 63198; Copyright (c) 2017, Muhammad Nadim
# Hanif; BSD 2-clause), kept as the reference of the compiled implementation. The loops that build matrices are
# vectorised; the grids, the end schedules, the loss, the criterion and the choices (the last minimum, as in the
# MATLAB loops) are those of the original. `maxlam` is an argument only to keep the tests short (3000 in MATLAB).
mround <- function(x) floor(x + 0.5)                   # MATLAB's round: halves away from zero (x > 0 here)

fmhp_matlab <- function(y, maxlam = 3000) {
  z <- log(y)
  t <- length(z)
  lam <- seq_len(maxlam)
  k <- matrix(0, t - 2, t)
  for (i in seq_len(t - 2)) k[i, i:(i + 2)] <- c(1, -2, 1)
  id <- diag(t)
  aa <- crossprod(k)
  last_min <- function(v) max(which(v == min(v)))
  crit <- function(dd) sum((z - dd %*% z)^2)
  # modified lambda
  sss <- vapply(lam, function(m) crit(solve(id + m * aa)) / t * (1 + 2 * t / m), 0)
  lamo <- lam[last_min(sss)]
  sched <- function(base, a, st, p) {
    v <- rep(base, t)
    it <- seq_len(st)
    v[it] <- a * (st - it + 1)^p + base
    v[t - st + it] <- a * it^p + base
    v
  }
  smoother <- function(v) solve(id + v * aa)          # row jd of aa scaled by v[jd]
  thita <- seq(0, 3.1, by = 0.1)
  d <- outer(seq_len(t), seq_len(t), function(i, j) j - i)
  loss <- function(dd) {
    g <- vapply(thita, function(w) sqrt(rowSums(dd * cos(w * d))^2 + rowSums(dd * sin(w * d))^2), numeric(t))
    c0 <- mround(t / 2)
    sum(0.1 * (g - matrix(g[c0, ], t, length(thita), byrow = TRUE))^2)
  }
  one_scheme <- function(alphas, st, p) {
    res <- vapply(alphas, function(a) loss(smoother(sched(lamo, a, st, p))), 0)
    ps <- last_min(res)
    s2 <- vapply(lam, function(m) crit(smoother(sched(m, alphas[ps], st, p))) / t * (1 + 2 * t / m), 0)
    lf <- lam[last_min(s2)]
    list(minloss = min(res), alpha = alphas[ps], lambda = lf, trend = drop(smoother(sched(lf, alphas[ps], st, p)) %*% z))
  }
  s1 <- one_scheme(seq(0, 2000, 10), 15, 1)
  s2 <- one_scheme(seq(0, 2000, 5), mround(min(22, t / 2 - 5)), 2)
  best <- if (s1$minloss < s2$minloss) s1 else s2
  list(trend = exp(best$trend), cycle = y - exp(best$trend), lambda_mhp = lamo, linear = s1, quadratic = s2)
}

# A plain-R port of modifiedhpfilternew.m, the MATLAB code of the modified HP filter of Choudhary, Hanif and Iqbal
# (2014): the criterion of their eq. 6 without the factor 1/T, over lambda = 1, ..., maxlam (100000 in the code),
# the last minimum, dense inverses, and no logarithms.
mhp_matlab <- function(y, maxlam = 100000) {
  x <- y
  t <- length(x)
  k <- matrix(0, t - 2, t)
  for (i in seq_len(t - 2)) k[i, i:(i + 2)] <- c(1, -2, 1)
  a <- crossprod(k)
  id <- diag(t)
  sss <- vapply(seq_len(maxlam), function(m) sum((x - solve(id + m * a) %*% x)^2) * (1 + 2 * t / m), 0)
  lambda <- max(which(sss == min(sss)))
  tc <- drop(solve(id + lambda * a) %*% x)
  list(lambda = lambda, trend = tc, cycle = x - tc)
}
