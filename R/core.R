#-------------------------------------------------------------------------------------------------------------------
# Internal cores of the filters: lambda by McDermott's criterion over an integer grid (MHP), the end-point loss of
# Bloechl (2014) and the fully modified HP filter of Hanif, Iqbal and Choudhary (2017). The heavy work is compiled
# (src/mhp_core.cpp); every trend is one O(T) pentadiagonal solve.
#-------------------------------------------------------------------------------------------------------------------

# MATLAB's round (halves away from zero), used where the published code rounds
.mhp_round <- function(x) floor(x + 0.5)

# the form of the end-point weights: 0 = on the second differences, 1 = on the rows (published code)
.mhp_form_code <- function(form) if (identical(form, "differences")) 0L else 1L

# the frequencies of the loss (0, 0.1, ..., 3.1) and its weight (their spacing), as in the published code
.mhp_theta <- function() seq(from = 0, to = 3.1, by = 0.1)

# the centre of a series of length T, the reference estimate of the loss
.mhp_centre <- function(T) as.integer(.mhp_round(T / 2))

# the smoothing parameters of a schedule (length T for the rows form, T - 2 for the differences form)
.mhp_schedule <- function(base, alpha, k, power, T, form) {
  drop(.mhp_schedule_cpp(base, alpha, as.integer(k), power, if (form == "differences") T - 2L else T))
}

# lambda minimising McDermott's criterion over 1, 2, ..., max_lambda (first minimum)
.mhp_lambda <- function(x, max_lambda, alpha = 0, k = 0L, power = 1, form = "rows") {
  grid <- seq_len(max_lambda)
  crit <- drop(.mhp_crit_cpp(x, as.numeric(grid), alpha, as.integer(k), power, .mhp_form_code(form)))
  i <- which.min(crit)
  list(lambda = grid[i], gcv = crit[i], path = crit)
}

# lambda minimising the exact GCV over 1, ..., max_lambda, from the eigen decomposition of A (first minimum)
.mhp_lambda_exact <- function(x, max_lambda) {
  T <- length(x)
  K <- diff(diag(T), differences = 2L)
  e <- eigen(crossprod(K), symmetric = TRUE)
  z2 <- drop(crossprod(e$vectors, x))^2
  grid <- seq_len(max_lambda)
  crit <- drop(.mhp_gcv_exact_cpp(pmax(e$values, 0), z2, as.numeric(grid)))
  i <- which.min(crit)
  list(lambda = grid[i], gcv = crit[i], path = crit)
}

# the trend of x for a lambda vector
.mhp_trend <- function(x, lam, form = "rows") {
  n <- if (form == "differences") length(x) - 2L else length(x)
  if (length(lam) == 1L) lam <- rep(lam, n)
  drop(.mhp_trend_cpp(matrix(x), as.numeric(lam), .mhp_form_code(form)))
}

# one end-point scheme of the FMHP filter (Hanif et al. 2017, steps 3 to 5)
.mhp_fmhp_scheme <- function(z, lambda_mhp, alphas, k, power, max_lambda, form, lambda_fixed) {
  T <- length(z)
  th <- .mhp_theta()
  loss <- drop(.mhp_scheme_loss_cpp(T, lambda_mhp, as.numeric(alphas), as.integer(k), power, th,
                                    .mhp_form_code(form), .mhp_centre(T), 0.1))
  ia <- max(which(loss == min(loss)))                     # the last minimum, as in the published loops
  alpha <- alphas[ia]
  lam <- if (is.null(lambda_fixed)) .mhp_lambda(z, max_lambda, alpha, k, power, form) else
    list(lambda = lambda_fixed, gcv = NA_real_, path = numeric())
  sched <- .mhp_schedule(lam$lambda, alpha, k, power, T, form)
  list(alpha = alpha, k = as.integer(k), power = power, loss = min(loss), loss_path = loss, lambda = lam$lambda,
       gcv = lam$gcv, gcv_path = lam$path, lambda_t = sched, trend = .mhp_trend(z, sched, form))
}

# the fully modified HP filter: endogenous lambda (MHP), then the linear and the quadratic end-point schemes with
# endogenous weights, then lambda again with the chosen weights; the scheme with the smaller loss is kept
.mhp_fmhp <- function(z, max_lambda, k_linear, k_quadratic, alpha_linear, alpha_quadratic, schemes, form,
                      lambda_fixed = NULL) {
  T <- length(z)
  mhp <- if (is.null(lambda_fixed)) .mhp_lambda(z, max_lambda) else list(lambda = lambda_fixed, gcv = NA_real_)
  k_quadratic <- if (is.null(k_quadratic)) as.integer(.mhp_round(min(22, T / 2 - 5))) else k_quadratic
  out <- list()
  if ("linear" %in% schemes)
    out$linear <- .mhp_fmhp_scheme(z, mhp$lambda, alpha_linear, k_linear, 1, max_lambda, form, lambda_fixed)
  if ("quadratic" %in% schemes)
    out$quadratic <- .mhp_fmhp_scheme(z, mhp$lambda, alpha_quadratic, k_quadratic, 2, max_lambda, form, lambda_fixed)
  losses <- vapply(out, `[[`, 0, "loss")
  # the linear scheme wins only when its loss is strictly smaller, as in the published code
  best <- if (length(out) == 2L && !(losses[["linear"]] < losses[["quadratic"]])) "quadratic" else names(out)[1L]
  list(lambda_mhp = mhp$lambda, gcv_mhp = mhp$gcv, schemes = out, scheme = best, fit = out[[best]])
}
