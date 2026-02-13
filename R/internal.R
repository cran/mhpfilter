# R/internal.R
# Internal helper functions (not exported)

# Internal: fast AR1 coefficient
.ar1 <- function(x) {
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  x1 <- x[-n]
  x2 <- x[-1]
  sum(x1 * x2) / sum(x1 * x1)
}
