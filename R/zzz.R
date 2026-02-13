# R/zzz.R - CORRECTED VERSION

# .onLoad is causing issues - let R handle DLL loading automatically
# The useDynLib in NAMESPACE will handle DLL loading

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "mhpfilter ",
    utils::packageVersion("mhpfilter"),
    " loaded"
  )
}

# Internal C++ wrapper functions
.mhp_core <- function(x, max_lambda) {
  .Call(`_mhpfilter_mhp_core`, x, max_lambda)
}

.hp_core <- function(x, lambda) {
  .Call(`_mhpfilter_hp_core`, x, lambda)
}

.mhp_batch_cpp <- function(X, max_lambda) {
  .Call(`_mhpfilter_mhp_batch`, X, max_lambda)
}
