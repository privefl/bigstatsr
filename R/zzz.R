.onLoad <- function(libname, pkgname) {
  options(bigstatsr.ncores.max = parallel::detectCores() / 2)
  options(bigstatsr.check.args = TRUE)
}

.onUnload <- function(libpath) {
  options(bigstatsr.ncores.max = NULL)
  options(bigstatsr.check.args = NULL)
}
