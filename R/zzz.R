################################################################################

.onLoad <- function(libname, pkgname) {
  options(bigstatsr.ncores.max = parallel::detectCores())
  options(bigstatsr.check.args = TRUE)
  options(bigstatsr.block.sizeGB = 1)
  options(bigstatsr.typecast.warning = TRUE)
}

################################################################################

.onUnload <- function(libpath) {
  options(bigstatsr.ncores.max = NULL)
  options(bigstatsr.check.args = NULL)
  options(bigstatsr.block.sizeGB = NULL)
  options(bigstatsr.typecast.warning = NULL)
}

################################################################################
