################################################################################

.onLoad <- function(libname, pkgname) {
  options(
    bigstatsr.cluster.type = "PSOCK",
    bigstatsr.check.args = TRUE,
    bigstatsr.block.sizeGB = 1,
    bigstatsr.downcast.warning = TRUE
  )
}

################################################################################

.onUnload <- function(libpath) {
  options(
    bigstatsr.cluster.type = NULL,
    bigstatsr.check.args = NULL,
    bigstatsr.block.sizeGB = NULL,
    bigstatsr.downcast.warning = NULL
  )
}

################################################################################
