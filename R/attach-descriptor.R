################################################################################

#' Attach a "big.matrix.descriptor"
#'
#' @param descriptorfile
#'
#' @return A [big.matrix.descriptor][big.matrix.descriptor-class] object.
#' @export
#'
#' @examples
#' tmpfile <- tempfile()
#' test <- big.matrix(10, 10, backingfile = basename(tmpfile),
#'                    backingpath = dirname(tmpfile),
#'                    descriptorfile = paste0(basename(tmpfile), ".desc"))
#' X.desc <- describe(test)
#' X.desc2 <- big_attach(paste0(tmpfile, ".desc"))
#' all.equal(X.desc, X.desc2)
big_attach <- function(descriptorfile) {
  describe(attach.big.matrix(descriptorfile))
}

################################################################################
