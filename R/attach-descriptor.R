################################################################################

#' Attach a "big.matrix.descriptor"
#'
#' @param descriptorfile Path to a ".desc" file.
#'
#' @return A [big.matrix.descriptor][big.matrix.descriptor-class] object.
#' @export
#' @rdname big_attach
#'
#' @examples
#' # tmpFBM
#' tmpfile <- tempfile()
#' X.desc <- FBM(backingroot = basename(tmpfile),
#'               backingpath = dirname(tmpfile))(10, 10)
#'
#' descriptorfile <- paste0(tmpfile, ".desc")
#' X.desc2 <- big_attach(descriptorfile)
#'
#' all.equal(X.desc, X.desc2)
big_attach <- function(descriptorfile) {
  describe(attach.big.matrix(descriptorfile))
}

#' @rdname big_attach
#' @export
#' @keywords internal
big_attachExtdata <- function() {
  desc <- system.file("extdata", "test_doc.desc", package = "bigstatsr")
  big_attach(desc)
}

################################################################################
