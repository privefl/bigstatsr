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
#' tmpfile <- tempfile()
#' test <- big.matrix(10, 10, backingfile = basename(tmpfile),
#'              backingpath = dirname(tmpfile),
#'              descriptorfile = paste0(basename(tmpfile), ".desc"))
#' X.desc <- describe(test)
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
