################################################################################

#' Attach a Filebacked Big Matrix
#'
#' @param rdsfile Path to a ".rds" file.
#'
#' @return The [FBM] object stored in the rdsfile.
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
big_attach <- function(rdsfile) {

  rdsfile <- normalizePath(rdsfile)
  fbm <- readRDS(rdsfile)

  if (!file.exists(fbm$backingfile <- sub("\\.rds$", ".bk", rdsfile)))
    stop2("Can't find the backingfile associated with this FBM.")

  fbm
}

#' @rdname big_attach
#' @export
#' @keywords internal
big_attachExtdata <- function() {
  big_attach(system.file("extdata", "example.rds", package = "bigstatsr"))
}

################################################################################
