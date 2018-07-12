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
#' X <- FBM(10, 10, save = TRUE)
#'
#' rdsfile <- sub("\\.bk$", ".rds", X$backingfile)
#' X2 <- big_attach(rdsfile)
#'
#' all.equal(X[], X2[])
big_attach <- function(rdsfile) {

  rdsfile <- normalizePath(rdsfile)
  fbm <- readRDS(rdsfile)

  # In case of moving files
  if (!file.exists(fbm$backingfile <- sub("\\.rds$", ".bk", rdsfile)))
    stop2("Can't find the backingfile associated with this FBM.")

  fbm
}

#' @rdname big_attach
#' @export
#' @keywords internal
big_attachExtdata <- function() {
  tmp <- tempfile()
  EXTS <- c(".rds", ".bk")
  file.copy(system.file("extdata", paste0("example", EXTS),
                        package = "bigstatsr"),
            paste0(tmp, EXTS))
  big_attach(paste0(tmp, ".rds"))
}

################################################################################
