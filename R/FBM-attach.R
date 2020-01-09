################################################################################

FBM_FIELDS <- c("extptr", "extptr_rw", "nrow", "ncol", "type", "backingfile",
                "is_read_only", "address", "address_rw", "bk", "rds", "is_saved",
                "type_chr", "type_size")

FBM_METHODS <- c("bm.desc", "bm", "add_columns", "save", "check_write_permissions")

################################################################################

reconstruct_if_old <- function(fbm,
                               msg1 = "FBM from an old version? Reconstructing..",
                               msg2 = "You should `$save()` it again.") {

  obj.fields  <- names(fbm$getClass()@fieldClasses)
  obj.methods <- names(fbm$getClass()@refMethods)

  # In case it was generated from old versions
  if (!all(FBM_FIELDS %in% obj.fields) || !all(FBM_METHODS %in% obj.methods)) {

    message2(msg1)
    new.fbm <- FBM(
      nrow = fbm$nrow,
      ncol = fbm$ncol,
      type = names(fbm$type),
      init = NULL,
      backingfile = sub_bk(fbm$backingfile),
      create_bk = FALSE,
      is_read_only = `if`(exists("is_read_only", fbm), fbm$is_read_only, FALSE)
    )

    if (inherits(fbm, "FBM.code256"))
      new.fbm <- add_code256(new.fbm, code = fbm$code256)

    message2(msg2)
    new.fbm

  } else fbm
}

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
#' X <- FBM(10, 10)$save()
#'
#' rdsfile <- sub_bk(X$backingfile, ".rds")
#' X2 <- big_attach(rdsfile)
#'
#' all.equal(X[], X2[])
big_attach <- function(rdsfile) {

  assert_exist(rdsfile)
  rdsfile <- normalizePath(rdsfile)
  fbm <- readRDS(rdsfile)

  # In case of moving files
  if (!file.exists(fbm$backingfile <- sub("\\.rds$", ".bk", rdsfile)))
    stop2("The backingfile associated with this FBM can't be found.")

  reconstruct_if_old(fbm)
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
