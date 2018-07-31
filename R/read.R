################################################################################

drop_ext <- function(file) tools::file_path_sans_ext(file)

#' Read a file
#'
#' Read a file as a Filebacked Big Matrix by using package {bigreadr}.
#' For a mini-tutorial, please see [this vignette](https://goo.gl/91oNxU).
#'
#' @param select Indices of columns to read (sorted).
#'   The length of `select` will be the number of columns of the resulting FBM.
#' @param nrow Number of rows of the resulting FBM.
#'   This is basically (nlines - skip - header).
#' @param backingfile Path to the file storing the Big Matrix on disk.
#'   An extension ".bk" will be automatically added.
#'   Default uses `file` without its extension.
#' @param nb_parts Number of parts in which to split reading.
#'   Parts are referring to blocks of selected columns.
#'   Default use global option `bigstatsr.block.sizeGB` to set a good value.
#' @inheritParams bigreadr::big_fread2
#' @inheritParams FBM
#' @param progress Show progress? Default is `TRUE`.
#' @param ... More arguments to be passed to [data.table::fread].
#'
#' @return A Filebacked Big Matrix of type '<type>' with <nrow> rows
#'   and <length(select)> columns.
#'
#' @export
#'
big_read <- function(file, select, nrow,
                     type = c("double", "integer", "unsigned short",
                              "unsigned char", "raw"),
                     backingfile = drop_ext(file),
                     nb_parts = NULL,
                     skip = 0,
                     progress = TRUE,
                     ...) {

  assert_exist(file)
  assert_int(select); assert_pos(select)
  if (is.unsorted(select, strictly = TRUE))
    stop2("Argument 'select' should be sorted.")

  # Need package {bigreadr}
  if (!requireNamespace("bigreadr", quietly = TRUE) ||
      utils::packageVersion("bigreadr") < package_version("0.1.1"))
    stop2("Please install package {bigreadr} (>= 0.1.1).")

  # Number of parts
  if (is.null(nb_parts)) {
    nb_parts <- ceiling(
      file.size(file) / (getOption("bigstatsr.block.sizeGB") * 2^29))
    if (progress) message2("Will read the file in %d parts.", nb_parts)
  }

  # Resulting FBM
  X <- FBM(nrow = nrow, ncol = length(select), type = type, init = NULL,
           backingfile = backingfile, create_bk = TRUE, save = TRUE)

  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(select), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  # Read and fill by parts
  offset <- 0
  colnames <- bigreadr::big_fread2(
    file, nb_parts, skip = skip, select = select, .transform = function(df) {
      ind <- cols_along(df)
      X[, offset + ind] <- df
      offset <<- offset + length(ind)
      if (progress) utils::setTxtProgressBar(pb, offset)
      names(df)
    }, .combine = unlist, showProgress = FALSE)

  # Returns
  structure(X, fbm_names = colnames)
}

################################################################################
