################################################################################

drop_ext <- function(file) tools::file_path_sans_ext(file)

################################################################################

#' Read a file
#'
#' Read a file as a Filebacked Big Matrix by using package {bigreadr}.
#' For a mini-tutorial, please see [this vignette](https://goo.gl/91oNxU).
#'
#' @param file File to read.
#' @param select Indices of columns to read (sorted).
#'   The length of `select` will be the number of columns of the resulting FBM.
#' @param filter Vector used to subset the rows of each data frame.
#' @param backingfile Path to the file storing the Big Matrix on disk.
#'   An extension ".bk" will be automatically added.
#'   Default uses `file` without its extension.
#' @inheritDotParams bigreadr::big_fread2 -file -select -.transform -.combine
#' @inheritParams FBM
#'
#' @return A Filebacked Big Matrix of type '<type>' with <length(select)> columns.
#'
#' @export
#'
big_read <- function(file, select, filter = NULL,
                     type = c("double", "float", "integer",
                              "unsigned short", "unsigned char", "raw"),
                     backingfile = drop_ext(file),
                     ...) {

  # Prepare reading
  X <- NULL
  offset <- 0
  fill_FBM <- function(df) {

    # Filter rows
    if (!is.null(filter)) df <- df[filter, , drop = FALSE]
    # Initialize FBM on first round
    if (offset == 0) {
      # Resulting FBM
      X <<- FBM(nrow(df), length(select), type = type, init = NULL,
                backingfile = backingfile, create_bk = TRUE)$save()
    }
    # Fill part of the FBM
    ind <- cols_along(df)
    X[, offset + ind] <- df
    offset <<- offset + length(ind)
    # Return colnames
    names(df)
  }

  # Read and fill by parts
  colnames <- bigreadr::big_fread2(
    file, select = select, .transform = fill_FBM, .combine = unlist, ... = ...)

  # Returns
  structure(X, fbm_names = colnames)
}

################################################################################

#' Write a file
#'
#' Write a file from a Filebacked Big Matrix (by parts).
#'
#' @inheritParams bigstatsr-package
#' @param file File to write to.
#' @param every_nrow Number of rows to write at once.
#' @param ... Other arguments to be passed to [data.table::fwrite],
#'   excepted `x`, `file`, `append`, `row.names`, `col.names` and `showProgress`.
#' @param progress Show progress? Default is `FALSE`.
#'
#' @return Input parameter `file`, invisibly.
#' @export
#'
#' @examples
#' X <- big_attachExtdata()
#' csv <- big_write(X, tempfile(), every_nrow = 100, progress = interactive())
big_write <- function(X, file, every_nrow,
                      ...,
                      ind.row = rows_along(X),
                      ind.col = cols_along(X),
                      progress = FALSE) {

  assert_noexist(file)

  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(ind.row), style = 3)
    on.exit(close(pb), add = TRUE)
  }

  already_written <- 0
  big_apply(X, a.FUN = function(X, ind) {

    part <- as.data.frame(X[ind, ind.col, drop = FALSE])
    bigreadr::fwrite2(part, file, ...,
                      append = (already_written != 0),
                      row.names = FALSE, col.names = FALSE,
                      showProgress = FALSE)

    already_written <<- already_written + length(ind)
    if (progress) utils::setTxtProgressBar(pb, already_written)

    NULL
  }, ind = ind.row, block.size = every_nrow)

  invisible(file)
}

################################################################################
