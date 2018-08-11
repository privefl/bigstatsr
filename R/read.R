################################################################################

drop_ext <- function(file) tools::file_path_sans_ext(file)

################################################################################

#' Read a file
#'
#' Read a file as a Filebacked Big Matrix by using package {bigreadr}.
#' For a mini-tutorial, please see [this vignette](https://goo.gl/91oNxU).
#'
#' @param select Indices of columns to read (sorted).
#'   The length of `select` will be the number of columns of the resulting FBM.
#' @param filter Vector used to subset the rows of each data frame.
#' @param backingfile Path to the file storing the Big Matrix on disk.
#'   An extension ".bk" will be automatically added.
#'   Default uses `file` without its extension.
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
big_read <- function(file, select,
                     nb_parts = NULL,
                     filter = NULL,
                     type = c("double", "integer", "unsigned short",
                              "unsigned char", "raw"),
                     backingfile = drop_ext(file),
                     skip = 0,
                     progress = TRUE,
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
    file, nb_parts, skip = skip, select = select, progress = progress,
    .transform = fill_FBM, .combine = unlist, ... = ...)

  # Returns
  structure(X, fbm_names = colnames)
}

################################################################################
