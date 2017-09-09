################################################################################

get_nline <- function(file) {
  scan(text = system(paste("wc -l", file), intern = TRUE),
       what = 1L, n = 1, quiet = TRUE)
}

################################################################################

#' Read a file
#'
#' Read a file as a Filebacked Big Matrix.
#' For a mini-tutorial, please see [this vignette](https://goo.gl/91oNxU).
#'
#' @inheritParams bigstatsr-package
#' @param file The path to the file to be read.
#' @param file.nheader Number of lines to read at the beginning of the file,
#' as a separate header information. Default is `0`. Each line is read as one
#' string. You may need to post-process it with [strsplit]. See examples.
#' @param file.nline Number of total lines of the file. Default is `NULL` and a
#' function computes it. This function doesn't work for compressed files so that
#' you will have to explicitly specify the number of lines of the file.
#' @param info.nelem Number of elements of extra information to read at the
#' beginning of each line. Default is `0`.
#' @param split The separator used in the file. Default is a space.
#' @param read.what What type of elements to scan? Default is `double()`.
#' You can also use `character()` or `integer()`.
#' @param read.transfo Function that transforms each line you read.
#' @param BM.type Type of the resulting Filebacked Big Matrix. Default uses the
#' type of `read.what`. This can be useful if you have only small integers or
#' that the `read.transfo` function transforms the type of what is read.
#' @param transpose Should the resulting Filebacked Big Matrix be transposed?
#' Default is `FALSE`.
#' @inheritDotParams FBM -nrow -ncol -type -init
#'
#' @return A Filebacked Big Matrix with two attributes `header` and `info`.
#'
#' @export
#' @importFrom magrittr %>%
#'
big_read <- function(file,
                     file.nheader = 0,
                     file.nline = NULL,
                     info.nelem = 0,
                     split = " ",
                     read.what = double(),
                     read.transfo = identity,
                     BM.type = typeof(read.what),
                     transpose = FALSE,
                     ...) {

  # get #lines of the file
  if (is.null(file.nline)) file.nline <- get_nline(file)
  read.nline <- file.nline - file.nheader

  # prepare the matrix of info
  info <- matrix(NA_character_, nrow = info.nelem, ncol = read.nline)

  # open connexion
  con <- file(file, open = "rt")
  on.exit(close(con), add = TRUE)

  # get header
  header <- readLines(con, n = file.nheader)

  # get first line (after header)
  firstline <- strsplit(readLines(con, n = 1), split = split, fixed = TRUE)[[1]]
  # get #elements of each line
  file.nelem <- length(firstline)
  read.nelem <- file.nelem - info.nelem
  ## get info part
  info[, 1] <- head(firstline, info.nelem)
  ## get read transfo part
  firstline.transfo <- firstline %>%
    tail(read.nelem) %>%
    methods::as(typeof(read.what)) %>%
    read.transfo()
  transfo.nelem <- length(firstline.transfo)

  # prepare the resulting Filebacked Big Matrix
  if (transpose) {
    res <- FBM(nrow = transfo.nelem, ncol = read.nline,
               type = BM.type, init = NULL)
  } else {
    res <- FBM(nrow = transfo.nelem, ncol = read.nline,
               type = BM.type, init = NULL, ...)
  }

  # functions for scanning
  info.scan <- function(con) {
    scan(con, what = "", n = info.nelem, sep = split, quiet = TRUE)
  }
  read.scan <- function(con) {
    scan(con, what = read.what, n = read.nelem, sep = split, quiet = TRUE)
  }

  # main read
  res[, 1] <- firstline.transfo
  for (k in 2:read.nline) {
    if (info.nelem) info[, k] <- info.scan(con)
    res[, k] <- read.transfo(read.scan(con))
  }

  # check EOF (if can't read another character)
  if (length(scan(con, what = "", n = 1, sep = split, quiet = TRUE)))
    warning2("Didn't reach EOF.")

  # returns
  structure(`if`(transpose, big_transpose(res, ...), res),
            header = header,
            info = info)
}

################################################################################
