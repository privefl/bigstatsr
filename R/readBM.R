################################################################################

get_nline <- function(file) {
  scan(text = system(paste("wc -l", file), intern = TRUE),
       what = 1L, n = 1, quiet = TRUE)
}

################################################################################

#' Read a file
#'
#' Read a file as a `big.matrix`. For a mini-tutorial, please see the
#' [vignette](https://privefl.github.io/bigstatsr/articles/read-BM-from-file.html).
#'
#' @param file The path to the file to be read.
#' @param file.nheader Number of lines to read at the beginning of the file,
#' as a separate header information. Default is `0`. Each line is read as one
#' string. You may need to postprocess it with [strsplit]. See examples.
#' @param info.nelem Number of elements of extra information to read at the
#' beginning of each line. Default is `0`.
#' @param split The separator used in the file. Default is a space.
#' @param read.what What type of elements to scan? Default is `double()`.
#' You can also use `character()` or `integer()`.
#' @param read.transfo Function that transforms each line you read.
#' @param BM.type Type of the resulting `big.matrix`. Default uses the type
#' of `read.what`. This can be useful if you have only small integers or
#' that the `read.transfo` function transforms the type of what is read.
#' @param descriptor Return the descriptor of the new `big.matrix` or
#' directly the `big.matrix` object? Default is `TRUE` (the descriptor).
#' @param transpose Should the resulting `big.matrix` be transposed?
#' Default is `FALSE`.
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init -dimnames
#'
#' @return A `big.matrix` (or its descriptor) with two attributes `header` and
#' `info`.
#' @export
#' @importFrom magrittr %>%
#'
big_readBM <- function(file,
                       file.nheader = 0,
                       info.nelem = 0,
                       split = " ",
                       read.what = double(),
                       read.transfo = identity,
                       BM.type = typeof(read.what),
                       descriptor = TRUE,
                       transpose = FALSE,
                       ...) {

  # get #lines of the file
  file.nline <- get_nline(file)
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
    as(typeof(read.what)) %>%
    read.transfo()
  transfo.nelem <- length(firstline.transfo)

  # prepare the resulting big.matrix
  if (transpose) {
    res <- tmpFBM(n = transfo.nelem, m = read.nline, type = BM.type)
    res.path <- paste0(res@description[3:2], collapse = "")
    on.exit(unlink(res.path), add = TRUE)
    res <- attach.BM(res)
  } else {
    res <- big.matrix(nrow = transfo.nelem, ncol = read.nline, init = NULL, ...)
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

  # transpose?
  if (transpose) res <- big_transpose(res, descriptor = FALSE, ...)

  # returns
  structure(`if`(descriptor, describe(res), res),
            header = header,
            info = info)
}

################################################################################
