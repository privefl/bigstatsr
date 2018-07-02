################################################################################

get_nline <- function(file) {
  scan(text = system(paste("wc -l", file), intern = TRUE),
       what = 1L, n = 1, quiet = TRUE)
}

################################################################################

#' Read a file
#'
#' Read a file as a Filebacked Big Matrix and a data frame of meta information.
#' For a mini-tutorial, please see [this vignette](https://goo.gl/91oNxU).
#'
#' @inheritParams bigstatsr-package
#' @param file The path to the file to be read.
#' @param sep The field separator character.
#' @param header A logical value indicating whether the file contains the names
#'   of the variables as its first line.
#' @param confirmed A logical value indicating whether you don't want to pause
#'   to confirm that everything has been parsed like you wanted.
#'   Default is `FALSE` and will ask you to confirm during execution.
#' @param verbose Be verbose? Default is `TRUE`.
#' @param ind.skip An integer vector specifying which columns to skip.
#' @param ind.meta An integer vector specifying which columns should be part of
#'   the meta information. Non-numeric columns will automatically be added to
#'   meta information because they can't be stored in an FBM.
#' @param nlines Number of lines of the file (including possible header).
#'   Default automatically gets this number thanks to a function of David Gohel.
#' @param nlines.block Number of lines to be read at once. Default automatically
#'   gets this number based on `getOption("bigstatsr.block.sizeGB")`.
#' @param fun.con Function to open a connection from a file.
#' @param type Type of the resulting Filebacked Big Matrix.
#'   Default is automatically guessing between `"integer"` and `"double"`.
#' @inheritDotParams FBM -nrow -ncol -type -init
#'
#' @return A list with 3 elements:
#' - A Filebacked Big Matrix.
#' - A vector of colnames.
#' - A data frame of meta information (e.g. non-numeric variables such as rownames)
#'
#' @export
#'
big_read <- function(file,
                     sep,
                     header,
                     confirmed = FALSE,
                     verbose = TRUE,
                     ind.skip = integer(0),
                     ind.meta = integer(0),
                     nlines = NULL,
                     nlines.block = NULL,
                     fun.con = function(f) file(f, open = "rt"),
                     type = NULL,
                     ...) {

  message3 <- function(...) if (verbose) message2(...)
  fread2 <- function(...) data.table::fread(..., data.table = FALSE)

  # Get #lines of the file
  if (is.null(nlines)) {
    nlines <- nlines_(file)
    # message3("%s lines detected.", nlines)
  }
  n <- nlines - header

  # Size of the blocks
  if (is.null(nlines.block)) {
    size.max <- getOption("bigstatsr.block.sizeGB") * 1024^3 / 2
    nlines.block <- min(max(1, floor(size.max / file.size(file) * n)), n)
    message3("Will use blocks of %s lines", nlines.block)
  }

  # Guess from data.table::fread()
  top_auto <- fread2(file, nrows = nlines.block, drop = ind.skip)
  # Verify sep & header
  top_verif <- fread2(file, nrows = 1, sep = sep, header = header)
  p.all <- ncol(top_verif)
  ind.keep <- setdiff(seq_len(p.all), ind.skip)
  if (!identical(top_auto[1, ], top_verif[, ind.keep, drop = FALSE]))
    stop2("There is a problem with either 'sep' or 'header'.")


  # Meta -> df  &  numeric -> FBM
  coltypes <- unname(sapply(top_auto, typeof))
  coltypes.num <- ALL.TYPES[coltypes]
  is.meta <- is.na(coltypes.num)
  ind.nonum <- ind.keep[is.meta]
  ind.meta.keep <- stats::na.omit(match(ind.meta, ind.keep))
  is.meta[ind.meta.keep] <- TRUE
  in.fbm <- !is.meta
  ind.meta.all <- ind.keep[is.meta]
  meta_diff <- setdiff(ind.meta.all, ind.meta)
  if (length(meta_diff) > 0) {
    message3(sprintf("Will add %s more columns to meta information.", length(meta_diff)))
  }

  p <- sum(in.fbm)
  colnames.fbm <- `if`(header, names(top_auto)[in.fbm], NULL)
  colnames.meta <- names(top_auto)[is.meta]
  stopifnot((p + length(ind.meta.all) + length(ind.skip)) == p.all)

  # Prepare the resulting Filebacked Big Matrix
  if (is.null(type)) {
    type <- `if`(any(coltypes[in.fbm] == "double"), "double", "integer")
  }
  message3("Will create a %s x %s FBM of type '%s'.", n, p, type)
  message3("Will create a %s x %s df of meta information.", n, length(ind.meta.all))

  if (!confirmed && interactive()) {
    readline(prompt = "Press [enter] to continue or [esc] to exit")
  }

  res <- FBM(nrow = n, ncol = p, type = type, init = NULL, ...)

  # Open connexion
  con <- fun.con(file)
  on.exit(close(con), add = TRUE)

  # Column types
  colclasses <- rep(list(NULL), p.all)
  for (i in seq_along(ind.keep)) colclasses[ind.keep[i]] <- coltypes[i]

  rm(top_auto, top_verif)

  meta_df <- big_apply(res, a.FUN = function(X, ind) {
    df_part <- utils::read.csv(
      con, sep = sep, header = header && (ind[1] == 1), nrows = length(ind),
      colClasses = colclasses, stringsAsFactors = FALSE)
    X[ind, ] <- as.matrix(df_part[in.fbm])
    stats::setNames(df_part[is.meta], colnames.meta)
  }, a.combine = "rbind", ind = seq_len(n), block.size = nlines.block)

  # Returns
  list(FBM = res, colnames = colnames.fbm, meta = meta_df)
}

################################################################################
