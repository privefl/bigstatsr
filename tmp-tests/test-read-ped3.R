get_nline <- function(file) {
  scan(text = system(paste("wc -l", file), intern = TRUE),
       what = 1L, n = 1, quiet = TRUE)
}

get_nelem <- function(file, split, nheader = 0) {
  con <- file(file, open = "rt")
  on.exit(close(con), add = TRUE)

  # skip header
  for (i in seq_len(nheader)) readLines(con, n = 1)

  length(strsplit(readLines(con, n = 1), split = split, fixed = TRUE)[[1]])
}

big_readBM <- function(file,
                       nelem = NULL,
                       nheader = 0,
                       ninfo = 0,
                       split = " ",
                       read.type = c("character", "double", "integer"),
                       read.transfo = identity) {

  # get the number of elements in a line
  if (is.null(nelem)) nelem <- get_nelem(file, split, nheader)
  # get the number of elements of a line to read

  # open connexion
  con <- file(file, open = "rt")
  on.exit(close(con), add = TRUE)

  # get header
  header <- readLines(con, n = nheader)

  # get first line as chracters
  tmp <- strsplit(readLines(con, n = 1), split = split, fixed = TRUE)[[1]]
  nelem <- length(tmp)
  nread <- nelem - ninfo

  ### use file.nelem and read.nelem



}
