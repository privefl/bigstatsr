################################################################################

#' @title Standard statistics for comulns of a big.matrix
#' @description Standard statistics for comulns of a big.matrix.
#' For now, the sum, mean, sd and var are implemented.
#' @inheritParams bigstatsr-package
#' @return Numeric vectors with the column statistics.
#' @example examples/example.colstats.R
#' @name colstats
NULL

################################################################################

#' @rdname colstats
#' @export
colsums <- function(X, ind.train = seq_len(nrow(X))) {
  check_X(X)
  bigcolsums(X@address, ind.train)
}

#' @rdname colstats
#' @export
colmeans <- function(X, ind.train = seq_len(nrow(X))) {
  check_X(X)
  colsums(X, ind.train) / length(ind.train)
}

#' @rdname colstats
#' @export
colvars <- function(X, ind.train = seq_len(nrow(X))) {
  check_X(X)
  bigcolvars(X@address, ind.train)$var
}

#' @rdname colstats
#' @export
colsds <- function(X, ind.train = seq_len(nrow(X))) {
  check_X(X)
  sqrt(colvars(X, ind.train))
}

#' @rdname colstats
#' @export
colmeans_sds <- function(X, ind.train = seq_len(nrow(X))) {
  check_X(X)
  tmp <- bigcolvars(X@address, ind.train)
  list(mean = tmp$sum / length(ind.train),
       sd = sqrt(tmp$var))
}

################################################################################
