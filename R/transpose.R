#' @title Transposition of a "big.matrix".
#' @description This function implements a simple
#' cache-oblivious algorithm for the transposition of
#' a "big.matrix".
#' @param X	A big.matrix. You may have missing values in your data.
#' @param ... Other parameters passed to
#' \code{\link[bigmemory]{big.matrix}}. Note that
#' \code{nrow}, \code{ncol} and \code{type} of the
#' resulting \code{big.matrix} are automatically defined.
#' @return The transposed big.matrix
#' @export
#' @example examples/example.transpose.R
transpose <- function(X, ...) {
  check_X(X)

  res <- big.matrix(ncol(X), nrow(X), type = typeof(X), ...)

  transpose3(res@address, X@address)

  return(res)
}
