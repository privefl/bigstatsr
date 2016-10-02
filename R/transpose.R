#' @title Transposition of a "big.matrix".
#' @description This function implements a simple
#' cache-oblivious algorithm for the transposition of
#' a "big.matrix".
#' @param X	A big.matrix. You may have missing values in your data.
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init
#' @return The transposed big.matrix. Its dimension and type
#' are automatically determined from the input \code{big.matrix}.
#' @export
#' @example examples/example.transpose.R
transpose <- function(X, ...) {
  check_X(X)

  res <- big.matrix(ncol(X), nrow(X), type = typeof(X), ...)

  transpose3(res@address, X@address)

  return(res)
}
