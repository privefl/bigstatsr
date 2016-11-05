################################################################################

#' @title Transposition of a "big.matrix".
#' @description This function implements a simple cache-oblivious
#' algorithm for the transposition of a "big.matrix".
#' @inheritParams bigstatsr-package
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init
#' @return The transposed big.matrix. Its dimension and type
#' are automatically determined from the input `big.matrix`.
#' @export
#' @example examples/example-transpose.R
big_transpose <- function(X, ...) {
  check_X(X)

  res <- big.matrix(ncol(X), nrow(X), type = typeof(X), ...)

  transpose3(res@address, X@address)

  return(res)
}

################################################################################
