################################################################################

#' @title Transposition
#' @description This function implements a simple cache-oblivious
#' algorithm for the transposition of a "big.matrix".
#' @inheritParams bigstatsr-package
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init -dimnames
#' @return The new transposed big.matrix. Its dimension, type and dimnames
#' are automatically determined from the input `big.matrix`.
#' @export
#' @examples
#' X <- big.matrix(11, 23, shared = FALSE)
#' X[] <- rnorm(length(X))
#'
#' Xt <- big_transpose(X, shared = FALSE)
#'
#' print(identical(
#'   t(X[,]),
#'     Xt[,]
#' ))
big_transpose <- function(X, ...) {
  check_X(X)

  res <- big.matrix(ncol(X), nrow(X), type = typeof(X),
                    dimnames = dimnames(X)[2:1], ...)

  transpose3(res@address, X@address)

  res
}

################################################################################
