################################################################################

#' @title Transposition
#' @description This function implements a simple cache-oblivious
#' algorithm for the transposition of a "big.matrix".
#' @inheritParams bigstatsr-package
#' @param descriptor Return the descriptor of the transposed `big.matrix` or
#' directly the `big.matrix` object? Default is `TRUE` (the descriptor).
#' @inheritDotParams bigmemory::big.matrix -nrow -ncol -type -init -dimnames
#' @return The new transposed `big.matrix` (or its descriptor). Its dimension,
#' type and dimnames are automatically determined from the input `big.matrix`.
#' @export
#' @examples
#' X.desc <- big_attachExtdata()
#'
#' tmpfile <- tempfile()
#' Xt.desc <- big_transpose(X.desc, descriptor = TRUE,
#'                          backingfile = basename(tmpfile),
#'                          backingpath = dirname(tmpfile),
#'                          descriptorfile = paste0(basename(tmpfile), ".desc"))
#'
#' identical(t(attach.BM(X.desc)[,]), attach.BM(Xt.desc)[,])
big_transpose <- function(X., descriptor = TRUE, ...) {
  X <- attach.BM(X.)

  res <- big.matrix(ncol(X), nrow(X), type = typeof(X),
                    dimnames = dimnames(X)[2:1], ...)

  transpose3(res@address, X@address)

  if (inherits(X, "BM.code")) res <- as.BM.code(res, code = X@code)

  `if`(descriptor, describe(res), res)
}

################################################################################
