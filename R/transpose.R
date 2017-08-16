################################################################################

#' Transposition
#'
#' This function implements a simple cache-oblivious algorithm for
#' the transposition of a "big.matrix".
#'
#' @inheritParams bigstatsr-package
#'
#' @return The new transposed `big.matrix` (or its descriptor). Its dimensions
#' and type are automatically determined from the input `big.matrix`.
#'
#' @export
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' Xt.desc <- big_transpose(X.desc, fun.createBM = tmpFBM())
#' identical(t(attach.BM(X.desc)[,]), attach.BM(Xt.desc)[,])
#'
big_transpose <- function(X, ...) {

  check_args()

  res <- FBM(ncol(X), nrow(X), typeof(X), init = NULL, ...)

  transpose3(res, X)

  # `if`(inherits(X, "BM.code"), as.BM.code(res, code = X@code), res)
  res
} # TODO: see how put a code on a FBM

################################################################################
