################################################################################

#' Transposition
#'
#' This function implements a simple cache-oblivious algorithm for
#' the transposition of a Filebacked Big Matrix.
#'
#' @inheritParams bigstatsr-package
#' @inheritParams FBM
#'
#' @return The new transposed Filebacked Big Matrix (or its descriptor). Its dimensions
#' and type are automatically determined from the input Filebacked Big Matrix.
#'
#' @export
#'
#' @examples
#' X <- FBM(10, 5, init = rnorm(50))
#' X[]
#' Xt <- big_transpose(X)
#' identical(t(X[]), Xt[])
#'
#' X <- big_attachExtdata()
#' Xt <- big_transpose(X)
#' identical(t(X[]), Xt[])
#'
big_transpose <- function(X, backingfile = tempfile()) {

  check_args(X = "assert_class(X, 'FBM')")

  res <- FBM(ncol(X), nrow(X), typeof(X), init = NULL,
             backingfile = backingfile, create_bk = TRUE)

  transpose3(res, X)

  `if`(inherits(X, "FBM.code256"), add_code256(res, code = X$code256), res)
}

################################################################################
