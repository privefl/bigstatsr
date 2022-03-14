################################################################################

#' Transpose an FBM
#'
#' This function implements a simple cache-oblivious algorithm for
#' the transposition of a Filebacked Big Matrix.
#'
#' @inheritParams bigstatsr-package
#' @inheritParams FBM
#'
#' @return The new transposed FBM. Dimensions and type are automatically
#' determined from the input FBM.
#'
#' @export
#'
#' @examples
#' X <- FBM(6, 5, init = rnorm(30))
#' X[]
#' Xt <- big_transpose(X)
#' identical(t(X[]), Xt[])
#'
big_transpose <- function(X, backingfile = tempfile(tmpdir = getOption("FBM.dir"))) {

  check_args(X = "assert_class(X, 'FBM')")

  res <- FBM(ncol(X), nrow(X), typeof(X), init = NULL,
             backingfile = backingfile, create_bk = TRUE)

  transpose3(res, X)

  `if`(inherits(X, "FBM.code256"), add_code256(res, code = X$code256), res)
}

################################################################################
