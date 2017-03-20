################################################################################

#' Predict method
#'
#' Predict method for class `big_sp`.
#'
#' @param object Object of class `big_sp`.
#' @inheritParams bigstatsr-package
#' @param ... Not used.
#'
#' @return A matrix of scores, with rows corresponding to `ind.row`
#' and columns corresponding to `lambda`.
#' @export
#' @import Matrix
#' @importFrom stats predict
#' @seealso [big_spLinReg], [big_spLogReg] and [big_spSVM].
#'
#' @example examples/example-predict.R
predict.big_sp <- function(object, X.,
                           ind.row = rows_along(X.),
                           covar.row = NULL,
                           block.size = 1000,
                           ...) {
  betas <- object$beta
  if (is.null(covar.row)) {
    scores <- big_prodMat(X., betas,
                          ind.row = ind.row,
                          block.size = block.size)
  } else {
    stopifnot(nrow(covar.row) == length(ind.row))
    ind.X <- cols_along(X.)
    scores <- big_prodMat(X., betas[ind.X, ],
                          ind.row = ind.row,
                          block.size = block.size)
    scores <- scores + covar.row %*% betas[-ind.X, ]
  }

  rownames(scores) <- ind.row
  sweep(scores, 2, object$intercept, '+')
}

################################################################################
