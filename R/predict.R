################################################################################

#' Predict method
#'
#' Predict method which is applicable to
#' [big_spLinReg], [big_spLogReg] and [big_spSVM].
#'
#' @param object Object of class `big_spReg`.
#' @inheritParams bigstatsr-package
#'
#' @return A vector of scores, corresponding to `ind.row`.
#' @export
#' @import Matrix
#' @importFrom stats predict
#'
#' @examples
predict.big_sp <- function(object, X, ind.row = seq_len(nrow(X)),
                           covar.row = NULL,
                           block.size = block.size,
                           ncores2 = 1) {
  check_X(X, ncores2 = ncores2)

  betas <- object$beta
  if (is.null(covar.row)) {
    scores <- big_prodMat(X, betas, ind.row = ind.row,
                          block.size = block.size,
                          ncores2 = ncores2)
  } else {
    stopifnot(nrow(covar.row) == length(ind.row))
    ind.X <- seq_len(ncol(X))
    scores <- big_prodMat(X, betas[ind.X, ], ind.row = ind.row,
                          block.size = block.size,
                          ncores2 = ncores2)
    scores <- scores + covar.row %*% betas[-ind.X, ]
  }

  rownames(scores) <- ind.row
  sweep(scores, 2, object$intercept, '+')
}

################################################################################
