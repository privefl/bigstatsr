################################################################################

check_sparseSVM <- function() {
  if (utils::packageVersion("sparseSVM") != "1.1-5.6670")
    stop(paste0("Please use my fork for now ",
                "(until I merge it with Yaohui Zeng's repo).\n",
                "You can get it via ",
                "'devtools::install_github(\"privefl/sparseSVM\")'."))
}

################################################################################

#' Sparse SVM
#'
#' Fit solution paths for sparse linear SVM regularized by lasso or elastic-net
#' over a grid of values for the regularization parameter lambda.
#' This is a wrapper of a modified version of
#' [sparseSVM][sparseSVM::sparseSVM].
#'
#' @inheritParams bigstatsr-package
#' @inheritDotParams sparseSVM::sparseSVM
#' alpha gamma screen nlambda lambda.min dfmax message
#'
#' @inherit sparseSVM::sparseSVM return
#'
#' @example
#'
#' @seealso [LiblineaR][LiblineaR::LiblineaR] [sparseSVM][sparseSVM::sparseSVM]
#'
#' @export
big_spSVM <- function(X, y01.train, ind.train = seq(nrow(X)),
                         covar.train = NULL, ...) {
  check_sparseSVM()

  sparseSVM::COPY_sparseSVM(X, y01.train, ind.train, covar.train,
                            ...)
}

################################################################################
