################################################################################

#' Product with a vector
#'
#' Product between a "big.matrix" and a vector.
#'
#' @inheritParams bigstatsr-package
#' @param y A vector of same size as `ind.col`.
#'
#' @return \eqn{X /cdot y}.
#' @export
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' n <- nrow(X.desc)
#' m <- ncol(X.desc)
#' y <- rnorm(m)
#'
#' test <- big_prodVec(X.desc, y)      # vector
#' true <- attach.BM(X.desc)[,] %*% y  # one-column matrix
#' all.equal(test, as.numeric(true))
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' \dontrun{test2 <- big_prodVec(X.desc, y, ind.row, ind.col)}
#' # returns an error. You need to use the subset of y:
#' test2 <- big_prodVec(X.desc, y[ind.col], ind.row, ind.col)
#' true2 <- attach.BM(X.desc)[ind.row, ind.col] %*% y[ind.col]
#' all.equal(test2, as.numeric(true2))
big_prodVec <- function(X., y,
                        ind.row = rows_along(X.),
                        ind.col = cols_along(X.)) {
  X <- attach.BM(X.)
  pMatVec4(X, y, ind.row, ind.col)
}

################################################################################

#' Cross-product with a vector
#'
#' Cross-product between a "big.matrix" and a vector.
#'
#' @inheritParams bigstatsr-package
#' @param y A vector of same size as `ind.row`.
#'
#' @return \eqn{X^T /cdot y}.
#' @export
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' n <- nrow(X.desc)
#' m <- ncol(X.desc)
#' y <- rnorm(n)
#'
#' test <- big_cprodVec(X.desc, y)             # vector
#' true <- crossprod(attach.BM(X.desc)[,], y)  # one-column matrix
#' all.equal(test, as.numeric(true))
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' \dontrun{test2 <- big_cprodVec(X.desc, y, ind.row, ind.col)}
#' # returns an error. You need to use the subset of y:
#' test2 <- big_cprodVec(X.desc, y[ind.row], ind.row, ind.col)
#' true2 <- crossprod(attach.BM(X.desc)[ind.row, ind.col], y[ind.row])
#' all.equal(test2, as.numeric(true2))
big_cprodVec <- function(X., y,
                         ind.row = rows_along(X.),
                         ind.col = cols_along(X.)) {
  X <- attach.BM(X.)
  cpMatVec4(X, y, ind.row, ind.col)
}

################################################################################

#' Product with a matrix
#'
#' Product between a "big.matrix" and a matrix.
#'
#' @inheritParams bigstatsr-package
#' @param A A matrix with `length(ind.col)` rows.
#'
#' @return \eqn{X /cdot A}.
#' @export
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' n <- nrow(X.desc)
#' m <- ncol(X.desc)
#' A <- matrix(0, m, 10); A[] <- rnorm(length(A))
#'
#' test <- big_prodMat(X.desc, A)
#' true <- attach.BM(X.desc)[,] %*% A
#' all.equal(test, true)
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' \dontrun{test2 <- big_prodMat(X.desc, A, ind.row, ind.col)}
#' # returns an error. You need to use the subset of A:
#' test2 <- big_prodMat(X.desc, A[ind.col, ], ind.row, ind.col)
#' true2 <- attach.BM(X.desc)[ind.row, ind.col] %*% A[ind.col, ]
#' all.equal(test2, true2)
big_prodMat <- function(X., A,
                        ind.row = rows_along(X.),
                        ind.col = cols_along(X.),
                        block.size = 1000,
                        ncores2 = 1) {
  stopifnot(length(ind.col) == nrow(A))

  big_apply(X., a.FUN = function(x, ind, A, ind.row, ind.col) {
    x[ind.row, ind.col[ind]] %*% A[ind, ]
  }, a.combine = '+', block.size = block.size,
  ind = seq_along(ind.col), ncores = ncores2, A = A,
  ind.row = ind.row, ind.col = ind.col)
}

################################################################################

#' Cross-product with a matrix
#'
#' Cross-product between a "big.matrix" and a matrix.
#'
#' @inheritParams bigstatsr-package
#' @param A A matrix with `length(ind.row)` rows.
#'
#' @return \eqn{X^T /cdot A}.
#' @export
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' n <- nrow(X.desc)
#' m <- ncol(X.desc)
#' A <- matrix(0, n, 10); A[] <- rnorm(length(A))
#'
#' test <- big_cprodMat(X.desc, A)
#' true <- crossprod(attach.BM(X.desc)[,], A)
#' all.equal(test, true)
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' \dontrun{test2 <- big_cprodMat(X.desc, A, ind.row, ind.col)}
#' # returns an error. You need to use the subset of A:
#' test2 <- big_cprodMat(X.desc, A[ind.row, ], ind.row, ind.col)
#' true2 <- crossprod(attach.BM(X.desc)[ind.row, ind.col], A[ind.row, ])
#' all.equal(test2, true2)
big_cprodMat <- function(X., A,
                         ind.row = rows_along(X.),
                         ind.col = cols_along(X.),
                         block.size = 1000,
                         ncores2 = 1) {
  stopifnot(length(ind.row) == nrow(A))

  big_apply(X., a.FUN = function(x, ind, A, ind.row) {
    crossprod(x[ind.row, ind], A)
  }, a.combine = 'rbind', block.size = block.size,
  ind = ind.col, ncores = ncores2, A = A, ind.row = ind.row)
}

################################################################################
