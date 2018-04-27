################################################################################

#' Product with a vector
#'
#' Product between a Filebacked Big Matrix and a vector.
#'
#' @inheritParams bigstatsr-package
#' @param y.col A vector of same size as `ind.col`.
#'
#' @return \eqn{X \cdot y}.
#' @export
#'
#' @examples
#' X <- big_attachExtdata()
#' n <- nrow(X)
#' m <- ncol(X)
#' y <- rnorm(m)
#'
#' test <- big_prodVec(X, y)      # vector
#' true <- X[] %*% y  # one-column matrix
#' all.equal(test, as.numeric(true))
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' tryCatch(test2 <- big_prodVec(X, y, ind.row, ind.col),
#'          error = function(e) print(e))
#' # returns an error. You need to use the subset of y:
#' test2 <- big_prodVec(X, y[ind.col], ind.row, ind.col)
#' true2 <- X[ind.row, ind.col] %*% y[ind.col]
#' all.equal(test2, as.numeric(true2))
#'
big_prodVec <- function(X, y.col,
                        ind.row = rows_along(X),
                        ind.col = cols_along(X)) {

  pMatVec4(X, y.col, ind.row, ind.col)
}

################################################################################

#' Cross-product with a vector
#'
#' Cross-product between a Filebacked Big Matrix and a vector.
#'
#' @inheritParams bigstatsr-package
#' @param y.row A vector of same size as `ind.row`.
#'
#' @return \eqn{X^T \cdot y}.
#' @export
#'
#' @examples
#' X <- big_attachExtdata()
#' n <- nrow(X)
#' m <- ncol(X)
#' y <- rnorm(n)
#'
#' test <- big_cprodVec(X, y)             # vector
#' true <- crossprod(X[], y)  # one-column matrix
#' all.equal(test, as.numeric(true))
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' tryCatch(test2 <- big_cprodVec(X, y, ind.row, ind.col),
#'          error = function(e) print(e))
#' # returns an error. You need to use the subset of y:
#' test2 <- big_cprodVec(X, y[ind.row], ind.row, ind.col)
#' true2 <- crossprod(X[ind.row, ind.col], y[ind.row])
#' all.equal(test2, as.numeric(true2))
#'
big_cprodVec <- function(X, y.row,
                         ind.row = rows_along(X),
                         ind.col = cols_along(X)) {

  cpMatVec4(X, y.row, ind.row, ind.col)
}

################################################################################

#' Product with a matrix
#'
#' Product between a Filebacked Big Matrix and a matrix.
#'
#' @inheritParams bigstatsr-package
#' @param A.col A matrix with `length(ind.col)` rows.
#'
#' @return \eqn{X \cdot A}.
#' @export
#'
#' @examples
#' X <- big_attachExtdata()
#' n <- nrow(X)
#' m <- ncol(X)
#' A <- matrix(0, m, 10); A[] <- rnorm(length(A))
#'
#' test <- big_prodMat(X, A)
#' true <- X[] %*% A
#' all.equal(test, true)
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' tryCatch(test2 <- big_prodMat(X, A, ind.row, ind.col),
#'          error = function(e) print(e))
#' # returns an error. You need to use the subset of A:
#' test2 <- big_prodMat(X, A[ind.col, ], ind.row, ind.col)
#' true2 <- X[ind.row, ind.col] %*% A[ind.col, ]
#' all.equal(test2, true2)
#'
big_prodMat <- function(X, A.col,
                        ind.row = rows_along(X),
                        ind.col = cols_along(X),
                        ncores = 1,
                        block.size = block_size(nrow(X), ncores)) {

  check_args()
  # assert_class(A.col, 'matrix')  # not only (e.g. sparses matrices)
  assert_lengths(ind.col, rows_along(A.col))

  big_apply(X, a.FUN = function(X, ind, M, ind.row, ind.col) {
    X[ind.row, ind.col[ind], drop = FALSE] %*% M[ind, , drop = FALSE]
  }, a.combine = "+", ind = seq_along(ind.col),
  ncores = ncores, block.size = block.size,
  M = A.col, ind.row = ind.row, ind.col = ind.col)
}

################################################################################

#' Cross-product with a matrix
#'
#' Cross-product between a Filebacked Big Matrix and a matrix.
#'
#' @inheritParams bigstatsr-package
#' @param A.row A matrix with `length(ind.row)` rows.
#'
#' @return \eqn{X^T \cdot A}.
#' @export
#'
#' @examples
#' X <- big_attachExtdata()
#' n <- nrow(X)
#' m <- ncol(X)
#' A <- matrix(0, n, 10); A[] <- rnorm(length(A))
#'
#' test <- big_cprodMat(X, A)
#' true <- crossprod(X[], A)
#' all.equal(test, true)
#'
#' # subsetting
#' ind.row <- sample(n, n/2)
#' ind.col <- sample(m, m/2)
#'
#' tryCatch(test2 <- big_cprodMat(X, A, ind.row, ind.col),
#'          error = function(e) print(e))
#' # returns an error. You need to use the subset of A:
#' test2 <- big_cprodMat(X, A[ind.row, ], ind.row, ind.col)
#' true2 <- crossprod(X[ind.row, ind.col], A[ind.row, ])
#' all.equal(test2, true2)
#'
big_cprodMat <- function(X, A.row,
                         ind.row = rows_along(X),
                         ind.col = cols_along(X),
                         ncores = 1,
                         block.size = block_size(nrow(X), ncores)) {

  check_args()
  # assert_class(A.row, 'matrix')  # not only (e.g. sparses matrices)
  assert_lengths(ind.row, rows_along(A.row))

  big_apply(X, a.FUN = function(X, ind, M, ind.row) {
    crossprod(X[ind.row, ind, drop = FALSE], M)
  }, a.combine = "rbind", ind = ind.col,
  ncores = ncores, block.size = block.size,
  M = A.row, ind.row = ind.row)
}

################################################################################
