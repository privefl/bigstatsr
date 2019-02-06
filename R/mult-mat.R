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

  if (length(ind.row) > 0 && length(ind.col) > 0) {
    big_apply(X, a.FUN = function(X, ind, M, ind.row, ind.col) {
      X[ind.row, ind.col[ind], drop = FALSE] %*% M[ind, , drop = FALSE]
    }, a.combine = "plus", ind = seq_along(ind.col),
    ncores = ncores, block.size = block.size,
    M = A.col, ind.row = ind.row, ind.col = ind.col)
  } else {
    matrix(0, length(ind.row), ncol(A.col))
  }
}

################################################################################

#' @export
#' @rdname big_prodMat
setMethod("%*%", signature(x = "FBM", y = "matrix"),
          function(x, y) prod_FBM_mat(x, y))

#' @export
#' @rdname big_prodMat
setMethod("%*%", signature(x = "matrix", y = "FBM"),
          function(x, y) prod_mat_FBM(x, y))

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

  if (length(ind.row) > 0 && length(ind.col) > 0) {
    big_apply(X, a.FUN = function(X, ind, M, ind.row) {
      crossprod(X[ind.row, ind, drop = FALSE], M)
    }, a.combine = "rbind", ind = ind.col,
    ncores = ncores, block.size = block.size,
    M = A.row, ind.row = ind.row)
  } else {
    matrix(0, length(ind.col), ncol(A.row))
  }
}

################################################################################

#' @export
#' @rdname big_cprodMat
setMethod("crossprod",  signature(x = "FBM", y = "matrix"),
          function(x, y) crossprod_FBM_mat(x, y))

#' @export
#' @rdname big_cprodMat
setMethod("tcrossprod", signature(x = "FBM", y = "matrix"),
          function(x, y) tcrossprod_FBM_mat(x, y))

#' @export
#' @rdname big_cprodMat
setMethod("crossprod",  signature(x = "matrix", y = "FBM"),
          function(x, y) crossprod_mat_FBM(x, y))

#' @export
#' @rdname big_cprodMat
setMethod("tcrossprod", signature(x = "matrix", y = "FBM"),
          function(x, y) tcrossprod_mat_FBM(x, y))

################################################################################
