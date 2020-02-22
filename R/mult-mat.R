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
#' X2 <- big_copy(X, type = "double")
#' all.equal(X2 %*% A, true)
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
                        block.size = block_size(nrow(X), ncores),
                        center = NULL,
                        scale = NULL) {

  check_args()
  assert_lengths(ind.col, rows_along(A.col))
  # assert_class(A.col, 'matrix')  # not only (e.g. sparse matrices)

  if (length(ind.row) == 0 || length(ind.col) == 0)
    return(matrix(0, length(ind.row), ncol(A.col)))

  if (!is.null(scale)) {
    assert_lengths(scale, ind.col)
    A.col <- A.col / as_vec(scale)
  }
  if (!is.null(center)) {
    assert_lengths(center, ind.col)
    center2 <- crossprod(as_vec(center), A.col)
  }

  res <- prod_FBM_block_mat(X, A.col, ind.row, ind.col, block.size)

  `if`(is.null(center), res, centering(res, center2))
}

################################################################################

#' @export
#' @param x A 'double' FBM or a matrix.
#' @param y A 'double' FBM or a matrix.
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
#' X2 <- big_copy(X, type = "double")
#' all.equal(crossprod(X2, A), true)
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
                         block.size = block_size(nrow(X), ncores),
                         center = NULL,
                         scale = NULL) {

  check_args()
  assert_lengths(ind.row, rows_along(A.row))
  # assert_class(A.row, 'matrix')  # not only (e.g. sparse matrices)

  if (length(ind.row) == 0 || length(ind.col) == 0)
    return(matrix(0, length(ind.col), ncol(A.row)))

  if (!is.null(scale)) {
    assert_lengths(scale, ind.col)
    scale <- as_vec(scale)
  }
  if (!is.null(center)) {
    assert_lengths(center, ind.col)
    center <- as_vec(center)
  }

  res <- cprod_FBM_block_mat(X, A.row, ind.row, ind.col, block.size)

  if (!is.null(center)) res <- res - tcrossprod(center, colSums(A.row))
  if (!is.null(scale))  res <- res / scale

  res
}

################################################################################

#' @export
#' @param x A 'double' FBM or a matrix.
#' @param y A 'double' FBM or a matrix.
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
