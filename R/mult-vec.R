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
                        ind.col = cols_along(X),
                        center = NULL,
                        scale = NULL,
                        ncores = 1) {

  check_args()
  assert_lengths(y.col, ind.col)

  if (length(ind.row) == 0 || length(ind.col) == 0)
    return(rep(0, length(ind.row)))

  if (!is.null(scale)) {
    assert_lengths(scale, ind.col)
    y.col <- y.col / as_vec(scale)
  }
  if (!is.null(center)) {
    assert_lengths(center, ind.col)
    center2 <- drop(crossprod(as_vec(center), y.col))
  }

  res <- big_parallelize(X, function(X, ind, y.col, ind.row, ind.col) {
    pMatVec4(X, y.col[ind], ind.row, ind.col[ind])
  }, p.combine = plus, ind = seq_along(ind.col), ncores = ncores,
  y.col = y.col, ind.row = ind.row, ind.col = ind.col)

  `if`(is.null(center), res, res - center2)
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
                         ind.col = cols_along(X),
                         center = NULL,
                         scale = NULL,
                         ncores = 1) {

  check_args()
  assert_lengths(y.row, ind.row)

  if (length(ind.row) == 0 || length(ind.col) == 0)
    return(rep(0, length(ind.col)))

  if (!is.null(scale)) {
    assert_lengths(scale, ind.col)
    scale <- as_vec(scale)
  }
  if (!is.null(center)) {
    assert_lengths(center, ind.col)
    center2 <- sum(y.row) * as_vec(center)
  }

  res <- big_parallelize(X, function(X, ind, y.row, ind.row) {
    cpMatVec4(X, y.row, ind.row, ind)
  }, p.combine = 'c', ind = ind.col, ncores = ncores,
  y.row = y.row, ind.row = ind.row)

  if (!is.null(center)) res <- res - center2
  if (!is.null(scale))  res <- res / scale

  res
}

################################################################################
