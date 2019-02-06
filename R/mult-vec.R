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
