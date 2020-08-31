################################################################################

context("PROD_MAT")

set.seed(SEED)

################################################################################

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)

################################################################################

for (t in TEST.TYPES) {
  X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

  test_that("standard matrix operations work", {
    A.col <- matrix(rnorm(N * M), M, N)
    A.row <- matrix(rnorm(N * M), N, M)
    if (t == "double") {
      expect_equal(X %*% A.col,          X[] %*% A.col)
      expect_equal(t(A.row) %*% X,       t(A.row) %*% X[])
      expect_equal(crossprod(X, A.row),  crossprod(X[], A.row))
      expect_equal(tcrossprod(X, A.row), tcrossprod(X[], A.row))
      expect_equal(crossprod(A.row, X),  crossprod(A.row, X[]))
      expect_equal(tcrossprod(A.row, X), tcrossprod(A.row, X[]))
    } else {
      ERR <- "for 'double' FBMs only"
      expect_error(X %*% A.col, ERR)
      expect_error(t(A.row) %*% X, ERR)
      expect_error(crossprod(X, A.row), ERR)
      expect_error(tcrossprod(X, A.row), ERR)
      expect_error(crossprod(A.row, X), ERR)
      expect_error(tcrossprod(A.row, X), ERR)
    }
  })

  test_that("equality with %*%", {
    replicate(10, {
      n <- sample(N, size = 1)
      m <- sample(M, size = 1)
      ind.row <- sample(N, size = n)
      ind.col <- sample(M, size = m)
      A.col <- matrix(rnorm(n * m), m, n)
      expect_equal(big_prodMat(X, A.col, ind.row, ind.col, ncores = test_cores()),
                   X[ind.row, ind.col, drop = FALSE] %*% A.col)
      A.row <- matrix(rnorm(n * m), n, m)
      expect_equal(big_cprodMat(X, A.row, ind.row, ind.col, ncores = test_cores()),
                   crossprod(X[ind.row, ind.col, drop = FALSE], A.row))

      center <- rnorm(m); scale <- runif(m)
      expect_equal(big_prodMat(X, A.col, ind.row, ind.col, center = center,
                               ncores = test_cores()),
                   scale(X[ind.row, ind.col, drop = FALSE],
                         center = center, scale = FALSE) %*% A.col)
      expect_equal(big_prodMat(X, A.col, ind.row, ind.col, ncores = test_cores(),
                               center = center, scale = scale),
                   scale(X[ind.row, ind.col, drop = FALSE],
                         center = center, scale = scale) %*% A.col)
      expect_equal(big_cprodMat(X, A.row, ind.row, ind.col, center = center,
                                ncores = test_cores()),
                   crossprod(scale(X[ind.row, ind.col, drop = FALSE],
                                   center = center, scale = FALSE), A.row))
      expect_equal(big_cprodMat(X, A.row, ind.row, ind.col, ncores = test_cores(),
                                center = center, scale = scale),
                   crossprod(scale(X[ind.row, ind.col, drop = FALSE],
                                   center = center, scale = scale), A.row))
    })
  })

  test_that("Incompatiblity between dimensions", {
    ind.row <- sample(N, size = 21)
    ind.col <- sample(M, size = 11)
    A.col <- matrix(1, 21, 11)
    expect_error(big_prodMat(X, A.col, ind.row, ind.col), GET_ERROR_DIM())
    A.row <- matrix(1, 11, 21)
    expect_error(big_cprodMat(X, A.row, ind.row, ind.col), GET_ERROR_DIM())
  })

  test_that("OK with dimension 0 or 1", {
    DIM <- sample(0:1, 1)
    ind.row <- sample(N, size = 17)
    ind.col <- sample(M, size = DIM)
    A.col <- matrix(1, DIM, 7)
    expect_equal(big_prodMat(X, A.col, ind.row, ind.col, ncores = test_cores()),
                 X[ind.row, ind.col, drop = FALSE] %*% A.col)
    A.row <- matrix(1, 17, 7)
    expect_equal(big_cprodMat(X, A.row, ind.row, ind.col, ncores = test_cores()),
                 crossprod(X[ind.row, ind.col, drop = FALSE], A.row))

    ind.row <- sample(N, size = DIM)
    ind.col <- sample(M, size = 17)
    A.col <- matrix(1, 17, 7)
    expect_equal(big_prodMat(X, A.col, ind.row, ind.col, ncores = test_cores()),
                 X[ind.row, ind.col, drop = FALSE] %*% A.col)
    A.row <- matrix(1, DIM, 7)
    expect_equal(big_cprodMat(X, A.row, ind.row, ind.col, ncores = test_cores()),
                 crossprod(X[ind.row, ind.col, drop = FALSE], A.row))
  })
}

################################################################################

test_that("OK with matrix as scaling", {

  X <- FBM(2, 2, init = 1:4)
  A <- matrix(1:4, 2)
  center <- c(0, 0)
  scale <- c(1, 1)

  for (fun1 in list(identity, base::t, matrix)) {
    for (fun2 in list(identity, base::t, matrix)) {
      center2 <- fun1(center)
      scale2  <- fun2(scale)
      expect_equal(big_prodMat(X, A, center = center2, scale = scale2),
                   X[] %*% A)
      expect_equal(big_cprodMat(X, A, center = center2, scale = scale2),
                   crossprod(X[], A))
    }
  }
})

################################################################################
