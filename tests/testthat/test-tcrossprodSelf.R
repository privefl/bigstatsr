################################################################################

context("TCROSSPROD_SELF")

set.seed(SEED)

options(bigstatsr.downcast.warning = FALSE)

################################################################################

# Simulating some data
N <- 43
M <- 101
x <- matrix(rnorm(N * M, 100, 5), N)

################################################################################

test_that("equality with tcrossprod", {
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    K <- big_tcrossprodSelf(X, block.size = 10)
    expect_equal(K[], tcrossprod(X[]))

    if (t == "double") {
      expect_equal(tcrossprod(X), tcrossprod(X[]))
    } else {
      expect_error(tcrossprod(X), "for 'double' FBMs only")
    }
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(M, M / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_tcrossprodSelf(X, ind.col = ind, block.size = 10)
    expect_equal(K[], tcrossprod(X[, ind]))

    # full scaling
    K2 <- big_tcrossprodSelf(X, fun.scaling = big_scale(), ind.col = ind,
                             block.size = 10)
    X.scaled <- scale(X[, ind])
    expect_equal(K2[], tcrossprod(X.scaled))
    expect_equal(attr(K2, "center"), attr(X.scaled, "scaled:center"))
    expect_equal(attr(K2, "scale"),  attr(X.scaled, "scaled:scale"))
  }
})

################################################################################

test_that("equality with tcrossprod with half of the data", {
  ind <- sample(N, N / 2)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    # no scaling
    K <- big_tcrossprodSelf(X, ind.row = ind, block.size = 10)
    expect_equal(K[], tcrossprod(X[ind, ]))

    # full scaling
    K2 <- big_tcrossprodSelf(X, fun.scaling = big_scale(), ind.row = ind,
                             block.size = 10)
    X.scaled <- scale(X[ind, ])
    expect_equal(K2[], tcrossprod(X.scaled))
    expect_equal(attr(K2, "center"), attr(X.scaled, "scaled:center"))
    expect_equal(attr(K2, "scale"),  attr(X.scaled, "scaled:scale"))
  }
})

################################################################################

test_that("we catch zero variance variables when scaling",{
  #create a simple dataset
  set.seed(123)
  X <- FBM(20, 20, init = rnorm(400))
  # big_scale() catches zero variance already. Make sure that we also catch custom functions
  custom_scaling <- function (X, ind.row=1, ind.col=1){
    return(data.frame(center=rep(1,ncol(X)),scale=c(0,rep(1,ncol(X)-1))))
  }
  expect_error(big_tcrossprodSelf(X, fun.scaling = custom_scaling),
               "Some variables have zero variance")
})

