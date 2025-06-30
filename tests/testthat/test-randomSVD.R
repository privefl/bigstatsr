################################################################################

context("RANDOM_SVD")

set.seed(SEED)

################################################################################

TOL <- 1e-4

# function for sampling scaling
sampleScale <- function() {
  tmp <- sample(list(c(TRUE, FALSE),
                     c(TRUE, TRUE),
                     c(FALSE, FALSE)))[[1]]
  list(center = tmp[1], scale = tmp[2])
}

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)

################################################################################

test_that("equality with prcomp", {

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    expect_error(big_randomSVD(X, ind.row = NULL), "'ind.row' can't be `NULL`.")
    expect_error(big_randomSVD(X, ind.col = NULL), "'ind.col' can't be `NULL`.")

    k <- sample(c(2, 5, 20), 1) # 2, 5 or 20

    test <- big_randomSVD(X, k = k, tol = 1e-10, ncores = test_cores())
    pca <- prcomp(X[], center = FALSE, scale. = FALSE)
    expect_equal(diffPCs(predict(test), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

    sc <- sampleScale()
    test <- big_randomSVD(X, k = k, tol = 1e-10, ncores = test_cores(),
                          fun.scaling = big_scale(center = sc$center,
                                                  scale = sc$scale))
    pca <- prcomp(X[], center = sc$center, scale. = sc$scale)
    expect_equal(diffPCs(predict(test), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)
    if (sc$center) expect_equal(test$center, pca$center)
    if (sc$scale)  expect_equal(test$scale,  pca$scale)

    p <- plot(test, type = sample(c("screeplot", "scores", "loadings"), 1))
    expect_s3_class(p, "ggplot")

    expect_error(predict(test, abc = 2), "Argument 'abc' not used.")
    expect_error(plot(test, abc = 2), "Argument 'abc' not used.")
  }
})

################################################################################

test_that("equality with prcomp with half of the data", {

  ind <- sample(N, N / 2)
  ind2 <- setdiff(1:N, ind)

  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    k <- sample(c(2, 5, 20), 1) # 2, 5 or 20
    sc <- sampleScale()

    test <- big_randomSVD(X, ncores = test_cores(),
                          ind.row = ind, k = k, tol = 1e-10,
                          fun.scaling = big_scale(center = sc$center,
                                                  scale = sc$scale))
    pca <- prcomp(X[ind, ], center = sc$center, scale. = sc$scale)

    expect_equal(diffPCs(predict(test), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

    if (sc$center) expect_equal(test$center, pca$center)
    if (sc$scale)  expect_equal(test$scale,  pca$scale)

    expect_equal(diffPCs(predict(test, X, ind.row = ind2),
                         predict(pca, X[ind2, ])), 0, tolerance = TOL)

    p <- plot(test, type = sample(c("screeplot", "scores", "loadings"), 1))
    expect_s3_class(p, "ggplot")
  }
})

################################################################################

test_that("equality with prcomp with half of half of the data", {

  ind <- sample(N, N / 2)
  ind2 <- setdiff(1:N, ind)
  ind.col <- sample(M, M / 2)
  for (t in TEST.TYPES) {
    X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

    k <- sample(c(2, 5, 20), 1) # 2, 5 or 20
    sc <- sampleScale()

    test <- big_randomSVD(X, ind.row = ind, ind.col = ind.col,
                          k = k, tol = 1e-10, ncores = test_cores(),
                          fun.scaling = big_scale(center = sc$center,
                                                  scale = sc$scale))
    pca <- prcomp(X[ind, ind.col], center = sc$center, scale. = sc$scale)

    expect_equal(diffPCs(predict(test), pca$x), 0, tolerance = TOL)
    expect_equal(diffPCs(test$v, pca$rotation), 0, tolerance = TOL)

    if (sc$center) expect_equal(test$center, pca$center)
    if (sc$scale)  expect_equal(test$scale,  pca$scale)

    expect_equal(diffPCs(predict(test, X, ind.row = ind2, ind.col = ind.col),
                         predict(pca, X[ind2, ind.col])), 0, tolerance = TOL)

    p <- plot(test, type = sample(c("screeplot", "scores", "loadings"), 1))
    expect_s3_class(p, "ggplot")
  }
})

################################################################################

test_that("as_scaling_fun() works", {

  df0 <- data.frame(center = 1:6, scale = 2:7)
  fun.scaling <- as_scaling_fun(df0$center, df0$scale)
  expect_identical(fun.scaling(NULL, NULL, 1:3), df0[1:3, ])
  fun.scaling2 <- as_scaling_fun(1:6, 2:7, ind.col = 6:1)
  expect_identical(fun.scaling2(NULL, NULL, 1:3), df0[6:4, ])

  X <- big_attachExtdata()
  sc <- big_scale()(X)
  fun <- as_scaling_fun(center = sc$center, scale = sc$scale)
  obj.svd <- big_randomSVD(X, fun.scaling = fun, ncores = test_cores())
  obj.svd2 <- big_randomSVD(X, fun.scaling = big_scale(), ncores = test_cores())
  expect_equal(obj.svd, obj.svd2)
})

################################################################################

test_that("zero variance is caught", {

  X <- FBM(20, 20, init = rnorm(400))
  expect_no_error(big_randomSVD(X, big_scale()))

  X[, 1] <- 0  # set a variable to have zero variance
  # this is also catched as a warning in big_scale()
  expect_error(expect_warning(big_randomSVD(X, big_scale()),
                              "Some variables have zero scaling"),
               "Some variables have zero scaling")

  expect_error(big_randomSVD(X, custom_scaling),
               "Some variables have zero scaling")
  expect_error(big_randomSVD(X, custom_scaling, ncores = test_cores()),
               "Some variables have zero scaling")
})

################################################################################
