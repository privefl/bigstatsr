################################################################################

context("CHECK_ARGS")

################################################################################

# defining some objects
X <- tmpFBM(descriptor = FALSE)(10, 10, type = "raw")
X.desc <- tmpFBM()(10, 10, type = "raw")
X.code <- as.BM.code(X, code = rep(0, 256))
X.code.desc <- as.BM.code(X.desc, code = rep(0, 256))


test_that("checking of BM (desc) arguments", {
  # X
  expect_null( (function(X) check_args())(X))
  expect_error((function(X) check_args())(X.desc))
  expect_null( (function(X) check_args())(X.code))
  expect_error((function(X) check_args())(X.code.desc))
  # X.desc
  expect_error((function(X.desc) check_args())(X))
  expect_null( (function(X.desc) check_args())(X.desc))
  expect_error((function(X.desc) check_args())(X.code))
  expect_null( (function(X.desc) check_args())(X.code.desc))
  # X.
  expect_null((function(X.) check_args())(X))
  expect_null((function(X.) check_args())(X.desc))
  expect_null((function(X.) check_args())(X.code))
  expect_null((function(X.) check_args())(X.code.desc))
  # X.code
  expect_error((function(X.code) check_args())(X))
  expect_error((function(X.code) check_args())(X.desc))
  expect_null( (function(X.code) check_args())(X.code))
  expect_null( (function(X.code) check_args())(X.code.desc))
})

################################################################################

# defining some objects
y <- rnorm(10)
y01 <- sample(0:1, size = 10, replace = TRUE)
ind1 <- 1:10
ind2 <- -c(1:10)
ind3 <- -10:10
ind4 <- seq(1, 10, by = 0.8)

test_that("checking of vector arguments", {
  # y01.train
  expect_error((function(y01.train) check_args())(y))
  expect_null( (function(y01.train) check_args())(y01))
  # ind.train
  expect_null( (function(ind.train) check_args())(ind1))
  expect_error((function(ind.train) check_args())(ind2))
  expect_error((function(ind.train) check_args())(ind3))
  expect_error((function(ind.train) check_args())(ind4))
  # ind.row
  expect_null( (function(ind.row) check_args())(ind1))
  expect_error((function(ind.row) check_args())(ind2))
  expect_error((function(ind.row) check_args())(ind3))
  expect_error((function(ind.row) check_args())(ind4))
  # ind.col
  expect_null( (function(ind.col) check_args())(ind1))
  expect_error((function(ind.col) check_args())(ind2))
  expect_error((function(ind.col) check_args())(ind3))
  expect_error((function(ind.col) check_args())(ind4))
})

################################################################################

# defining some objects
f1 <- tmpFBM()
f2 <- big_scale()
f3 <- tmpFBM
f4 <- big_scale

test_that("checking of function arguments", {
  # fun.scaling
  expect_error((function(fun.scaling) check_args())(f1))
  expect_null( (function(fun.scaling) check_args())(f2))
  expect_error((function(fun.scaling) check_args())(f3))
  expect_error((function(fun.scaling) check_args())(f4))
  # fun.createBM
  expect_null( (function(fun.createBM) check_args())(f1))
  expect_error((function(fun.createBM) check_args())(f2))
  expect_error((function(fun.createBM) check_args())(f3))
  expect_error((function(fun.createBM) check_args())(f4))
})

################################################################################

# defining some objects
ncores <- getOption("bigstatsr.ncores.max")
covar1 <- NULL
covar2 <- matrix(0, 10, 2)

test_that("checking of other arguments", {
  # ncores
  expect_null( (function(ncores) check_args())(ncores))
  expect_error((function(ncores) check_args())(ncores + 1))
  expect_error((function(ncores) check_args())(Inf))
  opt.save <- options(bigstatsr.ncores.max = Inf)
  expect_null( (function(ncores) check_args())(ncores + 1))
  expect_null( (function(ncores) check_args())(Inf))
  options(opt.save)
  # covar.train
  expect_null( (function(covar.train) check_args())(covar1))
  expect_null( (function(covar.train) check_args())(covar2))
  expect_error((function(covar.train) check_args())(ind1))
  # covar.row
  expect_null( (function(covar.row) check_args())(covar1))
  expect_null( (function(covar.row) check_args())(covar2))
  expect_error((function(covar.row) check_args())(ind1))
})

################################################################################

test_that("overwrite default checking", {
  # ind.train
  f1 <- function(ind.train) check_args(
    ind.train = "assert_int(ind.train); assert_pos(ind.train)")
  expect_error(f1(ind2))

  f2 <- function(ind.train) check_args(ind.train = "assert_int(ind.train)")
  expect_null(f2(ind2))
})

################################################################################

ind <- 1:10
y <- rnorm(10)
covar <- matrix(0, 10, 2)

test_that("checking lengths", {
  expect_error(assert_lengths(ind))
  expect_null( assert_lengths(ind, y))
  expect_error(assert_lengths(ind[-1], y))
  expect_error( assert_lengths(ind, y, covar))
  expect_null( assert_lengths(ind, y, rows_along(covar)))
})

################################################################################

X.code@code <- rep(NA_real_, 256)

test_that("checking missing values", {
  expect_error((function(X.code) check_args())(X.code))
  expect_null((function(X.code) check_args(
    X.code = "assert_classOrDesc(X.code, 'BM.code')"))(X.code))
})

################################################################################

