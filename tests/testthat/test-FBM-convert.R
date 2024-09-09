################################################################################

context("FBM_CONVERT")

opt.save <- options(bigstatsr.downcast.warning = TRUE)

set.seed(SEED)

################################################################################

to_gen <- function(x) {

  if (gen == "scalar") {
    `if`(typeof(x) == "double", x[1]^2, x[1])
  } else if (gen == "vector") {
    x
  } else {
    dim(x) <- c(10, 10)
    x
  }
}

gen <- "scalar"
expect_identical(to_gen(1:10), 1L)
gen <- "vector"
expect_identical(to_gen(1:10), 1:10)
gen <- "matrix"
expect_identical(to_gen(1:100), matrix(1:100, 10))

################################################################################

expect_FBM <- function(expr) {
  res <- eval.parent(substitute(expr))
  expect_s4_class(res, class = "FBM")
}

expect_warning(FBM(10, 10, "raw", runif(100)))
if (exists("XX")) rm("XX")
expect_false(exists("XX"))
expect_FBM(without_downcast_warning(XX <- FBM(10, 10, "raw", runif(100))))
expect_true(exists("XX"))

################################################################################

test_that("Downcast warnings work", {

  skip_if_not(not_cran)

  get_text <- function(rtype, ctype) {
    sprintf("while converting from R type '%s' to C type '%s'.", rtype, ctype)
  }

  for (gen in c("scalar", "vector", "matrix")) {

    ## From double
    x1 <- to_gen(runif(100))
    # To raw
    expect_warning(X <- FBM(10, 10, x1, type = "raw"),
                   get_text("double", "unsigned char (raw)"), fixed = TRUE)
    expect_warning(X[1:5] <- x1[1:5],
                   get_text("double", "unsigned char (raw)"), fixed = TRUE)
    expect_warning(X[2] <- NA_real_,
                   get_text("double", "unsigned char (raw)"), fixed = TRUE)
    expect_FBM(without_downcast_warning( FBM(10, 10, x1, type = "raw") ))
    # To ushort
    expect_warning(X <- FBM(10, 10, x1, type = "unsigned short"),
                   get_text("double", "unsigned short"), fixed = TRUE)
    expect_warning(X[1:5] <- x1[1:5],
                   get_text("double", "unsigned short"), fixed = TRUE)
    expect_warning(X[2] <- NA_real_,
                   get_text("double", "unsigned short"), fixed = TRUE)
    expect_FBM(without_downcast_warning( FBM(10, 10, x1, type = "unsigned short") ))
    # To int
    expect_warning(X <- FBM(10, 10, x1, type = "integer"),
                   get_text("double", "integer"), fixed = TRUE)
    expect_warning(X[1:5] <- x1[1:5],
                   get_text("double", "integer"), fixed = TRUE)
    X[2] <- NA_real_
    expect_identical(X[2], NA_integer_)
    expect_warning(X[2] <- Inf,  get_text("double", "integer"), fixed = TRUE)
    expect_identical(X[2], NA_integer_)
    expect_warning(X[2] <- -Inf, get_text("double", "integer"), fixed = TRUE)
    expect_identical(X[2], NA_integer_)
    expect_warning(X[2] <- NaN,  get_text("double", "integer"), fixed = TRUE)
    expect_identical(X[2], NA_integer_)
    expect_FBM(without_downcast_warning( FBM(10, 10, x1, type = "integer") ))
    # To float
    expect_warning(X <- FBM(10, 10, x1, type = "float"))
    X[2] <- NA_real_; expect_identical(X[2], NA_real_)
    X[2] <- Inf;      expect_identical(X[2], Inf)
    X[2] <- -Inf;     expect_identical(X[2], -Inf)
    X[2] <- NaN;      expect_identical(X[2], NaN)
    # To double
    expect_FBM(X <- FBM(10, 10, x1, type = "double"))
    expect_identical(X[1:5] <- x1[1:5], x1[1:5])
    X[2] <- NA_real_; expect_identical(X[2], NA_real_)
    X[2] <- Inf;      expect_identical(X[2], Inf)
    X[2] <- -Inf;     expect_identical(X[2], -Inf)
    X[2] <- NaN;      expect_identical(X[2], NaN)


    ## From integer
    x2 <- to_gen(1:100 + 1e6L)
    # To raw
    expect_warning(X <- FBM(10, 10, x2, type = "raw"),
                   get_text("integer", "unsigned char (raw)"), fixed = TRUE)
    expect_warning(X[1:5] <- x2[1:5],
                   get_text("integer", "unsigned char (raw)"), fixed = TRUE)
    expect_warning(X[2] <- NA_integer_,
                   get_text("integer", "unsigned char (raw)"), fixed = TRUE)
    expect_FBM(without_downcast_warning( FBM(10, 10, x2, type = "raw") ))
    # To ushort
    expect_warning(X <- FBM(10, 10, x2, type = "unsigned short"),
                   get_text("integer", "unsigned short"), fixed = TRUE)
    expect_warning(X[1:5] <- x2[1:5],
                   get_text("integer", "unsigned short"), fixed = TRUE)
    expect_warning(X[2] <- NA_integer_,
                   get_text("integer", "unsigned short"), fixed = TRUE)
    expect_FBM(without_downcast_warning( FBM(10, 10, x2, type = "unsigned short") ))
    # To int
    expect_FBM(X <- FBM(10, 10, x2, type = "integer"))
    expect_identical(X[1:5] <- x2[1:5], x2[1:5])
    X[2] <- NA_integer_
    expect_identical(X[2], NA_integer_)
    # To float
    expect_FBM(X <- FBM(10, 10, x2, type = "float"))
    expect_identical(X[1:5] <- x2[1:5], x2[1:5])
    X[2] <- NA_integer_
    expect_identical(X[2], NA_real_)
    # To double
    expect_FBM(X <- FBM(10, 10, x2, type = "double"))
    expect_identical(X[1:5] <- x2[1:5], x2[1:5])
    X[2] <- NA_integer_
    expect_identical(X[2], NA_real_)


    ## From logical
    x3 <- to_gen(sample(c(TRUE, FALSE), 100, TRUE))
    # To raw
    expect_FBM(X <- FBM(10, 10, x3, type = "raw"))
    expect_identical(X[1:5] <- na.omit(x3[1:5]), na.omit(x3[1:5]))
    expect_warning(X[2] <- NA,
                   get_text("logical", "unsigned char (raw)"), fixed = TRUE)
    # To ushort
    expect_FBM(X <- FBM(10, 10, x3, type = "unsigned short"))
    expect_identical(X[1:5] <- na.omit(x3[1:5]), na.omit(x3[1:5]))
    expect_warning(X[2] <- NA,
                   get_text("logical", "unsigned short"), fixed = TRUE)
    # To int
    expect_FBM(X <- FBM(10, 10, x3, type = "integer"))
    expect_identical(X[1:5] <- x3[1:5], x3[1:5])
    X[2] <- NA
    expect_identical(X[2], NA_integer_)
    # To float
    expect_FBM(X <- FBM(10, 10, x3, type = "float"))
    expect_identical(X[1:5] <- x3[1:5], x3[1:5])
    X[2] <- NA
    expect_identical(X[2], NA_real_)
    # To double
    expect_FBM(X <- FBM(10, 10, x3, type = "double"))
    expect_identical(X[1:5] <- x3[1:5], x3[1:5])
    X[2] <- NA
    expect_identical(X[2], NA_real_)


    ## From raw
    x4 <- to_gen(sample(as.raw(0:255), 100, TRUE))
    expect_FBM(X <- FBM(10, 10, x4, type = "raw"))
    expect_identical(X[1:5] <- x4[1:5], x4[1:5])
    expect_FBM(X <- FBM(10, 10, x4, type = "unsigned short"))
    expect_identical(X[1:5] <- x4[1:5], x4[1:5])
    expect_FBM(X <- FBM(10, 10, x4, type = "integer"))
    expect_identical(X[1:5] <- x4[1:5], x4[1:5])
    expect_FBM(X <- FBM(10, 10, x4, type = "float"))
    expect_identical(X[1:5] <- x4[1:5], x4[1:5])
    expect_FBM(X <- FBM(10, 10, x4, type = "double"))
    expect_identical(X[1:5] <- x4[1:5], x4[1:5])

  }

})

################################################################################

test_that("Missing values transfer from int to double", {

  for (t in c("float", "double")) {

    for (na in list(NA, NA_integer_, NA_real_)) {
      a <- matrix(na, 7, 11)
      A <- as_FBM(a, type = "double")
      A[] <- a
      expect_true(all(is.na(A[])))
      A[] <- as.vector(a)
      expect_true(all(is.na(A[])))
      A[] <- a[1]
      expect_true(all(is.na(A[])))
      A[] <- matrix(a[1], 1, 1)
      expect_true(all(is.na(A[])))
      A[1:5] <- a[1:5]
      expect_true(all(is.na(A[1:5])))
      A[1:5] <- matrix(a[1:5], 5, 1)
      expect_true(all(is.na(A[1:5])))
      A[1:5] <- a[5]
      expect_true(all(is.na(A[1:5])))
      A[1:5] <- matrix(a[5], 1, 1)
      expect_true(all(is.na(A[1:5])))
      A[2] <- a[2]
      expect_identical(A[2], NA_real_)
    }

  }

})

################################################################################

test_that("Special values are handled for floats", {

  expect_identical(expect_warning(FBM(1, 1, "float", 2^-126), "NA")[], NA_real_)

  expect_warning(FBM(1, 1, "float", 1.175494e-38)[])
  expect_warning(FBM(1, 1, "float", 1.1754943e-38)[])
  expect_warning(FBM(1, 1, "float", 0.51))
  expect_identical(FBM(1, 1, "float", 0.5)[], 0.5)
  expect_identical(FBM(1, 1, "float", 2^-149)[], 2^-149)

  expect_identical(FBM(1, 1, "float", NA)[], NA_real_)
  expect_identical(FBM(1, 1, "float", NA_integer_)[], NA_real_)
  expect_identical(FBM(1, 1, "float", NA_real_)[], NA_real_)
  expect_identical(FBM(1, 1, "float", Inf)[], Inf)
  expect_identical(FBM(1, 1, "float", -Inf)[], -Inf)
  expect_identical(FBM(1, 1, "float", NaN)[], NaN)
})

################################################################################

test_that("No copy is made", {

  options(bigstatsr.downcast.warning = FALSE)

  N <- M <- 2000
  size <- N * M * 8 / 1024^2
  x4 <- matrix(round(rnorm(N * M, 100, 10)), N)
  x3 <- x2 <- x1 <- x4
  storage.mode(x1) <- "raw"
  storage.mode(x2) <- "logical"
  storage.mode(x3) <- "integer"

  X5 <- big_copy(x4, type = "double")
  X4 <- big_copy(x4, type = "float")
  X3 <- big_copy(x4, type = "integer")
  X2 <- big_copy(x4, type = "unsigned short")
  X1 <- big_copy(x4, type = "unsigned char")

  tmp <- gc(reset = TRUE)
  x <- x3 + 0
  expect_gt((gc() - tmp)[2, 6], size / 10)

  # print(size)
  for (X in list(X1, X2, X3, X4, X5)) {
    # print(typeof(X))
    for (x in list(x1, x2, x3, x4)) {
      tmp <- gc(reset = TRUE)
      X[] <- x
      diff <- gc() - tmp
      mb <- tail(diff["Vcells", ], 1)
      expect_true(names(mb) == "(Mb)")
      expect_lt(mb, size / 10)
    }
  }
})

################################################################################

options(opt.save)

################################################################################
