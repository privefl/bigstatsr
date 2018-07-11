################################################################################

context("FBM_CONVERT")

opt.save <- options(bigstatsr.downcast.warning = TRUE)

set.seed(SEED)

################################################################################

to_gen <- function(x) {

  if (gen == "scalar") {
    x[1]
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

get_text <- function(rtype, ctype) {
  sprintf("while converting from R type '%s' to C type '%s'.", rtype, ctype)
}

for (gen in c("scalar", "vector", "matrix")) {

  # From double
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
  expect_warning(X[2] <- Inf,
                 get_text("double", "integer"), fixed = TRUE)
  expect_warning(X[2] <- NaN,
                 get_text("double", "integer"), fixed = TRUE)
  expect_FBM(without_downcast_warning( FBM(10, 10, x1, type = "integer") ))
  # To double
  expect_FBM(X <- FBM(10, 10, x1, type = "double"))
  expect_identical(X[1:5] <- x1[1:5], x1[1:5])
  X[2] <- NA_real_
  expect_identical(X[2], NA_real_)


  # From integer
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
  # To double
  expect_FBM(X <- FBM(10, 10, x2, type = "double"))
  expect_identical(X[1:5] <- x2[1:5], x2[1:5])
  X[2] <- NA_integer_
  expect_identical(X[2], NA_real_)


  # From logical
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
  # To double
  expect_FBM(X <- FBM(10, 10, x3, type = "double"))
  expect_identical(X[1:5] <- x3[1:5], x3[1:5])
  X[2] <- NA
  expect_identical(X[2], NA_real_)


  # From raw
  x4 <- to_gen(sample(as.raw(0:255), 100, TRUE))
  expect_FBM(X <- FBM(10, 10, x4, type = "raw"))
  expect_identical(X[1:5] <- x4[1:5], x4[1:5])
  expect_FBM(X <- FBM(10, 10, x4, type = "unsigned short"))
  expect_identical(X[1:5] <- x4[1:5], x4[1:5])
  expect_FBM(X <- FBM(10, 10, x4, type = "integer"))
  expect_identical(X[1:5] <- x4[1:5], x4[1:5])
  expect_FBM(X <- FBM(10, 10, x4, type = "double"))
  expect_identical(X[1:5] <- x4[1:5], x4[1:5])

}

################################################################################

options(opt.save)

################################################################################
