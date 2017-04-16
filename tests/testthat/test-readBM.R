################################################################################

context("READ_BM")

opt.save <- options(bigmemory.typecast.warning = FALSE,
                    bigmemory.default.shared = TRUE)

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N*M, sd = 5), N)

tmp <- tempfile()

################################################################################

test_that("read from write.table", {

  for (t in ALL.TYPES[-1]) {

    X <- as.big.matrix(x, type = t)

    write.table(X[,], tmp, quote = FALSE)

    test <- big_readBM(tmp,
                       file.nheader = 1,
                       info.nelem = 1,
                       read.what = X[1, 1],
                       BM.type = t,
                       descriptor = FALSE,
                       transpose = TRUE)

    expect_equal(test[,], X[,])
  }
})

################################################################################

dimnames(x) <- list(sample(letters, N, TRUE), sample(LETTERS, M , TRUE))

test_that("read from write.table with dimnames", {

  for (t in ALL.TYPES[-1]) {

    X <- as.big.matrix(x, type = t)

    write.table(X[,], tmp, quote = FALSE)

    test <- big_readBM(tmp,
                       file.nheader = 1,
                       info.nelem = 1,
                       read.what = X[1, 1],
                       BM.type = t,
                       descriptor = FALSE,
                       transpose = TRUE)

    colnames <- attr(test, "info")[1, ]
    rownames <- strsplit(attr(test, "header"),
                         split = " ",
                         fixed = TRUE)[[1]]

    expect_equal(structure(test[,], .Dimnames = list(colnames, rownames)), X[,])
  }
})

################################################################################

options(opt.save)

################################################################################
