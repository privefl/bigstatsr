################################################################################

context("READ_BM")

# Simulating some data
N <- 73
M <- 43
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)

tmp <- tempfile()

################################################################################

test_that("read from write.table", {

  for (t in TEST.TYPES[-1]) {

    X <- big_copy(x, type = t)

    write.table(X[], tmp, quote = FALSE)

    test <- big_read(tmp,
                     file.nheader = 1,
                     info.nelem = 1,
                     read.what = X[1, 1],
                     BM.type = t,
                     transpose = TRUE)

    expect_true(typeof(test) == t)

    expect_equal(test[], X[])
  }
})

################################################################################

dimnames(x) <- list(sample(letters, N, TRUE), sample(LETTERS, M, TRUE))

test_that("read from write.table with dimnames", {

  for (t in TEST.TYPES[-1]) {

    X <- big_copy(x, type = t)

    write.table(X[], tmp, quote = FALSE)

    # with transpose
    test <- big_read(tmp,
                     file.nheader = 1,
                     info.nelem = 1,
                     read.what = X[1, 1],
                     BM.type = t,
                     transpose = TRUE)

    expect_true(typeof(test) == t)

    colnames <- attr(test, "info")[1, ]
    rownames <- strsplit(attr(test, "header"),
                         split = " ",
                         fixed = TRUE)[[1]]

    expect_equal(test[], X[])

    # without transpose
    test2 <- big_read(tmp,
                      file.nheader = 1,
                      info.nelem = 1,
                      read.what = X[1, 1],
                      BM.type = t)

    expect_true(typeof(test2) == t)

    colnames2 <- attr(test2, "info")[1, ]
    rownames2 <- strsplit(attr(test2, "header"),
                          split = " ",
                          fixed = TRUE)[[1]]

    expect_equal(t(test2[]), X[])
  }
})

################################################################################
