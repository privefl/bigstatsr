################################################################################

context("READ_BM")

set.seed(SEED)

################################################################################

# Simulating some data
N <- nrow(iris)
M <- 500
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
iris$Species <- as.character(iris$Species)
csv <- tempfile(fileext = ".csv")

SEPS <- c(" ", ",", ";", "|", "\t")

################################################################################

test_that("read without dimnames", {

  header <- FALSE

  for (t in c("integer", "double")) {

    X <- big_copy(x, type = t)

    for (sep in SEPS) {

      data.table::fwrite(cbind.data.frame(iris, X[]), csv,
                         quote = FALSE, sep = sep, col.names = header)

      skip <- sample(6:M, 50)
      tmp <- tempfile()
      save <- sample(c(TRUE, FALSE), 1)
      test <- big_read(csv, sep = sep, header = header,
                       ind.meta = 1:5, ind.skip = skip,
                       confirmed = TRUE, verbose = FALSE,
                       backingfile = tmp, save = save)

      expect_true(typeof(test$FBM) == t)
      expect_equal(test$FBM$backingfile, normalizePath(paste0(tmp, ".bk")))
      expect_true(file.exists(paste0(tmp, ".rds")) == save)
      expect_equal(test$FBM[], X[, -(skip - 5)])
      expect_identical(test$colnames, NULL)
      expect_equal(test$meta, iris, check.attributes = FALSE)

    }

  }
})

################################################################################

test_that("read with dimnames", {

  header <- TRUE

  for (t in c("integer", "double")) {

    X <- big_copy(x, type = t)

    for (sep in SEPS) {

      data.table::fwrite(cbind.data.frame(iris, X[]), csv,
                         quote = FALSE, sep = sep, col.names = header)

      test <- big_read(csv, sep = sep, header = header,
                       ind.meta = 1:5, ind.skip = 5:50, nlines.block = 40,
                       confirmed = TRUE, verbose = FALSE)

      expect_true(typeof(test$FBM) == t)
      expect_equal(test$FBM[], X[, -(1:45)])
      expect_identical(test$colnames, paste(46:M))
      expect_identical(test$meta, iris[-5])

    }

  }
})

################################################################################
