################################################################################

context("READ")

set.seed(SEED)

################################################################################

M <- 500

csv <- tempfile(fileext = ".csv")

SEPS <- c(" ", ",", ";", "|", "\t")

################################################################################

test_that("read with or without dimnames", {

  for (header in c(FALSE, TRUE)) {

    for (t in c("integer", "double")) {

      N <- nrow(mtcars)
      x <- matrix(rnorm(N * M, mean = 100, sd = 5), N, M)
      X <- big_copy(x, type = t)

      for (sep in SEPS) {

        bigreadr::fwrite2(cbind.data.frame(mtcars, X[]), csv,
                          sep = sep, col.names = header)

        expect_error(big_read(csv, select = rev(seq_len(M))),
                     "Argument 'select' should be sorted.", fixed = TRUE)


        skip <- sample(0:2, 1)
        skip2 <- max(0, skip - header)
        ind_no_skip <- setdiff(seq_len(N), seq_len(skip2))
        header2 <- header && (skip == 0)

        tmp <- tempfile()
        test <- big_read(csv, select = 5:50, type = t, skip = skip,
                         backingfile = tmp, progress = FALSE)

        expect_true(typeof(test) == t)
        expect_equal(test$backingfile, normalizePath(paste0(tmp, ".bk")))
        expect_true(file.exists(paste0(tmp, ".rds")))
        mat <- as.matrix(mtcars); storage.mode(mat) <- t
        expect_equal(test[, 1:7], mat[ind_no_skip, 5:11],
                     check.attributes = FALSE)
        expect_equal(test[, 8:46], X[ind_no_skip, 1:39])
        expect_identical(
          attr(test, "fbm_names"),
          `if`(header2, c(names(mtcars[5:11]), 1:39), paste0("V", 5:50)))
      }

    }

  }
})

################################################################################

test_that("read with filtering", {

    for (t in c("integer", "double")) {

      N <- nrow(mtcars)
      x <- matrix(rnorm(N * M, mean = 100, sd = 5), N, M)
      X <- big_copy(x, type = t)

      filter0 <- (mtcars$cyl == 4)

      for (sep in SEPS) {

        filter <- sample(list(filter0, which(filter0), -which(!filter0)))[[1]]

        bigreadr::fwrite2(cbind.data.frame(mtcars, X[]), csv,
                          sep = sep, col.names = TRUE)

        tmp <- tempfile()
        test <- big_read(csv, select = 5:50, type = t,
                         backingfile = tmp, progress = FALSE, filter = filter)

        expect_true(typeof(test) == t)
        expect_equal(test$backingfile, normalizePath(paste0(tmp, ".bk")))
        expect_true(file.exists(paste0(tmp, ".rds")))
        mat <- as.matrix(mtcars); storage.mode(mat) <- t
        expect_equal(test[, 1:7], mat[filter, 5:11],
                     check.attributes = FALSE)
        expect_equal(test[, 8:46], X[filter, 1:39])
        expect_identical(attr(test, "fbm_names"), c(names(mtcars[5:11]), 1:39))
      }

    }

})

################################################################################

test_that("big_write() works", {

  X <- big_attachExtdata()

  for (sep in SEPS) {

    for (nrows in c(100, Inf)) {

      csv <- big_write(X, tempfile(), every_nrow = nrows, sep = sep)
      test <- bigreadr::fread2(csv)
      expect_identical(substr(readLines(csv, 1), 0, 3), sprintf("2%s2", sep))
      expect_equal(bigreadr::nlines(csv), nrow(X))
      expect_equal(dim(test), dim(X))
      expect_equal(test, as.data.frame(X[]))

      ind.row <- sample(rows_along(X), 100)
      ind.col <- sample(cols_along(X), 200)
      csv <- big_write(X, tempfile(), every_nrow = nrows, sep = sep,
                       ind.row = ind.row, ind.col = ind.col)
      test <- bigreadr::fread2(csv)
      expect_equal(bigreadr::nlines(csv), length(ind.row))
      expect_equal(dim(test), dim(X2 <- X[ind.row, ind.col]))
      expect_equal(test, as.data.frame(X2))
    }
  }
})

################################################################################
