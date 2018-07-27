# ################################################################################
#
# context("READ_BM")
#
# set.seed(SEED)
#
# ################################################################################
#
# M <- 500
#
# csv <- tempfile(fileext = ".csv")
#
# SEPS <- c(" ", ",", ";", "|", "\t")
#
# ################################################################################
#
# test_that("read without dimnames", {
#
#   header <- FALSE
#
#   for (t in c("integer", "double")) {
#
#     N <- nrow(mtcars)
#     x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
#     X <- big_copy(x, type = t)
#
#     for (sep in SEPS) {
#
#       data.table::fwrite(cbind.data.frame(mtcars, X[]), csv,
#                          quote = FALSE, sep = sep, col.names = header)
#
#       skip <- sample(12:M, 50)
#       tmp <- tempfile()
#       save <- sample(c(TRUE, FALSE), 1)
#       expect_error(big_read(csv, sep = sep, header = header))
#       test <- big_read(csv, sep = sep, header = header, nlines = N,
#                        ind.meta = 1:11, ind.skip = skip,
#                        confirmed = TRUE, verbose = FALSE,
#                        backingfile = tmp, save = save)
#
#       expect_true(typeof(test$FBM) == t)
#       expect_equal(test$FBM$backingfile, normalizePath(paste0(tmp, ".bk")))
#       expect_true(file.exists(paste0(tmp, ".rds")) == save)
#       expect_equal(test$FBM[], X[, -(skip - 11)])
#       expect_identical(test$colnames, NULL)
#       expect_equal(test$meta, mtcars, check.attributes = FALSE)
#
#     }
#
#   }
# })
#
# ################################################################################
#
# test_that("read with dimnames", {
#
#   header <- TRUE
#
#   for (t in c("integer", "double")) {
#
#     N <- nrow(iris)
#     x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
#     iris$Species <- as.character(iris$Species)
#     X <- big_copy(x, type = t)
#
#     for (sep in SEPS) {
#
#       data.table::fwrite(cbind.data.frame(iris, X[]), csv,
#                          quote = FALSE, sep = sep, col.names = header)
#
#       test <- big_read(csv, sep = sep, header = header, nlines = N + 1,
#                        ind.meta = 1:5, ind.skip = 5:50, nlines.block = 40,
#                        confirmed = TRUE, verbose = FALSE)
#
#       expect_true(typeof(test$FBM) == t)
#       expect_equal(test$FBM[], X[, -(1:45)])
#       expect_identical(test$colnames, paste(46:M))
#       expect_identical(test$meta, iris[-5])
#
#     }
#
#   }
# })
#
# ################################################################################
