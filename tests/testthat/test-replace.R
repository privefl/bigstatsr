# ################################################################################
#
# context("BM_REPLACE")
#
# test_manipBoth <- function(call) {
#   call <- deparse(substitute(call))
#   assign <- sprintf("%s <- %s", sub("^mat", "X", call), sub("^mat", "x", call))
#   eval(parse(text = assign), parent.frame())
#   eval(parse(text = "expect_equal(X[], x)"), parent.frame())
# }
#
# x0 <- matrix(rnorm(256, mean = 100, sd = 10), 16)
#
# ################################################################################
#
# for (t in TEST.TYPES) {
#   X <- big_copy(x0, type = t)
#   expect_s4_class(X, "FBM")
#   x <- X[]
#   expect_true(is.matrix(x))
#
#   test_that("same dimensions", {
#     expect_equal(nrow(X), nrow(x))
#     expect_equal(ncol(X), ncol(x))
#     expect_equal(dim(X), dim(x))
#     expect_equal(length(X), length(x))
#   })
#
#   test_that("same accessing", {
#
#     test_manipBoth(mat[, 1])
#     expect_equal(X[, 1], X[, 2])
#
#     test_manipBoth(mat[1, 1] <- x[2, 3])
#
#     # logicals
#     test_manipBoth(mat[] <- x[1:5, ])
#
#     # matrix
#     test_manipBoth(mat[cbind(1:16, 1:16)] <- x[, 3])
#     expect_error(X[cbind(TRUE, FALSE)] <- 2)
#
#     expect_equal(X[], x[])
#     expect_equal(X[], x[])
#     expect_equal(X[, , drop = FALSE], x[, , drop = FALSE])
#     expect_equal(X[, , drop = TRUE], x[, , drop = TRUE])
#     expect_equal(X[1, ], x[1, ])
#     expect_equal(X[1, , drop = FALSE], x[1, , drop = FALSE])
#     expect_equal(X[1, , drop = TRUE], x[1, , drop = TRUE])
#     expect_equal(X[cbind(1:5, 1:5)], x[cbind(1:5, 1:5)])
#
#     for (ind in list(1:5, -(1:5), c(TRUE, FALSE, TRUE))) {
#
#       expect_equal(X[ind, ], x[ind, ])
#       expect_equal(X[ind, , drop = FALSE], x[ind, , drop = FALSE])
#       expect_equal(X[ind, , drop = TRUE], x[ind, , drop = TRUE])
#       expect_equal(X[, 1], x[, 1])
#       expect_equal(X[, 1, drop = FALSE], x[, 1, drop = FALSE])
#       expect_equal(X[, 1, drop = TRUE], x[, 1, drop = TRUE])
#       expect_equal(X[1, 1], x[1, 1])
#       expect_equal(X[1, 1, drop = FALSE], x[1, 1, drop = FALSE])
#       expect_equal(X[1, 1, drop = TRUE], x[1, 1, drop = TRUE])
#       expect_equal(X[ind, 1], x[ind, 1])
#       expect_equal(X[ind, 1, drop = FALSE], x[ind, 1, drop = FALSE])
#       expect_equal(X[ind, 1, drop = TRUE], x[ind, 1, drop = TRUE])
#       expect_equal(X[, ind], x[, ind])
#       expect_equal(X[, ind, drop = FALSE], x[, ind, drop = FALSE])
#       expect_equal(X[, ind, drop = TRUE], x[, ind, drop = TRUE])
#       expect_equal(X[1, ind], x[1, ind])
#       expect_equal(X[1, ind, drop = FALSE], x[1, ind, drop = FALSE])
#       expect_equal(X[1, ind, drop = TRUE], x[1, ind, drop = TRUE])
#       expect_equal(X[ind, ind], x[ind, ind])
#       expect_equal(X[ind, ind, drop = FALSE], x[ind, ind, drop = FALSE])
#       expect_equal(X[ind, ind, drop = TRUE], x[ind, ind, drop = TRUE])
#     }
#   })
# }
#
# ################################################################################
