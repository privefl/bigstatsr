################################################################################

context("FBM_ACCESSORS")

set.seed(SEED)

################################################################################

test_extract <- function(call) {
  call <- deparse(substitute(call))
  eval(parse(text = sprintf("expect_equal(%s, %s)",
                            sub("^mat", "X", call),
                            sub("^mat", "x", call))), parent.frame())
}

test_replace <- function(call) {

  call <- deparse(substitute(call))

  assign <- sprintf("%s <- %s",
                    sub("^mat", "X", call),
                    sub("^mat", "x", call))
  eval(parse(text = assign), parent.frame())
  eval(parse(text = "expect_equal(X[], x)"), parent.frame())

  assign <- sprintf("%s <- as.vector(%s)",
                    sub("^mat", "X", call),
                    sub("^mat", "x", call))
  eval(parse(text = assign), parent.frame())
  eval(parse(text = "expect_equal(X[], x)"), parent.frame())
}

x0 <- matrix(rnorm(256, mean = 100, sd = 10), 16)

################################################################################

for (t in c(TEST.TYPES, "FBM.code256")) {

  if (t == "FBM.code256") {
    X <- big_copy(x0, type = "raw")
    X[] <- as.raw(0:255)
    X <- add_code256(X, code = as.vector(x0))
    expect_s4_class(X, "FBM")
    expect_s4_class(X, "FBM.code256")

    x <- x0

    TEST.ACCS <- c(test_extract)
  } else {
    X <- big_copy(x0, type = t)
    expect_s4_class(X, "FBM")

    x <- X[]

    TEST.ACCS <- c(test_extract, test_replace)
  }

  test_that("same dimensions", {
    expect_equal(nrow(X), nrow(x))
    expect_equal(ncol(X), ncol(x))
    expect_equal(dim(X), dim(x))
    expect_equal(length(X), length(x))
  })

  test_that("same accessing", {

    for (test_acc in TEST.ACCS) {

      test_acc(mat[])
      test_acc(mat[, , drop = FALSE])
      test_acc(mat[, , drop = TRUE])
      test_acc(mat[1, ])
      test_acc(mat[1, , drop = FALSE])
      test_acc(mat[1, , drop = TRUE])
      test_acc(mat[cbind(1:5, 1:5)])

      for (ind in list(1:5, -(1:5), c(TRUE, FALSE, TRUE))) {

        test_acc(mat[ind, ])
        test_acc(mat[ind, , drop = FALSE])
        test_acc(mat[ind, , drop = TRUE])
        test_acc(mat[, 1])
        test_acc(mat[, 1, drop = FALSE])
        test_acc(mat[, 1, drop = TRUE])
        test_acc(mat[1, 1])
        test_acc(mat[1, 1, drop = FALSE])
        test_acc(mat[1, 1, drop = TRUE])
        test_acc(mat[ind, 1])
        test_acc(mat[ind, 1, drop = FALSE])
        test_acc(mat[ind, 1, drop = TRUE])
        test_acc(mat[, ind])
        test_acc(mat[, ind, drop = FALSE])
        test_acc(mat[, ind, drop = TRUE])
        test_acc(mat[1, ind])
        test_acc(mat[1, ind, drop = FALSE])
        test_acc(mat[1, ind, drop = TRUE])
        test_acc(mat[ind, ind])
        test_acc(mat[ind, ind, drop = FALSE])
        test_acc(mat[ind, ind, drop = TRUE])
      }

    }
  })
}

################################################################################
