################################################################################

context("FBM_REPLACE_DF")

opt.save <- options(bigstatsr.downcast.warning = FALSE)

################################################################################

test_replace <- function(call) {

  call <- deparse(substitute(call))

  assign <- sprintf("%s <- %s",
                    sub("^mat", "X", call),
                    sub("^mat", "x", call))
  eval(parse(text = assign), parent.frame())
  check <- "expect_equal(X[], transf(x), check.attributes = FALSE)"
  eval(parse(text = check), parent.frame())
}

################################################################################

test_that("Replace works with data frames", {

  x <- datasets::airquality
  x[] <- lapply(x, function(x_j) ifelse(is.na(x_j) | x_j >= 256, 0L, x_j))

  transf <- function(df) {
    mat <- as.matrix(df)
    if (t != "double") storage.mode(mat) <- "integer"
    mat
  }

  for (t in TEST.TYPES) {

    X <- FBM(nrow(x), ncol(x), type = t)

    expect_error(X[] <- x[-1], "dimension of")

    test_replace(mat[])
    test_replace(mat[, , drop = FALSE])
    test_replace(mat[, , drop = TRUE])
    test_replace(mat[1, ])
    test_replace(mat[1, , drop = FALSE])
    test_replace(mat[1, , drop = TRUE])
    test_replace(mat[cbind(1:5, 1:5)])

    for (ind in list(1:5, -(1:5), c(TRUE, FALSE, TRUE))) {

      test_replace(mat[ind, ])
      test_replace(mat[ind, , drop = FALSE])
      test_replace(mat[ind, , drop = TRUE])
      test_replace(mat[, 1])
      test_replace(mat[, 1, drop = FALSE])
      test_replace(mat[, 1, drop = TRUE])
      test_replace(mat[1, 1])
      test_replace(mat[1, 1, drop = FALSE])
      test_replace(mat[1, 1, drop = TRUE])
      test_replace(mat[ind, 1])
      test_replace(mat[ind, 1, drop = FALSE])
      test_replace(mat[ind, 1, drop = TRUE])
      test_replace(mat[, ind])
      test_replace(mat[, ind, drop = FALSE])
      test_replace(mat[, ind, drop = TRUE])
      test_replace(mat[1, ind])
      test_replace(mat[1, ind, drop = FALSE])
      test_replace(mat[1, ind, drop = TRUE])
      test_replace(mat[ind, ind])
      test_replace(mat[ind, ind, drop = FALSE])
      test_replace(mat[ind, ind, drop = TRUE])
    }
  }
})

################################################################################

test_that("Some types won't work", {

  iris <- datasets::iris

  iris$Species <- as.character(iris$Species)
  expect_error(big_copy(iris, type = "double"),
               "R type 'character' is not supported.")

  iris[[5]] <- list(NULL)
  expect_error(big_copy(iris, type = "double"),
               "R type 'list' is not supported.")
})

################################################################################

options(opt.save)

################################################################################
