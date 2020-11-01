################################################################################

context("PARTIAL_COR")

################################################################################

test_that("pcor() works", {

  iris <- datasets::iris
  test <- pcor(iris$Sepal.Length, iris$Sepal.Width, NULL)
  # (r <- cor(iris$Sepal.Length, iris$Sepal.Width))
  # dput(c(r, psychometric::CIr(r, nrow(iris))))
  expect_equal(test, c(-0.117569784, -0.272693248, 0.04351158))

  test2 <- sapply(2:4, function(k) pcor(iris[[1]], iris[[k]], iris[-c(1, k)]))
  # dput(unname(ppcor::pcor(model.matrix(~ ., iris)[, -1])$estimate[1, 2:4]))
  expect_equal(test2[1, ], c(0.43282058, 0.71005994, -0.1711388825))

  iris[1:5, ] <- NA
  test3 <- sapply(2:4, function(k) pcor(iris[[1]], iris[[k]], iris[-c(1, k)]))
  # dput(unname(ppcor::pcor(model.matrix(~ ., iris)[, -1])$estimate[1, 2:4]))
  expect_equal(test3[1, ], c(0.42700984, 0.71191098, -0.1717036666))
})

################################################################################
