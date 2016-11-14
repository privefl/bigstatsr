################################################################################

context("MULT")

# Simulating some data
N <- 100
M <- 50
x <- matrix(rnorm(N*M), N, M)
mat <- matrix(rnorm(M*N), M, N)
vec.center <- rnorm(M)
vec.scale <- rnorm(M)

################################################################################

test_that("equality with %*%", {
  expect_equal(multScaled(x, 10, mat, vec.center, vec.scale),
               sweep(sweep(x, 2, vec.center, '-'), 2, vec.scale, '/') %*% mat)
})

################################################################################
