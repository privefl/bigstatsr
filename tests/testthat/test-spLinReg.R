################################################################################

context("SP_LIN_REG")

set.seed(SEED)

################################################################################

if (not_cran) {

  # Simulating some data
  N <- 934
  M <- 1053
  m <- 30
  x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)
  set <- sample(M, size = m)
  eff <- rnorm(m)
  s <- drop(scale(x[, set]) %*% eff) / m
  y <- s + rnorm(N) / 10
  y2 <- (y > 0)
  y3 <- y; y3[] <- 0
  y4 <- y; y4[10] <- NA

  covar0 <- matrix(rnorm(N * 3), N)
  lcovar <- list(NULL, covar0)

  ################################################################################

  test_that("can be used with a subset of samples", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        expect_warning(big_spLinReg(X, y2, covar.train = covar,
                                    ncores = test_cores(), warn = FALSE),
                       "'y.train' is composed of only two different levels.", fixed = TRUE)
        expect_error(big_spLinReg(X, y3, covar.train = covar, ncores = test_cores()),
                     "'y.train' should be composed of different values.", fixed = TRUE)
        expect_error(big_spLinReg(X, y4, covar.train = covar, ncores = test_cores()),
                     "You can't have missing values in 'y.train'.", fixed = TRUE)

        ind <- sample(N, N / 2)
        alphas <- c(runif(1, min = 0.01, max = 1), 1)

        mod.bigstatsr <- big_spLinReg(X, y, covar.train = covar, alphas = alphas,
                                      dfmax = Inf, n.abort = Inf, warn = FALSE,
                                      ncores = test_cores())
        lapply(mod.bigstatsr, function(mod) lapply(mod, function(fold) {
          expect_length(fold$lambda, 200); NULL
        }))

        if (is.null(covar)) {
          expect_length(predict(mod.bigstatsr[2], X, ind.row = (1:N)[-ind]), N / 2)
        } else {
          expect_error(predict(mod.bigstatsr[2], X, ind.row = (1:N)[-ind]),
                       "You forgot to provide 'covar.row' in predict().")
        }
        preds <- predict(mod.bigstatsr, X, ind.row = (1:N)[-ind],
                         covar.row = covar[-ind, ])
        expect_gt(cor(preds, y[-ind]), 0.75)

        expect_s3_class(plot(mod.bigstatsr), "ggplot")

        mod.bigstatsr2 <- big_spLinReg(X, y[ind], ind.train = ind,
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ncores = test_cores(),
                                       warn = FALSE)
        preds2 <- predict(mod.bigstatsr2, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        expect_gt(cor(preds2, y[-ind]), 0.6)

        expect_error(predict(mod.bigstatsr2, X, covar.row = covar, abc = 2),
                     "Argument 'abc' not used.")

        flatten <- unlist(mod.bigstatsr2, recursive = FALSE)
        expect_true(all(sapply(flatten, class) == "big_sp"))
        lapply(flatten, function(obj) {
          expect_false(is.null(nb_candidate <- obj$nb_candidate))
          expect_false(is.unsorted(nb_candidate))
          expect_true(all(nb_candidate >= obj$nb_active))
        })
      }
    }
  })

  ################################################################################

  test_that("can be used with a subset of variables (and penalty factors work)", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

      ind.col <- cols_along(X)[-set]
      ind.novar <- sample(ind.col, 10); X[, ind.novar] <- 100
      set2 <- match(set, (1:M)[-ind.novar])

      for (covar in sample(lcovar, 1)) {

        ind <- sample(N, N / 2)
        alpha <- runif(1, min = 0.1, max = 1)

        # Test that prediction is bad when removing the explanatory variables
        expect_warning(
          mod.bigstatsr3 <- big_spLinReg(X, y[ind], ind.train = ind,
                                         ind.col = ind.col,
                                         covar.train = covar[ind, ],
                                         alphas = alpha,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        preds3 <- predict(mod.bigstatsr3, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        if (any(diff(preds3) != 0)) expect_lt(cor(preds3, y[-ind]), 0.2)

        # Test prediction with different penalizations of the explanatory variables
        expect_warning(
          mod.bigstatsr4 <- big_spLinReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alpha,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        expect_equal(length(attr(mod.bigstatsr4, "ind.col")), M - 10)
        expect_lte(length(mod.bigstatsr4[[c(1, 1)]]$beta), M - 10 + ncol(covar0))
        preds4 <- predict(mod.bigstatsr4, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        cor0 <- cor(preds4, y[-ind])

        pf <- rep(1, ncol(X)); pf[set] <- 10
        expect_warning(
          mod.bigstatsr5 <- big_spLinReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alpha, pf.X = pf,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        preds5 <- predict(mod.bigstatsr5, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])

        pf[set] <- 0
        expect_warning(
          mod.bigstatsr6 <- big_spLinReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alpha, pf.X = pf,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        lapply(unlist(mod.bigstatsr6, recursive = FALSE),
               function(mod) expect_true(all(mod$beta[set2] != 0)))
        preds6 <- predict(mod.bigstatsr6, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])

        expect_gt(cor(preds6, y[-ind]), cor0)
        if (any(diff(preds5) != 0)) expect_lt(cor(preds5, y[-ind]), cor0)
      }
    }
  })

  ################################################################################

  test_that("parameter 'return.all' is deprecated and loss is correct", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        alpha <- runif(1, min = 0.01, max = 1)

        expect_warning(
          big_spLinReg(X, y, covar.train = covar, alphas = alpha,
                       return.all = TRUE, warn = FALSE, ncores = test_cores()))

        mod.bigstatsr4 <- big_spLinReg(X, y, covar.train = covar, alphas = alpha,
                                       ncores = test_cores(), warn = FALSE)

        expect_length(mod.bigstatsr4, 1)
        flatten <- mod.bigstatsr4[[1]]
        expect_true(all(sapply(flatten, class) == "big_sp"))

        lapply(flatten, function(obj) {
          ind.val <- setdiff(rows_along(X), obj$ind.train)
          y.val <- y[ind.val]
          preds <- predict(obj, X, ind.row = ind.val,
                           ind.col = attr(mod.bigstatsr4, "ind.col"),
                           covar.row = covar[ind.val, ])
          loss.val <- mean((y.val - preds)^2)  ## MSE
          diff <- abs(loss.val - obj$loss.val)
          expect_true(any(diff < 1e-8))
        })
      }
    }
  })

  ################################################################################

  test_that("Use a base predictor", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        ind <- sample(N, N / 2)
        alphas <- c(runif(1, min = 0.01, max = 1), 1)
        ind.sets <- sample(rep_len(1:10, length(ind)))

        mod.bigstatsr <- big_spLinReg(X, y[ind], ind.train = ind,
                                      covar.train = covar[ind, ],
                                      alphas = alphas,
                                      ind.sets = ind.sets,
                                      ncores = test_cores(),
                                      warn = FALSE)
        preds <- predict(mod.bigstatsr, X, covar.row = covar)
        expect_gt(cor(preds[-ind], y[-ind]), 0.6)

        expect_equal(nrow(summary(mod.bigstatsr)), length(alphas))

        mod.bigstatsr2 <- big_spLinReg(X, y[ind], ind.train = ind,
                                       base.train = rep(10, length(ind)),
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ind.sets = ind.sets,
                                       ncores = test_cores(),
                                       warn = FALSE)
        expect_equal(sapply(unlist(mod.bigstatsr2, recursive = FALSE),
                            function(x) x$intercept) + 10,
                     sapply(unlist(mod.bigstatsr, recursive = FALSE),
                            function(x) x$intercept))
        expect_equal(sapply(unlist(mod.bigstatsr2, recursive = FALSE),
                            function(x) x$beta.X),
                     sapply(unlist(mod.bigstatsr, recursive = FALSE),
                            function(x) x$beta.X))

        mod.bigstatsr3 <- big_spLinReg(X, y[ind], ind.train = ind,
                                       base.train = preds[ind] / 2,
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ind.sets = ind.sets,
                                       ncores = test_cores(),
                                       warn = FALSE)
        expect_error(predict(mod.bigstatsr3, X, covar.row = covar),
                     "You forgot to provide 'base.row' in predict().")
        preds3 <- predict(mod.bigstatsr3, X, covar.row = covar, base.row = rep(0, N))
        expect_equal(preds3, preds / 2, tolerance = 0.1)
      }
    }
  })

}

################################################################################

test_that("Warns if not all converged", {
  skip_if_not(not_cran)
  set.seed(1)

  # simulating some data
  N <- 230
  M <- 730
  X <- FBM(N, M, init = rnorm(N * M, sd = 5))
  y <- rowSums(X[, 1:10]) + rnorm(N)
  covar <- matrix(rnorm(N * 3), N)

  ind.train <- sort(sample(nrow(X), 150))
  ind.test <- setdiff(rows_along(X), ind.train)

  # fitting model for multiple lambdas and alphas
  ALPHAS <- c(1, 0.5, 0.1, 0.01)
  expect_warning(
    test <- big_spLinReg(X, y[ind.train], ind.train = ind.train,
                         covar.train = covar[ind.train, ],
                         alphas = ALPHAS, K = 5),
    "Some models may not have reached a minimum", fixed = TRUE
  )

  # peek at the models
  test_summary <- summary(test)
  expect_identical(test_summary$alpha, sort(ALPHAS))
  expect_identical(test_summary$all_conv, c(FALSE, FALSE, FALSE, TRUE))
  test_summary2 <- summary(test, sort = TRUE)
  expect_identical(test_summary2$alpha, ALPHAS)
  expect_identical(test_summary2$all_conv, c(TRUE, FALSE, FALSE, FALSE))
  expect_false(is.unsorted(test_summary2$validation_loss))
})

################################################################################
