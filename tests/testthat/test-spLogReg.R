################################################################################

context("SP_LOG_REG")

set.seed(SEED)

# options(bigstatsr.downcast.warning = FALSE)

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
  y0 <- s + rnorm(N) / 10
  y <- as.numeric(y0 > 0)
  y2 <- y; y2[] <- 0
  y3 <- y; y3[10] <- NA

  covar0 <- matrix(rnorm(N * 3), N)
  lcovar <- list(NULL, covar0)

  ################################################################################

  test_that("can be used with a subset of samples", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x, TRUE), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        expect_error(big_spLogReg(X, y0, covar.train = covar, ncores = test_cores()),
                     "'y01.train' should be composed of 0s and 1s.", fixed = TRUE)
        expect_error(big_spLogReg(X, y2, covar.train = covar, ncores = test_cores()),
                     "'y01.train' should be composed of 0s and 1s.", fixed = TRUE)
        expect_error(big_spLogReg(X, y3, covar.train = covar, ncores = test_cores()),
                     "'y01.train' should be composed of 0s and 1s.", fixed = TRUE)

        ind <- sample(N, N / 2)

        alphas <- c(runif(1, min = 0.1, max = 1), 1)

        mod.bigstatsr <- big_spLogReg(X, y, covar.train = covar, alphas = alphas,
                                      dfmax = Inf, nlam.min = Inf, warn = FALSE,
                                      ncores = test_cores())
        lapply(mod.bigstatsr, function(mod) lapply(mod, function(fold) {
          if (fold$message != "Model saturated") expect_length(fold$lambda, 200)
        }))

        if (is.null(covar)) {
          expect_length(predict(mod.bigstatsr[2], X, ind.row = (1:N)[-ind],
                                ncores = test_cores()),
                        N / 2)
        } else {
          expect_error(predict(mod.bigstatsr[2], X, ind.row = (1:N)[-ind]),
                       "You forgot to provide 'covar.row' in predict().")
        }
        preds <- predict(mod.bigstatsr, X, ind.row = (1:N)[-ind],
                         covar.row = covar[-ind, ], ncores = test_cores())
        expect_gt(AUC(preds, y[-ind]), 0.85)

        expect_s3_class(plot(mod.bigstatsr), "ggplot")

        mod.bigstatsr2 <- big_spLogReg(X, y[ind], ind.train = ind,
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ncores = test_cores(),
                                       warn = FALSE)
        preds2 <- predict(mod.bigstatsr2, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ], ncores = test_cores())
        expect_gt(AUC(preds2, y[-ind]), 0.7)

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

      X <- `if`(t == "raw", asFBMcode(x, TRUE), big_copy(x, type = t))

      ind.col <- cols_along(X)[-set]
      ind.novar <- sample(ind.col, 10); X[, ind.novar] <- 100
      set2 <- match(set, (1:M)[-ind.novar])

      for (covar in sample(lcovar, 1)) {

        ind <- sample(N, N / 2)
        alphas <- c(runif(1, min = 0.1, max = 1), 1)

        expect_warning(
          mod.bigstatsr3 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         ind.col = ind.col,
                                         covar.train = covar[ind, ],
                                         alphas = alphas,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )

        expect_equal(nrow(summary(mod.bigstatsr3)), length(alphas))
        preds3 <- predict(mod.bigstatsr3, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ], ncores = test_cores())
        # Test that prediction is bad when removing the explanatory variables
        expect_lt(AUC(preds3, y[-ind]), 0.7)

        # Test prediction with different penalizations of the explanatory variables
        expect_warning(
          mod.bigstatsr4 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alphas,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        expect_equal(length(attr(mod.bigstatsr4, "ind.col")), M - 10)
        expect_lte(length(mod.bigstatsr4[[c(1, 1)]]$beta), M - 10 + ncol(covar0))
        preds4 <- predict(mod.bigstatsr4, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ], ncores = test_cores())
        auc0 <- AUC(preds4, y[-ind])

        pf <- rep(1, ncol(X)); pf[set] <- 10
        expect_warning(
          mod.bigstatsr5 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alphas, pf.X = pf,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        preds5 <- predict(mod.bigstatsr5, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ], ncores = test_cores())
        expect_lt(AUC(preds5, y[-ind]), auc0)

        pf[set] <- 0
        expect_warning(
          mod.bigstatsr6 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alphas, pf.X = pf,
                                         ncores = test_cores(),
                                         warn = FALSE),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        lapply(unlist(mod.bigstatsr6, recursive = FALSE),
               function(mod) expect_true(all(mod$beta[set2] != 0)))
        preds6 <- predict(mod.bigstatsr6, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ], ncores = test_cores())
        expect_gt(AUC(preds6, y[-ind]), auc0)
      }
    }
  })

  ################################################################################

  test_that("parameter 'return.all' is deprecated and loss is correct", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x, TRUE), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        alpha <- runif(1, min = 0.01, max = 1)

        expect_warning(big_spLogReg(X, y, covar.train = covar, alphas = alpha,
                                    return.all = TRUE, warn = FALSE,
                                    ncores = test_cores()))

        mod.bigstatsr4 <- big_spLogReg(X, y, covar.train = covar, alphas = alpha,
                                       warn = FALSE, ncores = test_cores())

        expect_length(mod.bigstatsr4, 1)
        flatten <- mod.bigstatsr4[[1]]
        expect_true(all(sapply(flatten, class) == "big_sp"))

        lapply(flatten, function(obj) {
          ind.val <- setdiff(rows_along(X), obj$ind.train)
          y.val <- y[ind.val]
          preds <- predict(obj, X, ind.row = ind.val,
                           ind.col = attr(mod.bigstatsr4, "ind.col"),
                           covar.row = covar[ind.val, ])
          p <- 1 / (1 + exp(-preds))
          loss.val <- -mean(y.val * log(p) + (1 - y.val) * log(1 - p))  ## negLogLik
          diff <- abs(loss.val - obj$loss.val)
          expect_true(any(diff < 1e-8))
        })
      }
    }
  })

  ################################################################################

  test_that("Use a base predictor", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x, TRUE), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        ind <- sample(N, N / 2)
        alphas <- c(runif(1, min = 0.01, max = 1), 1)
        ind.sets <- sample(rep_len(1:10, length(ind)))

        mod.bigstatsr <- big_spLogReg(X, y[ind], ind.train = ind,
                                      covar.train = covar[ind, ],
                                      alphas = alphas,
                                      ind.sets = ind.sets,
                                      ncores = test_cores(),
                                      warn = FALSE)
        preds <- predict(mod.bigstatsr, X, covar.row = covar, ncores = test_cores())
        expect_gt(AUC(preds[-ind], y[-ind]), 0.7)

        mod.bigstatsr2 <- big_spLogReg(X, y[ind], ind.train = ind,
                                       base.train = rep(5, length(ind)),
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ind.sets = ind.sets,
                                       ncores = test_cores(),
                                       warn = FALSE)
        expect_equal(sapply(unlist(mod.bigstatsr2, recursive = FALSE),
                            function(x) x$intercept) + 5,
                     sapply(unlist(mod.bigstatsr, recursive = FALSE),
                            function(x) x$intercept))
        expect_equal(sapply(unlist(mod.bigstatsr2, recursive = FALSE),
                            function(x) x$beta.X),
                     sapply(unlist(mod.bigstatsr, recursive = FALSE),
                            function(x) x$beta.X))

        mod.bigstatsr3 <- big_spLogReg(X, y[ind], ind.train = ind,
                                       base.train = preds[ind] / 2,
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ind.sets = ind.sets,
                                       ncores = test_cores(),
                                       warn = FALSE)
        expect_error(predict(mod.bigstatsr3, X, covar.row = covar),
                     "You forgot to provide 'base.row' in predict().")
        preds3 <- predict(mod.bigstatsr3, X, covar.row = covar, base.row = rep(0, N),
                          ncores = test_cores())
        expect_gt(cor(preds3, preds), 0.9)
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
  y01 <- as.numeric((rowSums(X[, 1:10]) + 2 * rnorm(N)) > 0)
  covar <- matrix(rnorm(N * 3), N)

  ind.train <- sort(sample(nrow(X), 150))
  ind.test <- setdiff(rows_along(X), ind.train)

  # fitting model for multiple lambdas and alphas
  ALPHAS <- c(1, 0.5, 0.1, 0.01)
  expect_warning(
    test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                         covar.train = covar[ind.train, ],
                         alphas = ALPHAS, K = 4),
    "Some models may not have reached a minimum", fixed = TRUE
  )

  # peek at the models
  test_summary <- summary(test)
  expect_identical(test_summary$alpha, sort(ALPHAS))
  expect_identical(test_summary$all_conv, c(FALSE, FALSE, TRUE, TRUE))
  test_summary2 <- summary(test, sort = TRUE)
  expect_identical(test_summary2$alpha, ALPHAS)
  expect_identical(test_summary2$all_conv, c(TRUE, TRUE, FALSE, FALSE))
  expect_false(is.unsorted(test_summary2$validation_loss))
})

################################################################################

test_that("code is used for FBM.code256", {

  # simulating some data
  N <- 230
  M <- 730
  X <- FBM.code256(N, M, init = round(100 + rnorm(N * M, sd = 5)), code = rnorm(256))
  y01 <- as.numeric((rowSums(X[, 1:10]) + rnorm(N)) > 0)
  covar <- matrix(rnorm(N * 3), N)

  test <- big_spLogReg(X, y01, K = 5)

  X2 <- X$copy(code = -X$code256)
  test2 <- big_spLogReg(X2, y01, K = 5)
  expect_lt(print(cor(summary(test)$beta[[1]], summary(test2)$beta[[1]])), -0.7)
})

################################################################################

test_that("New power parameters work", {

  skip_if_not(not_cran)
  set.seed(1)

  # simulating some data
  N <- 230
  M <- 730
  X <- FBM(N, M, init = rnorm(N * M, sd = 5))
  y01 <- as.numeric((rowSums(X[, 1:10]) + 2 * rnorm(N)) > 0)
  covar <- matrix(rnorm(N * 3), N)

  ind.train <- sort(sample(nrow(X), 150))
  ind.test <- setdiff(rows_along(X), ind.train)

  # fitting model for multiple lambdas and alphas
  ALPHAS <- c(1, 0.5, 0.1, 0.01)
  expect_warning(
    test <- big_spLogReg(X, y01[ind.train], ind.train = ind.train,
                         covar.train = covar[ind.train, ], K = 4,
                         alphas = ALPHAS,
                         power_scale = c(0, 0.5, 1),
                         power_adaptive = c(0, 0.5, 1.5)),
    "Some models may not have reached a minimum", fixed = TRUE
  )

  # peek at the models
  test_summary <- summary(test)
  expect_identical(test_summary$alpha, rep(sort(ALPHAS), each = 9))

  library(dplyr)
  nb_var <- test_summary %>%
    group_by(alpha, power_adaptive) %>%
    summarise(nb_var = mean(nb_var)) %>%
    ungroup()
  nb_var %>%
    group_by(alpha) %>%
    summarise(cor = cor(power_adaptive, nb_var, method = "spearman")) %>%
    pull(cor) %>%
    expect_equal(rep(-1, 4))
  nb_var %>%
    group_by(power_adaptive) %>%
    summarise(cor = cor(alpha, nb_var, method = "spearman")) %>%
    pull(cor) %>%
    expect_equal(rep(-1, 3))
})

################################################################################
