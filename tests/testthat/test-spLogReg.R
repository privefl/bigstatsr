################################################################################

context("SP_LOG_REG")

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
  y0 <- s + rnorm(N) / 10
  y <- as.numeric(y0 > 0)
  y2 <- y; y2[] <- 0
  y3 <- y; y3[10] <- NA

  covar0 <- matrix(rnorm(N * 3), N)
  lcovar <- list(NULL, covar0)

  ################################################################################

  test_that("can be used with a subset of samples", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

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
                                      dfmax = Inf, nlam.min = Inf,
                                      ncores = test_cores())
        lapply(mod.bigstatsr, function(mod) lapply(mod, function(fold) {
          if (fold$message != "Model saturated") expect_length(fold$lambda, 200)
        }))

        if (is.null(covar)) {
          expect_length(predict(mod.bigstatsr[2], X, ind.row = (1:N)[-ind]), N / 2)
        } else {
          expect_error(predict(mod.bigstatsr[2], X, ind.row = (1:N)[-ind]),
                       "You forgot to provide 'covar.row' in predict().")
        }
        preds <- predict(mod.bigstatsr, X, ind.row = (1:N)[-ind],
                         covar.row = covar[-ind, ])
        expect_gt(AUC(preds, y[-ind]), 0.85)

        expect_s3_class(plot(mod.bigstatsr), "ggplot")

        mod.bigstatsr2 <- big_spLogReg(X, y[ind], ind.train = ind,
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ncores = test_cores())
        preds2 <- predict(mod.bigstatsr2, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
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

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

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
                                         ncores = test_cores()),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )

        expect_equal(nrow(summary(mod.bigstatsr3)), length(alphas))
        preds3 <- predict(mod.bigstatsr3, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        # Test that prediction is bad when removing the explanatory variables
        expect_lt(AUC(preds3, y[-ind]), 0.7)

        # Test prediction with different penalizations of the explanatory variables
        expect_warning(
          mod.bigstatsr4 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alphas,
                                         ncores = test_cores()),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        expect_equal(length(attr(mod.bigstatsr4, "ind.col")), M - 10)
        expect_lte(length(mod.bigstatsr4[[c(1, 1)]]$beta), M - 10 + ncol(covar0))
        preds4 <- predict(mod.bigstatsr4, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        auc0 <- AUC(preds4, y[-ind])

        pf <- rep(1, ncol(X)); pf[set] <- 10
        expect_warning(
          mod.bigstatsr5 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alphas, pf.X = pf,
                                         ncores = test_cores()),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        preds5 <- predict(mod.bigstatsr5, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        expect_lt(AUC(preds5, y[-ind]), auc0)

        pf[set] <- 0
        expect_warning(
          mod.bigstatsr6 <- big_spLogReg(X, y[ind], ind.train = ind,
                                         covar.train = covar[ind, ],
                                         alphas = alphas, pf.X = pf,
                                         ncores = test_cores()),
          "10 variables with low/no variation have been removed.", fixed = TRUE
        )
        lapply(unlist(mod.bigstatsr6, recursive = FALSE),
               function(mod) expect_true(all(mod$beta[set2] != 0)))
        preds6 <- predict(mod.bigstatsr6, X, ind.row = (1:N)[-ind],
                          covar.row = covar[-ind, ])
        expect_gt(AUC(preds6, y[-ind]), auc0)
      }
    }
  })

  ################################################################################

  test_that("parameter 'return.all' is deprecated and loss is correct", {

    for (t in TEST.TYPES) {

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        alpha <- runif(1, min = 0.01, max = 1)

        expect_warning(big_spLogReg(X, y, covar.train = covar, alphas = alpha,
                                    return.all = TRUE, ncores = test_cores()))

        mod.bigstatsr4 <- big_spLogReg(X, y, covar.train = covar, alphas = alpha,
                                       ncores = test_cores())

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

      X <- `if`(t == "raw", asFBMcode(x), big_copy(x, type = t))

      for (covar in sample(lcovar, 1)) {

        ind <- sample(N, N / 2)
        alphas <- c(runif(1, min = 0.01, max = 1), 1)
        ind.sets <- sample(rep_len(1:10, length(ind)))

        mod.bigstatsr <- big_spLogReg(X, y[ind], ind.train = ind,
                                      covar.train = covar[ind, ],
                                      alphas = alphas,
                                      ind.sets = ind.sets,
                                      ncores = test_cores())
        preds <- predict(mod.bigstatsr, X, covar.row = covar)
        expect_gt(AUC(preds[-ind], y[-ind]), 0.7)

        mod.bigstatsr2 <- big_spLogReg(X, y[ind], ind.train = ind,
                                       base.train = rep(5, length(ind)),
                                       covar.train = covar[ind, ],
                                       alphas = alphas,
                                       ind.sets = ind.sets,
                                       ncores = test_cores())
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
                                       ncores = test_cores())
        expect_error(predict(mod.bigstatsr3, X, covar.row = covar),
                     "You forgot to provide 'base.row' in predict().")
        preds3 <- predict(mod.bigstatsr3, X, covar.row = covar, base.row = rep(0, N))
        expect_gt(cor(preds3, preds), 0.9)
      }
    }
  })

}

################################################################################
