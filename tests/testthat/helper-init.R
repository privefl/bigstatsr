################################################################################

# https://github.com/hadley/testthat/issues/567
Sys.unsetenv("R_TESTS")

################################################################################

library(testthat)
library(bigstatsr)
library(bigparallelr)

################################################################################

opt.save <- options(bigstatsr.downcast.warning = FALSE,
                    bigstatsr.block.sizeGB = 1e-5,
                    bigstatsr.check.parallel.blas = FALSE)

################################################################################

not_cran <- identical(Sys.getenv("BIGSTATSR_CRAN"), "false")

test_cores <- function() {
  `if`(not_cran && (parallel::detectCores() > 2) &&
         identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "false"),
       sample(2, size = 1), 1)
}

################################################################################

TEST.TYPES <- c("raw", "unsigned short", "integer", "float", "double")

asFBMcode <- function(x, same_code = FALSE) {
  x <- round(x)
  if (same_code) {
    tmp <- unique(as.vector(x))
    code <- rep(NA_real_, 256)
    code[tmp + 1] <- tmp
  } else {
    code <- rnorm(256)
  }
  storage.mode(x) <- "raw"
  add_code256(big_copy(x, type = "raw"), code)
}

################################################################################

# function for comparing PCs
diffPCs <- function(test, rot) {
  k <- ncol(test)
  diff1 <- 2 * abs(test - rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff2 <- 2 * abs(test + rot[, 1:k]) / (abs(test) + abs(rot[, 1:k]))
  diff <- pmin(diff1, diff2)
  mean(diff)
}

################################################################################

set.seed(NULL)
if (not_cran) {
  # Seeds that won't work (because of bad luck)
  # 3544 -> spLinReg & spLogReg
  # 4475, 6649 -> spLinReg
  do_not_use <- c(3544, 4475, 6649)
  while ((SEED <- round(runif(1, 1, 9999))) %in% do_not_use) NULL
} else {
  SEED <- 1
}
cat("========== SEED:", SEED, "==========\n")

################################################################################
