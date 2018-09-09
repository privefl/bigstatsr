################################################################################

# https://github.com/hadley/testthat/issues/567
Sys.unsetenv("R_TESTS")

################################################################################

library(testthat)
library(bigstatsr)

################################################################################

opt.save <- options(bigstatsr.downcast.warning = FALSE,
                    bigstatsr.block.sizeGB = 1e-5)

################################################################################

not_cran <- identical(Sys.getenv("BIGSTATSR_CRAN"), "false")

test_cores <- function() `if`(not_cran, sample(2, size = 1), 1)

################################################################################

TEST.TYPES <- c("raw", "unsigned short", "integer", "float", "double")

asFBMcode <- function(x) {
  x <- round(x)
  tmp <- unique(as.vector(x))
  code <- rep(NA_real_, 256)
  code[tmp + 1] <- tmp
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
  # 6649 -> spLinReg
  do_not_use <- c(3544, 6649)
  while ((SEED <- round(runif(1, 1, 9999))) %in% do_not_use) NULL
} else {
  SEED <- 1
}
cat("========== SEED:", SEED, "==========\n")

################################################################################
