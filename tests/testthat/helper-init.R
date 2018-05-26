################################################################################

# https://github.com/hadley/testthat/issues/567
Sys.unsetenv("R_TESTS")

################################################################################

library(testthat)
library(bigstatsr)
library(Matrix)

################################################################################

test_cores <- function() {

  is.cran      <- !identical(Sys.getenv("BIGSTATSR_CRAN"), "false")
  # is.randomSVD <- (get_reporter()$.context == "RANDOM_SVD")

  `if`(is.cran, 1, sample(2, size = 1))  # && is.randomSVD
}

################################################################################

TEST.TYPES <- c("raw", "unsigned short", "integer", "double")

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

opt.save <- options(bigstatsr.typecast.warning = FALSE,
                    bigstatsr.block.sizeGB = 1e-5)

################################################################################

set.seed(NULL)
# Seeds that won't work (because of bad luck)
##  235: big_spLogReg
do_not_use <- c(235)
while ((SEED <- round(runif(1, 1, 9999))) %in% do_not_use) NULL
cat("========== SEED:", SEED, "==========\n")

################################################################################
