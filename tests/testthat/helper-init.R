################################################################################

library(Matrix)

################################################################################

test_cores <- function() {

  is.cran <- !identical(Sys.getenv("NOT_CRAN"), "true")

  is.randomSVD <- (get_reporter()$.context == "RANDOM_SVD")

  `if`(is.cran && is.randomSVD, 1, sample(2, size = 1, prob = c(3, 1)))
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

opt.save <- options(bigstatsr.typecast.warning = FALSE)

################################################################################
