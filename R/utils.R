################################################################################

# Global variables
ALL.TYPES <- c("char", "short", "integer", "float", "double")

ERROR_BIGMATRIX <- "X must be a big.matrix."
ERROR_SHARED <- "You can't use parallelism with a non-shared big.matrix"
WARNING_NCORES <- "You shouldn't try to use more cores that you actually have"

# also defined in inst/include/bigstatsr.h
ERROR_TYPE <- "unknown type detected for big.matrix object!"

################################################################################

check_biglasso <- function() {
  if(utils::packageVersion("biglasso") != "1.3.1.6670")
    stop(paste0("Please use my fork for now ",
                "(until I merge it with Yaohui Zeng's repo.)\n",
                "You can get it via ",
                "'devtools::install_github(\"privefl/biglasso\")'."))
}

################################################################################

check_X <- function(X, ncores = 1) {
  if (class(X) != "big.matrix")
    stop(ERROR_BIGMATRIX)

  if (ncores > 1 && !is.shared(X))
    stop(ERROR_SHARED)

  if (ncores > parallel::detectCores())
    warning(WARNING_NCORES)
}

################################################################################

detect_MRO <- function() {
  # is.element("RevoUtilsMath", rownames(utils::installed.packages()))
  requireNamespace("RevoUtilsMath", quietly = TRUE)
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

printf <- function(...) cat(sprintf(...))

################################################################################

CutBySize <- function(m, block.size, nb = ceiling(m / block.size)) {
  int <- m / nb

  upper <- round(1:nb * int)
  lower <- c(1, upper[-nb] + 1)
  size <- c(upper[1], diff(upper))

  cbind(lower, upper, size)
}

################################################################################

seq2 <- function(lims) {
  seq(lims[1], lims[2])
}

################################################################################
