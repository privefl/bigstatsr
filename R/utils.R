################################################################################

# Global variables
ALL.TYPES <- c("char", "short", "integer", "float", "double") # for tests
globalVariables("ic") # for foreach

ERROR_BIGMATRIX <- "X must be a big.matrix."
ERROR_SHARED <- "You can't use parallelism with a non-shared big.matrix."
WARNING_NCORES <- "You shouldn't try to use more cores than you actually have."
WARNING_NCORES2 <- paste("For this function, you shouldn't try to use",
                          "more than half of the cores you have.")

# also defined in inst/include/bigstatsr.h
ERROR_TYPE <- "unknown type detected for big.matrix object!"

################################################################################

check_X <- function(X, ncores = 1, ncores2 = 1) {
  if (class(X) != "big.matrix")
    stop(ERROR_BIGMATRIX)

  if (ncores > 1 && !is.shared(X))
    stop(ERROR_SHARED)

  Allcores <- parallel::detectCores()

  if (ncores > Allcores)
    warning(WARNING_NCORES)

  if (ncores2 > max(1, Allcores / 2))
    warning(WARNING_NCORES2)
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
