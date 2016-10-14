################################################################################

# Global variables
ALL.TYPES <- c("char", "short", "integer", "float", "double")

ERROR_CLASS <- "y should be a vector of 1 (cases) and -1 (controls)."

ERROR_REG <- paste0("y has not enough unique elements.\n",
                    "Try using CoeffsClass instead.")

ERROR_BIGMATRIX <- "X must be a big.matrix."

# also defined in src/utils.h
ERROR_TYPE <- "unknown type detected for big.matrix object!"

################################################################################

check_X <- function(X, y = NULL, y.type = "null") {
  if (class(X) != "big.matrix")
    stop(ERROR_BIGMATRIX)

  if (y.type == "reg")
    if (!(length(unique(y)) > 2))
      stop(ERROR_REG)

  if (y.type == "class")
    if (!(all(sort(unique(y)) == c(-1, 1))))
      stop(ERROR_CLASS)
}

################################################################################

printf <- function(...) cat(sprintf(...))

################################################################################

CutBySize <- function(m, block.size) {
  nb <- ceiling(m / block.size)
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
