################################################################################

# Global variables
ALL.TYPES <- c("char", "short", "integer", "float", "double")
ERROR_CLASS <- "y should be a vector of 1 (cases) and -1 (controls)."
ERROR_REG <- paste0("y has not enough unique elements.\n",
                    "Try using CoeffsClass instead.")

# also defined in src/utils.h
ERROR_TYPE <- "unknown type detected for big.matrix object!"

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
