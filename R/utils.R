################################################################################

# Global variables
ALL.TYPES <- c("char", "short", "integer", "float", "double")
ERROR_CLASS <- "y should be a vector of 1 (cases) and -1 (controls)."
ERROR_REG <- paste0("y has not enough unique elements.\n",
                    "Try using CoeffsClass instead.")
ERROR_TYPE <- function() {
  "unknown type detected for big.matrix object!"
}

################################################################################

printf <- function(...) cat(sprintf(...))

################################################################################
