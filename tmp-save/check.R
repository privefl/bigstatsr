
ERROR_CLASS <- "y should be a vector of 1 (cases) and -1 (controls)."

ERROR_REG <- paste0("y has not enough unique elements.\n",
                    "Try using CoeffsClass instead.")

################################################################################

#' @title Checking of objects
#' @description Check if X is a `big.matrix` and
#' possibly if y is compatible with classification or regression.
#'
#' @param X A matrix-like object you want to check as being a
#' [big.matrix][bigmemory::big.matrix-class].
#' @param y A vector of responses you want to check compatibility with
#' what you are doing (either classification or regression).
#' @param y.type A string, either
#' - "reg" if you want to check the compatibility with regression,
#' - "class" if you want to check the compatibility with classification.
#'
#' @return Throwing an error if something is wrong.
#' @export
#'
#' @example examples/example.check_X.R
check_X <- function(X, y = NULL, y.type = "null") {
  if (class(X) != "big.matrix")
    stop(ERROR_BIGMATRIX)

  if (y.type == "reg") {
    if (!(length(unique(y)) > 2))
      stop(ERROR_REG)
  } else if (y.type == "class") {
    if (!isTRUE(all.equal(sort(unique(y)), c(-1, 1))))
      stop(ERROR_CLASS)
  }

  0
}

################################################################################
