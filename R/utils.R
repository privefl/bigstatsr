################################################################################

# Global variables
ALL.TYPES <- c("char", "short", "integer", "float", "double")

ERROR_BIGMATRIX <- "X must be a big.matrix."

# also defined in src/utils.h
ERROR_TYPE <- "unknown type detected for big.matrix object!"

################################################################################

check_X <- function(X, y = NULL, y.type = "null") {
  if (class(X) != "big.matrix")
    stop(ERROR_BIGMATRIX)
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

foreach2 <- function(obj, expr_fun, ncores, outfile = NULL) {
  if (is.seq <- (ncores == 1)) {
    foreach::registerDoSEQ()
  } else {
    if (is.null(outfile)) {
      cl <- parallel::makeCluster(ncores)
    } else {
      cl <- parallel::makeCluster(ncores, outfile = outfile)
    }
    doParallel::registerDoParallel(cl)
  }
  res <- eval(parse(text = sprintf("foreach::`%%dopar%%`(obj, expr_fun(%s))",
                                   obj$argnames)))
  if (!is.seq) parallel::stopCluster(cl)

  return(res)
}

################################################################################

