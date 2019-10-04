################################################################################

# Global variables
ALL.TYPES <- structure(c(1L, 1L, 2L, 4L, 6L, 8L),
                       names = c("raw", "unsigned char", "unsigned short",
                                 "integer", "float", "double"))
globalVariables(c("ic", "mods", "k", "loss_index", "set"))

################################################################################

#' Determine a correct value for the block.size parameter
#'
#' It determines the value of `block.size` such that a matrix of doubles of
#' size `n` x `block.size` takes less memory than
#' `getOption("bigstatsr.block.sizeGB")` GigaBytes (default is 1GB).
#'
#' @param n The number of rows.
#' @param ncores The number of cores.
#'
#' @return An integer >= 1.
#'
#' @export
#'
#' @examples
#' block_size(1e3)
#' block_size(1e6)
#' block_size(1e6, 6)
block_size <- function(n, ncores = 1) {
  block.max <- getOption("bigstatsr.block.sizeGB") / ncores
  # 8 * n * m < opt * 1024^3
  # m < opt * 1024^3 / (8 * n)
  max(1, floor(block.max * 1024^3 / (8 * n)))
}

################################################################################

CutBySize <- function(m, block.size, nb = ceiling(m / block.size)) {

  if (nb > m) {
    nb <- m
  } else if (nb == 0) {  ## block.size = Inf
    nb <- 1
  }
  assert_pos(nb); assert_int(nb)
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

getAvailMem <- function(format = TRUE) {

  .Deprecated("memuse::Sys.meminfo()")

  gc()

  if (Sys.info()[["sysname"]] == "Windows") {
    memfree <- 1024^2 * (utils::memory.limit() - utils::memory.size())
  } else {
    # http://stackoverflow.com/a/6457769/6103040
    memfree <- 1024 * as.numeric(
      system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))
  }

  `if`(format, format(structure(memfree, class = "object_size"),
                      units = "auto"), memfree)
}

################################################################################
#### Sequence generation ####

#' Sequence generation
#'
#' Similar to [seq_along], it creates sequences of size `nrow(x)` or `ncol(x)`.
#'
#' @param x Any object on which you can call `nrow` and `ncol`.
#'
#' @examples
#' X <- big_attachExtdata()
#' dim(X)
#' str(rows_along(X))
#' str(cols_along(X))
#'
#' @rdname seq-dim
#' @keywords internal
#' @export
rows_along <- function(x) seq_len(nrow(x))

#' @rdname seq-dim
#' @export
cols_along <- function(x) seq_len(ncol(x))

################################################################################

#' Increment an FBM
#'
#' @param X An `FBM` (of type double) to increment.
#' @param add A matrix of same dimensions as `X`. Or a vector of same size.
#' @param use_lock Whether to use locks when incrementing. Default is `FALSE`.
#'   This is useful when incrementing in parallel.
#'
#' @return Returns nothing (`NULL`, invisibly).
#'
#' @export
#'
#' @examples
#' X <- FBM(10, 10, init = 0)
#' mat <- matrix(rnorm(100), 10, 10)
#'
#' big_increment(X, mat)
#' all.equal(X[], mat)
#'
#' big_increment(X, mat)
#' all.equal(X[], 2 * mat)
#'
big_increment <- function(X, add, use_lock = FALSE) {

  if (use_lock) {
    locked <- flock::lock(X$backingfile)
    on.exit(flock::unlock(locked), add = TRUE)
  }

  if (is.matrix(add)) incr_FBM_mat(X, add) else incr_FBM_vec(X, add)
}

################################################################################
