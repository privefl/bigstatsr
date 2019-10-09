################################################################################

# Global variables
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
#### Splitting ####

CutBySize <- function(m, block.size, nb = ceiling(m / block.size)) {
  bigparallelr::split_len(m, nb_split = nb)
}

################################################################################
#### Sequence generation ####

seq2 <- bigparallelr::seq_range

#' @importFrom bigparallelr rows_along
#' @export
bigparallelr::rows_along

#' @importFrom bigparallelr cols_along
#' @export
bigparallelr::cols_along

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
    locked <- bigparallelr::lock(X$backingfile)
    on.exit(bigparallelr::unlock(locked), add = TRUE)
  }

  if (is.matrix(add)) incr_FBM_mat(X, add) else incr_FBM_vec(X, add)
}

################################################################################
