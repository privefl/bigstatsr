################################################################################

# Global variables
globalVariables(c("ic", "mods", "k", "loss_index", "set", "pow_sc", "pow_adapt",
                  "_DF_", "_IND_COL_"))

################################################################################

other_if_null <- function(x, other) `if`(is.null(x), other, x)

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

#' Numeric matrix from data frame
#'
#' Transform a data frame to a numeric matrix by one-hot encoding factors.
#' The last factor value is always omitted to prevent having a singular matrix
#' when adding a column of 1s (intercept) in models.
#'
#' @param df A data frame.
#'
#' @return A numeric matrix.
#' @export
#'
#' @examples
#' mat <- covar_from_df(iris)
#' head(mat)
covar_from_df <- function(df) {

  assert_class(df, "data.frame")

  if (is.null(names(df))) names(df) <- paste0("V", seq_along(df))

  # https://stackoverflow.com/a/49071411/6103040
  stats::model.matrix.lm(~ ., data = df, na.action = "na.pass")[, -1, drop = FALSE]
}

################################################################################

#' Scaling function creator
#'
#' Convenience function to create a function to be used as parameter `fun.scaling`
#' when you want to use your own precomputed center and scale.
#'
#' @param center.col Vector of centers corresponding to `ind.col`.
#' @param scale.col Vector of scales corresponding to `ind.col`.
#' @param ind.col Column indices for which these are provided.
#'
#' @return A function to be used as parameter `fun.scaling`.
#' @export
#'
#' @examples
#' fun.scaling <- as_scaling_fun(1:6, 2:7)
#' fun.scaling(NULL, NULL, 1:3)  # first two parameters X and ind.row are not used here
#' fun.scaling2 <- as_scaling_fun(1:6, 2:7, ind.col = 6:1)
#' fun.scaling2(NULL, NULL, 1:3)
#'
#'
#' X <- big_attachExtdata()
#' sc <- big_scale()(X)
#' fun <- as_scaling_fun(center = sc$center, scale = sc$scale)
#' obj.svd <- big_randomSVD(X, fun.scaling = fun)
#' obj.svd2 <- big_randomSVD(X, fun.scaling = big_scale())
#' all.equal(obj.svd, obj.svd2)
#'
as_scaling_fun <- function(center.col, scale.col, ind.col = seq_along(center.col)) {

  assert_lengths(center.col, scale.col, ind.col)

  e <- new.env(parent = baseenv())
  assign("_DF_", data.frame(center = center.col, scale = scale.col), envir = e)
  assign("_IND_COL_", ind.col, envir = e)

  f <- function(X, ind.row, ind.col) {
    ind <- match(ind.col, `_IND_COL_`)
    `_DF_`[ind, ]
  }
  environment(f) <- e

  f
}

################################################################################
