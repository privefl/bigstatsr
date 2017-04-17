################################################################################

#' Augment a dataset with interactions
#'
#' Augment the number of column of a `big.matrox` by adding new columns
#' which are interactions between pairs. It may be good idea to use
#' [big_standardize] to have scaled columns as input.
#'
#' @inheritParams bigstatsr-package
#' @param arr.ind Matrix of two columns `row` and `col` specifying pairs
#' that need to be multiplied to create new columns.
#'
#' @return The newly created `big.matrix` (or its descriptor).
#' @export
#'
#' @examples
#' tmp <- tmpFBM(descriptor = FALSE)(10, 5, type = "double")
#' tmp[] <- rnorm(length(tmp))
#' big_standardize(tmp)
#' apply(tmp[,], 2, function(x) c(mean(x), sd(x)))
#'
#' test <- big_addInter(tmp, arr.ind = cbind(1:4, 5))
big_addInter <- function(X., arr.ind, fun.createBM = BM()) {

  assert_type(X., "double")

  res <- fun.createBM(nrow = nrow(X.),
                      ncol = ncol(X.) + nrow(arr.ind),
                      type = "double")

  addInter(attach.BM(res)@address,
           attach.BM(X.)@address,
           arr.ind)

  res
}

################################################################################
