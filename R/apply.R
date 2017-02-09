################################################################################

#' Standard univariate statistics
#'
#' Standard __univariate statistics__ for columns of a big.matrix.
#' For now, the `sum` and `var` are implemented
#' (the `mean` and `sd` can easily be deduced, see examples).
#'
#' @inheritParams bigstatsr-package
#'
#' @return Data.frame of two numeric vectors `sum` and `var` with the
#' corresponding column statistics.
#' @export
#'
#' @seealso [colSums] [apply]
#' @example examples/example-colstats.R
big_colstats <- function(X, ind.train = seq(nrow(X)), ind.col = seq(ncol(X))) {
  check_X(X)
  data.frame(bigcolvars(X@address, ind.train, ind.col))
}

################################################################################

big_applySeq <- function(X, FUN, .combine, block.size = 1e3) {
  intervals <- CutBySize(ncol(X), block.size)

  foreach(k = 1:nrow(intervals), .combine = .combine) %do% {
    FUN(X[, seq2(intervals[k, ]), drop = FALSE])
  }
}

#' Split-Apply-Combine
#'
#' A Split-Apply-Combine strategy to apply common R functions to a `big.matrix`.
#'
#' This function split a `big.matrix` in column blocks, then apply a given
#' function to each block matrix and finally combine the results.
#'
#' @inheritParams bigstatsr-package
#' @param FUN The function to be applied to each column block matrix.
#' @inheritParams foreach::foreach
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @examples
big_apply <- function(X, FUN, .combine, block.size = 1e3, ncores = 1) {
  if (ncores == 1) {
    check_X(X)

    big_applySeq(X, FUN, .combine, block.size)
  } else {
    check_X(X, ncores = ncores)

    range.parts <- CutBySize(ncol(X), nb = ncores)

    X.desc <- describe(X)
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    foreach(ic = 1:ncores, .combine = .combine) %dopar% {
      lims <- range.parts[ic, ]

      X.part <- sub.big.matrix(X.desc, firstCol = lims[1], lastCol = lims[2])

      # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
      if (detect_MRO()) {
        nthreads.save <- RevoUtilsMath::setMKLthreads(1)
        on.exit(RevoUtilsMath::setMKLthreads(nthreads.save), add = TRUE)
      }
      big_applySeq(X.part, FUN, .combine, block.size)
    }
  }
}

################################################################################
