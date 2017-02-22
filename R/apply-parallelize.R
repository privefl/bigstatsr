################################################################################

#' Split-Apply-Combine
#'
#' A Split-Apply-Combine strategy to apply common R functions to a `big.matrix`.
#'
#' This function splits indices in parts, then apply a given function to each
#' subset matrix and finally combine the results.
#'
#' @inheritParams bigstatsr-package
#' @param a.FUN The function to be applied to each subset matrix.
#' @inheritParams foreach::foreach
#' @param ... Extra arguments to be passed to `a.FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @example examples/example-apply.R
big_apply <- function(X.desc, a.FUN, a.combine, block.size = 1000,
                      ind = cols_along(X.desc),
                      ...) {
  X <- attach.big.matrix(X.desc)

  intervals <- CutBySize(length(ind), block.size)

  foreach(ic = 1:nrow(intervals), .combine = a.combine) %do% {
    a.FUN(X, ind = ind[seq2(intervals[ic, ])], ...)
  }
}

################################################################################

#' Split-parApply-Combine
#'
#' A Split-Apply-Combine strategy to parallelize the evaluation of a function.
#'
#' This function splits indices in parts, then apply a given function to each
#' part and finally combine the results.
#'
#' @inheritParams bigstatsr-package
#' @param p.FUN The function to be applied. `X.desc` must be its first argument
#' and it should have two other arguments, `ind.row` and `ind.col`.
#' @inheritParams foreach::foreach
#' @param ... Extra arguments to be passed to `p.FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @example examples/example-parallelize.R
big_parallelize <- function(X.desc, p.FUN, p.combine, ncores,
                            ind = cols_along(X.desc),
                            ...) {
  range.parts <- CutBySize(length(ind), nb = ncores)

  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  multi <- detect_MRO()

  foreach(ic = 1:ncores, .combine = p.combine) %dopar% {
    # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
    if (multi) {
      nthreads.save <- RevoUtilsMath::setMKLthreads(1)
      on.exit(RevoUtilsMath::setMKLthreads(nthreads.save), add = TRUE)
    }

    p.FUN(X.desc, ind = ind[seq2(range.parts[ic, ])], ...)
  }
}

################################################################################

#' Split-parApply-Combine
#'
#' A Split-Apply-Combine strategy to parallelize the evaluation of
#' `big_apply` on a function.
#'
#' This function splits indices in parts for parallelization, then split again
#' them on each core, apply a given function to each part and finally combine
#' the results (on each cluster and then from each cluster).
#'
#' @inheritParams bigstatsr-package
#' @param p.FUN The function to be applied. `X.desc` must be its first argument
#' and it should have two other arguments, `ind.row` and `ind.col`.
#' @inheritParams foreach::foreach
#' @param ... Extra arguments to be passed to `FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @example examples/example-parApply.R
big_parApply <- function(X.desc, a.FUN, a.combine, ncores, block.size = 1000,
                         ind = cols_along(X.desc),
                         ...) {
  big_parallelize(X.desc,
                  p.FUN = big_apply,
                  p.combine = a.combine,
                  ncores = ncores,
                  ind = ind,
                  a.FUN = a.FUN,
                  a.combine = a.combine,
                  block.size = block.size,
                  ...)
}

################################################################################
