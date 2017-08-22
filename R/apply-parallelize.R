################################################################################

#' Split-parApply-Combine
#'
#' A Split-Apply-Combine strategy to parallelize the evaluation of a function.
#'
#' This function splits indices in parts, then apply a given function to each
#' part and finally combine the results.
#'
#' @inheritParams bigstatsr-package
#' @param p.FUN The function to be applied to each subset matrix.
#'   It must take a [Filebacked Big Matrix][FBM-class] as first argument and
#'   `ind`, a vector of indices, which are used to split the data.
#'   For example, if you want to apply a function to \code{X[ind.row, ind.col]},
#'   you may use \code{X[ind.row, ind.col[ind]]} in `a.FUN`.
#' @param p.combine function that is used by [foreach] to process the tasks
#'   results as they generated. This can be specified as either a function or a
#'   non-empty character string naming the function. Specifying 'c' is useful
#'   for concatenating the results into a vector, for example.
#'   The values 'cbind' and 'rbind' can combine vectors into a matrix.
#'   The values '+' and '*' can be used to process numeric data.
#'   By default, the results are returned in a list.
#' @param ind Initial vector of subsetting indices.
#'   Default is the vector of all column indices.
#' @param ... Extra arguments to be passed to `p.FUN`.
#'
#' @return The result of [foreach].
#' @export
#'
#' @example examples/example-parallelize.R
#' @seealso [big_apply]
big_parallelize <- function(X, p.FUN, p.combine,
                            ind = cols_along(X),
                            ...) {

  check_args()
  assert_args(p.FUN, "ind")
  assert_int(ind); assert_pos(ind)

  ncores <- getDoParWorkers()
  if (ncores > 1) { # parallel

    range.parts <- CutBySize(length(ind), nb = ncores)

    # Microsoft R Open?
    multi <- requireNamespace("RevoUtilsMath", quietly = TRUE)

    foreach(ic = 1:ncores, .combine = p.combine) %dopar% {
      # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
      if (multi) {
        nthreads.save <- RevoUtilsMath::setMKLthreads(1)
        on.exit(RevoUtilsMath::setMKLthreads(nthreads.save), add = TRUE)
      }

      p.FUN(X, ind = ind[seq2(range.parts[ic, ])], ...)
    }
  } else { # sequential
    p.FUN(X, ind = ind, ...)
  }
}

################################################################################

big_applySeq <- function(X, a.FUN, a.combine, block.size, ind, ...) {

  intervals <- CutBySize(length(ind), block.size)

  foreach(ic = rows_along(intervals), .combine = a.combine) %do% {
    a.FUN(X, ind = ind[seq2(intervals[ic, ])], ...)
  }
}

################################################################################

#' Split-Apply-Combine
#'
#' A Split-Apply-Combine strategy to apply common R functions to a
#' Filebacked Big Matrix.
#'
#' This function splits indices in parts, then apply a given function to each
#' subset matrix and finally combine the results. If parallelization is used,
#' this function splits indices in parts for parallelization, then split again
#' them on each core, apply a given function to each part and finally combine
#' the results (on each cluster and then from each cluster).
#'
#' @inheritParams bigstatsr-package
#' @param a.FUN The function to be applied to each subset matrix.
#'   It must take a [Filebacked Big Matrix][FBM-class] as first argument and
#'   `ind`, a vector of indices, which are used to split the data.
#'   For example, if you want to apply a function to \code{X[ind.row, ind.col]},
#'   you may use \code{X[ind.row, ind.col[ind]]} in `a.FUN`.
#' @param a.combine function that is used by [foreach] to process the tasks
#'   results as they generated. This can be specified as either a function or a
#'   non-empty character string naming the function. Specifying 'c' is useful
#'   for concatenating the results into a vector, for example.
#'   The values 'cbind' and 'rbind' can combine vectors into a matrix.
#'   The values '+' and '*' can be used to process numeric data.
#'   By default, the results are returned in a list.
#' @param ind Initial vector of subsetting indices.
#'   Default is the vector of all column indices.
#' @param block.size Maximum number of columns (or rows, depending on how you
#'   use `ind` for subsetting) read at once. Default uses [block_size].
#' @param ... Extra arguments to be passed to `a.FUN`.
#'
#' @return The result of [foreach].
#' @export
#'
#' @example examples/example-apply.R
#' @seealso [big_parallelize]
big_apply <- function(X, a.FUN, a.combine,
                      ind = cols_along(X),
                      block.size = block_size(nrow(X)),
                      ...) {

  check_args()
  assert_args(a.FUN, "ind")
  assert_int(ind); assert_pos(ind)

  big_parallelize(X = X,
                  p.FUN = big_applySeq,
                  p.combine = a.combine,
                  ind = ind,
                  a.FUN = a.FUN,
                  a.combine = a.combine,
                  block.size = block.size,
                  ...)
}

################################################################################
