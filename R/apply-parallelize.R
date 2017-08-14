################################################################################

#' Recommended number of cores to use
#'
#' This is base on the following rule: use only physical cores and if you have
#' only physical cores, leave one core for the OS/UI.
#'
#' @inheritParams parallel::detectCores
#'
#' @return The recommended number of cores to use.
#' @export
#'
#' @seealso [parallel::detectCores]
#'
#' @examples
#' # Number of cores in total
#' parallel::detectCores()
#' # Number of physical cores
#' parallel::detectCores(logical = FALSE)
#' # Recommended number of cores to use
#' ncores()
ncores <- function(all.tests = FALSE) {
  all_cores <- parallel::detectCores(all.tests = all.tests)
  all_physical_cores <- parallel::detectCores(all.tests = all.tests,
                                              logical = FALSE)
  `if`(all_physical_cores < all_cores, all_physical_cores, all_cores - 1)
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
#' @param p.FUN The function to be applied. It must take a
#' [big.matrix.descriptor][big.matrix.descriptor-class] as first argument
#' and provide some arguments for subsetting.
#' @param p.combine function that is used by [foreach] to process the tasks
#' results as they generated. This can be specified as either a function or a
#' non-empty character string naming the function. Specifying 'c' is useful
#' for concatenating the results into a vector, for example. The values 'cbind'
#' and 'rbind' can combine vectors into a matrix. The values '+' and '*' can be
#' used to process numeric data. By default, the results are returned in a list.
#' @param ind Initial vector of subsetting indices.
#' Default is the vector of all column indices.
#' @param ... Extra arguments to be passed to `p.FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @example examples/example-parallelize.R
#' @seealso [big_apply]
big_parallelize <- function(X., p.FUN, p.combine, ncores,
                            ind = cols_along(X.),
                            ...) {

  check_args(X. = "assert_classOrDesc(X., 'big.matrix')")
  assert_args(p.FUN, "ind")
  assert_int(ind); assert_pos(ind)

  if (ncores > 1) { # parallel
    X.desc <- describe(X.)
    range.parts <- CutBySize(length(ind), nb = ncores)

    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Microsoft R Open?
    multi <- requireNamespace("RevoUtilsMath", quietly = TRUE)

    foreach(ic = 1:ncores, .combine = p.combine) %dopar% {
      # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
      if (multi) {
        nthreads.save <- RevoUtilsMath::setMKLthreads(1)
        on.exit(RevoUtilsMath::setMKLthreads(nthreads.save), add = TRUE)
      }

      p.FUN(X.desc, ind = ind[seq2(range.parts[ic, ])], ...)
    }
  } else { # sequential
    p.FUN(X., ind = ind, ...)
  }
}

#' export
big_parallelize2 <- function(ind, p.FUN, p.combine,
                             ncores = ncores(),
                             ...) {

  # str(list(...)) #DEBUG

  # check_args(X. = "assert_classOrDesc(X., 'big.matrix')")
  assert_args(p.FUN, "ind")
  assert_int(ind); assert_pos(ind)

  if (ncores > 1) { # parallel

    range.parts <- CutBySize(length(ind), nb = ncores)

    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Microsoft R Open?
    multi <- requireNamespace("RevoUtilsMath", quietly = TRUE)

    foreach(ic = 1:ncores, .combine = p.combine) %dopar% {
      # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
      if (multi) {
        nthreads.save <- RevoUtilsMath::setMKLthreads(1)
        on.exit(RevoUtilsMath::setMKLthreads(nthreads.save), add = TRUE)
      }

      p.FUN(ind = ind[seq2(range.parts[ic, ])], ...)
    }
  } else { # sequential
    p.FUN(ind = ind, ...)
  }
}

################################################################################

big_applySeq <- function(X., a.FUN, a.combine, block.size, ind, ...) {

  X <- attach.BM(X.)
  intervals <- CutBySize(length(ind), block.size)

  foreach(ic = 1:nrow(intervals), .combine = a.combine) %do% {
    a.FUN(X, ind = ind[seq2(intervals[ic, ])], ...)
  }
}

#' export
big_applySeq2 <- function(ind, a.FUN, a.combine, block.size, ...) {

  # str(list(...)) #DEBUG

  intervals <- CutBySize(length(ind), block.size)

  foreach(ic = 1:nrow(intervals), .combine = a.combine) %do% {
    a.FUN(ind = ind[seq2(intervals[ic, ])], ...)
  }
}

################################################################################

#' Split-Apply-Combine
#'
#' A Split-Apply-Combine strategy to apply common R functions to a `big.matrix`.
#'
#' This function splits indices in parts, then apply a given function to each
#' subset matrix and finally combine the results. If parallelization is used,
#' this function splits indices in parts for parallelization, then split again
#' them on each core, apply a given function to each part and finally combine
#' the results (on each cluster and then from each cluster).
#'
#' @inheritParams bigstatsr-package
#' @param a.FUN The function to be applied to each subset matrix.
#' It must take a `big.matrix` as first argument and `ind`, a vector of
#' indices, which are used to split the data. Example: if you want to apply
#' a function to `X[ind.row, ind.col]`, you may use
#' \code{X[ind.row, ind.col[ind]]} in `a.FUN`.
#' @param a.combine function that is used by [foreach] to process the tasks
#' results as they generated. This can be specified as either a function or a
#' non-empty character string naming the function. Specifying 'c' is useful
#' for concatenating the results into a vector, for example. The values 'cbind'
#' and 'rbind' can combine vectors into a matrix. The values '+' and '*' can be
#' used to process numeric data. By default, the results are returned in a list.
#' @param ind Initial vector of subsetting indices.
#' Default is the vector of all column indices.
#' @param block.size Maximum number of columns (or rows, depending on how you
#' use `ind` for subsetting) read at once. Default is `1000`.
#' @param ... Extra arguments to be passed to `a.FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @example examples/example-apply.R
#' @seealso [big_parallelize]
big_apply <- function(X., a.FUN, a.combine,
                      ncores = 1,
                      block.size = 1000,
                      ind = cols_along(X.),
                      ...) {

  check_args(X. = "assert_classOrDesc(X., 'big.matrix')")
  assert_args(a.FUN, "ind")
  assert_int(ind); assert_pos(ind)

  big_parallelize(X. = X.,
                  p.FUN = big_applySeq,
                  p.combine = a.combine,
                  ncores = ncores,
                  ind = ind,
                  a.FUN = a.FUN,
                  a.combine = a.combine,
                  block.size = block.size,
                  ...)
}

#' @export
big_apply2 <- function(ind, a.FUN, a.combine,
                       ncores = 1,
                       block.size = 1000,
                       ...) {

  # str(list(...)) #DEBUG

  # check_args(X. = "assert_classOrDesc(X., 'big.matrix')")
  assert_args(a.FUN, "ind")
  assert_int(ind); assert_pos(ind)

  big_parallelize2(ind = ind,
                   p.FUN = big_applySeq2,
                   p.combine = a.combine,
                   ncores = ncores,
                   a.FUN = a.FUN,
                   a.combine = a.combine,
                   block.size = block.size,
                   ...)
}

################################################################################
