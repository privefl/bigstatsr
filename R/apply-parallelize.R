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
#' @param ... Extra arguments to be passed to `FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @example examples/example-apply.R
big_apply <- function(X.desc, a.MARGIN, a.FUN, a.combine, block.size = 1000,
                      ind.row = seq_len(nrow(X.desc)),
                      ind.col = seq_len(ncol(X.desc)),
                      ...) {
  X <- attach.big.matrix(X.desc)

  l <- length(`if`(a.MARGIN == 2, ind.col, ind.row))
  intervals <- CutBySize(l, block.size)

  foreach(ic = 1:nrow(intervals), .combine = a.combine) %do% {
    lims <- intervals[ic, ]

    if (a.MARGIN == 2) {
      a.FUN(X[ind.row, ind.col[seq2(lims)]], ...)
    } else {
      a.FUN(X[ind.row[seq2(lims)], ind.col], ...)
    }
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
#' @param ... Extra arguments to be passed to `FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @examples
#' X.desc <- big_attachExtdata()
#'
#' test <- big_colstats(X.desc)
#' test2 <- big_parallelize(X.desc, 2, big_colstats,
#'                          p.combine = 'rbind', ncores = 2)
#'
#' all.equal(test2, test)
big_parallelize <- function(X.desc, p.MARGIN, p.FUN, p.combine, ncores,
                            ind.row = seq_len(nrow(X.desc)),
                            ind.col = seq_len(ncol(X.desc)),
                            ...) {
  l <- length(`if`(p.MARGIN == 2, ind.col, ind.row))
  range.parts <- CutBySize(l, nb = ncores)

  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  multi <- detect_MRO()

  foreach(ic = 1:ncores, .combine = p.combine) %dopar% {
    lims <- range.parts[ic, ]

    # https://www.r-bloggers.com/too-much-parallelism-is-as-bad/
    if (multi) {
      nthreads.save <- RevoUtilsMath::setMKLthreads(1)
      on.exit(RevoUtilsMath::setMKLthreads(nthreads.save), add = TRUE)
    }
    if (p.MARGIN == 2) {
      p.FUN(X.desc, ind.row = ind.row, ind.col = ind.col[seq2(lims)], ...)
    } else {
      p.FUN(X.desc, ind.row = ind.row[seq2(lims)], ind.col = ind.col, ...)
    }
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
#' @param ... Extra arguments to be passed to `FUN`.
#'
#' @return The result of [foreach].
#' @export
#' @import foreach
#'
#' @examples
#' X.desc <- big_attachExtdata()
#'
#' test <- big_colstats(X.desc)
#' test2 <- big_parallelize(X.desc, 2, big_colstats,
#'                          p.combine = 'rbind', ncores = 2)
#'
#' all.equal(test2, test)
big_parApply <- function(X.desc, MARGIN, FUN, .combine, ncores,
                         block.size = 1000,
                         ind.row = seq_len(nrow(X.desc)),
                         ind.col = seq_len(ncol(X.desc)),
                         ...) {
  big_parallelize(X.desc,
                  p.MARGIN = MARGIN,
                  p.FUN = big_apply,
                  p.combine = .combine,
                  ncores = ncores,
                  a.MARGIN = MARGIN,
                  a.FUN = FUN,
                  a.combine = .combine,
                  block.size = block.size,
                  ind.row = ind.row,
                  ind.col = ind.col,
                  ...)
}

################################################################################

sub1 <- function(x, ind) x[ind, ]
sub2 <- function(x, ind) x[, ind]

#' Title
#'
#' @param X.desc
#' @param a.MARGIN
#' @param a.FUN
#' @param a.combine
#' @param block.size
#' @param ind.row
#' @param ind.col
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
big_apply2 <- function(BM, a.MARGIN, a.FUN, a.combine, block.size = 1000,
                       ...) {
  X <- attach.big.matrix(X.desc)

  l <- length(`if`(a.MARGIN == 2, ind.col, ind.row))
  intervals <- CutBySize(l, block.size)

  foreach(ic = 1:nrow(intervals), .combine = a.combine) %do% {
    lims <- intervals[ic, ]
    ind <- seq2(lims)
    extra.args <- c(args,
                    lapply(args.sub1, sub1, ind = ind),
                    lapply(args.sub2, sub2, ind = ind))
    str(extra.args)

    if (a.MARGIN == 2) {
      do.call(a.FUN, c(x = X[ind.row, ind.col[ind]], extra.args))
    } else {
      do.call(a.FUN, c(x = X[ind.row[ind], ind.col], extra.args))
    }
  }
}
