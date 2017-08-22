################################################################################

#' Copy a Filebacked Big Matrix
#'
#' Copy a Filebacked Big Matrix with possible subsetting.
#'
#' @inheritParams bigstatsr-package
#' @inheritParams FBM
#'
#' @return A copy of the [FBM][FBM-class].
#' @export
#'
#' @examples
#' X <- FBM(10, 10, init = 1:100)
#' X[]
#' X2 <- big_copy(X, ind.row = 1:5)
#' X2[]
#'
big_copy <- function(X, ind.row = rows_along(X),
                     ind.col = cols_along(X),
                     type = names(X$type),
                     backingfile = tempfile(),
                     save = FALSE,
                     block.size = block_size(length(ind.row))) {

  res <- FBM(
    nrow = length(ind.row),
    ncol = length(ind.col),
    init = NULL,
    type = type,
    backingfile = backingfile,
    save = save
  )

  # Warn only once
  warn_downcast(from = X, to = res)
  opt.save <- options(bigstatsr.typecast.warning = FALSE)
  on.exit(options(opt.save), add = TRUE)

  big_apply(X, a.FUN = function(X, ind, X2, ind.row, ind.col) {
    X2[, ind] <- X[ind.row, ind.col[ind]]
    NULL
  }, a.combine = 'c', ind = seq_along(ind.col), block.size = block.size,
  X2 = res, ind.row = ind.row, ind.col = ind.col)

  res
}

################################################################################
