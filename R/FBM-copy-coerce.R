################################################################################

#' Methods to copy an object to a FBM
#'
#' @param x Either a [FBM][FBM-class], a [big.matrix][big.matrix-class] or
#'   a standard R matrix.
#'
#' @export
#'
setGeneric(
  "big_copy",
  function(x, ...) {
    standardGeneric("big_copy")
  }
)

################################################################################

big_copy0 <- function(X, ind.row = rows_along(X),
                      ind.col = cols_along(X),
                      ...,
                      block.size = block_size(length(ind.row)),
                      warn = TRUE) {

  res <- FBM(
    nrow = length(ind.row),
    ncol = length(ind.col),
    init = NULL,
    ...
  )

  # Warn only once
  if (warn) warn_downcast(from = X, to = res)
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

#' @rdname big_copy
#' @export
setMethod(
  "big_copy", signature(x = "matrix"),
  function(x, ...) {

    opt.save <- options(bigstatsr.check.args = FALSE)
    on.exit(options(opt.save), add = TRUE)

    big_copy0(X = x, ...)
  }
)

#' @rdname big_copy
#' @export
setMethod(
  "big_copy", signature(x = "FBM"),
  function(x, type = names(x$type), ...) {

    big_copy0(X = x, type = type, ...)
  }
)

#' @rdname big_copy
#' @export
setMethod(
  "big_copy", signature(x = "big.matrix"),
  function(x, ...) {

    if (!requireNamespace("bigmemory"))
      stop2("You need to install package 'bigmemory'.")

    if (!bigmemory::is.filebacked(x))
      stop2("'x' has to be a FILEBACKED big.matrix.")

    opt.save <- options(bigstatsr.check.args = FALSE)
    on.exit(options(opt.save), add = TRUE)

    big_copy0(X = x, ...)
  }
)

################################################################################

#' Convert to base R matrix
#'
#' Extract values from a Filebacked Big Matrix and convert to a base R matrix.
#'
#' @param x A [FBM][FBM-class] object.
#'
#' @export
#'
setMethod(
  "as.matrix", signature(x = "FBM"),
  function(x) methods::as(x, "matrix")
)
setAs("FBM", "matrix", function(from) from[])

################################################################################
