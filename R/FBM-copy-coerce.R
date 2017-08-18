################################################################################

#' Methods to copy an object to a FBM
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
                      ncores = 1,
                      warn = TRUE) {

  res <- FBM(
    nrow = length(ind.row),
    ncol = length(ind.col),
    init = NULL,
    ...
  )

  # Warn only once
  warn_downcast(from = X, to = res)
  opt.save <- options(bigstatsr.typecast.warning = FALSE)
  on.exit(options(opt.save), add = TRUE)

  big_apply(ind = seq_along(ind.col),
            a.FUN = function(X, X2, ind, ind.row, ind.col) {
              X2[, ind] <- X[ind.row, ind.col[ind]]
              NULL
            }, a.combine = 'c', block.size = block.size, ncores = ncores,
            X = X, X2 = res, ind.row = ind.row, ind.col = ind.col)

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

    if (!is.filebacked(x))
      stop2("'x' has to be a FILEBACKED big.matrix.")

    opt.save <- options(bigstatsr.check.args = FALSE)
    on.exit(options(opt.save), add = TRUE)

    big_copy0(X = x, ...)
  }
)

################################################################################

#' @exportMethod as.matrix
setMethod(
  "as.matrix", signature(x = "FBM"),
  function(x) as(x, "matrix")
)
setAs("FBM", "matrix", function(from) from[])

################################################################################
