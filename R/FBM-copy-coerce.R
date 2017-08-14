#' Title
#'
#' @param X
#' @param ind.row
#' @param ind.col
#' @param type
#' @param ...
#' @param block.size
#' @param ncores
#'
#' @return
#' @export
#'
#' @examples
big_copy <- function(X, ind.row = rows_along(X),
                     ind.col = cols_along(X),
                     type = X$description$type,
                     ...,
                     block.size = block_size(length(ind.row)),
                     ncores = 1) {

  X2 <- new_FBM(
    nrow = length(ind.row),
    ncol = length(ind.col),
    type = type,
    init = NULL,
    ...
  )

  # Warn only once
  warn_downcast(from = X, to = X2)
  opt.save <- options(bigstatsr.typecast.warning = FALSE)
  on.exit(options(opt.save), add = TRUE)

  big_apply2(ind = seq_along(ind.col), a.FUN = function(X, X2, ind,
                                                        ind.row, ind.col) {
    X2[, ind] <- X[ind.row, ind.col[ind]]
    NULL
  }, a.combine = 'c', block.size = block.size, ncores = ncores,
  X = X, X2 = X2, ind.row = ind.row, ind.col = ind.col)

  X2
}

# TODO: use dots
#' @export
setGeneric("as.FBM", function(x, ...) {
  standardGeneric("as.FBM")
})


#' @export
setMethod("as.FBM", signature(x = "matrix"),
          function(x, type = "double", ...) big_copy(X = x, type = type, ...))

# # TODO: finish this with CRAN version of bigmemory
# #' @export
# setMethod("as.FBM", signature(x = "big.matrix"),
#           function(x) {
#             if (!requireNamespace("bigmemory"))
#               stop2("You need to install package 'bigmemory'.")
#
#             if (!is.filebacked(x))
#               stop2("'x' has to be a FILEBACKED big.matrix.")
#
#             new_FBM()
#           })

