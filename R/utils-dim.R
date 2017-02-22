#' @export
#' @keywords internal
setMethod('nrow', signature(x = "big.matrix.descriptor"),
          function(x) x@description$nrow)

#' @export
#' @keywords internal
setMethod('ncol', signature(x = "big.matrix.descriptor"),
          function(x) x@description$ncol)

#' @export
#' @keywords internal
rows_along <- function(x) seq_len(nrow(x))

#' @export
#' @keywords internal
cols_along <- function(x) seq_len(ncol(x))
