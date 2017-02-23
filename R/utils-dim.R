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
setMethod('dim', signature(x = "big.matrix.descriptor"),
          function(x) c(nrow(x), ncol(x)))

#' @export
#' @keywords internal
setMethod('length', signature(x = "big.matrix.descriptor"),
          function(x) prod(dim(x)))

#' @export
#' @keywords internal
rows_along <- function(x) seq_len(nrow(x))

#' @export
#' @keywords internal
cols_along <- function(x) seq_len(ncol(x))

#' @export
#' @keywords internal
setGeneric('address', function(x) {
  standardGeneric('address')
})

#' @export
#' @keywords internal
setMethod('address', signature(x = 'big.matrix'), function(x) x@address)

#' @export
#' @keywords internal
setMethod('address', signature(x = 'big.matrix.descriptor'),
          function(x) address(attach.big.matrix(x)))

#' @export
#' @keywords internal
setMethod('describe', signature(x = 'big.matrix.descriptor'), identity)

#' @export
#' @keywords internal
setMethod('typeof', signature(x = 'big.matrix.descriptor'),
          function(x) x@description$type)

#' @export
#' @keywords internal
setGeneric('attach.BM', function(x) {
  standardGeneric('attach.BM')
})

#' @export
#' @keywords internal
setMethod('attach.BM', signature(x = 'big.matrix'), identity)

#' @export
#' @keywords internal
setMethod('attach.BM', signature(x = 'big.matrix.descriptor'),
          function(x) attach.big.matrix(x))
