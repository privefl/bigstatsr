#' @export
setClass("BM",
         contains = "big.matrix",
         representation(ind.row = "integer",
                        ind.col = "integer")
)

#' @export
setClass("BM.descriptor",
         contains = "big.matrix.descriptor",
         representation(ind.row = "integer",
                        ind.col = "integer")
)

#' @export
setMethod("describe", signature(x = "BM"), function(x) {
  new("BM.descriptor",
      description = callNextMethod()@description,
      ind.row = x@ind.row,
      ind.col = x@ind.col)
})

#' @export
setGeneric("as.BM", function(x) standardGeneric("as.BM"))

setMethod('as.BM', signature(x = 'big.matrix'), function(x) {
            new("BM", address = x@address,
                ind.row = seq_len(nrow(x)),
                ind.col = seq_len(ncol(x)))
          })

setGeneric("sub.BM", function(x) standardGeneric("sub.BM"))

setMethod('as.BM', signature(x = 'big.matrix'), function(x) {
  new("BM", address = x@address,
      ind.row = seq_len(nrow(x)),
      ind.col = seq_len(ncol(x)))
})
# setMethod('subset', signature(x = 'BM'),
#           function(x, ind.row = seq_len(nrow(x)), ind.col = seq_len(ncol(x))) {
#             new("BM", address = x@address,
#                 ind.row = x@ind.row[ind.row],
#                 ind.col = x@ind.col[ind.col])
#           })

################################################################################

#' Dimensions of a "BM" object
#'
#' @param x A `BM`.
#'
#' @rdname dim-methods2
#' @export
setMethod('dim', signature(x = "BM"), function(x) c(nrow(x), ncol(x)))

################################################################################

#' @rdname dim-methods2
#' @export
setMethod('nrow', signature(x = "BM"), function(x) length(x@ind.row))

################################################################################

#' @rdname dim-methods2
#' @export
setMethod('ncol', signature(x = "BM"), function(x) length(x@ind.col))

################################################################################

#' @rdname dim-methods2
#' @export
setMethod('length', signature(x = "BM"), function(x) prod(dim(x)))

################################################################################
