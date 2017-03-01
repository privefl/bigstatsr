################################################################################
#### New class ####

#' @export
setClass("BM.code",
         contains = "big.matrix",
         representation(code = "numeric")
)

#' @export
setClass("BM.code.descriptor",
         contains = "big.matrix.descriptor",
         representation(code = "numeric")
)

################################################################################
#### New dim elements ####

#' @export
setMethod("nrow", signature(x = "big.matrix.descriptor"),
          function(x) x@description$nrow)

#' @export
setMethod("ncol", signature(x = "big.matrix.descriptor"),
          function(x) x@description$ncol)

#' @export
setMethod("dim", signature(x = "big.matrix.descriptor"),
          function(x) c(nrow(x), ncol(x)))

#' @export
setMethod("length", signature(x = "big.matrix.descriptor"),
          function(x) prod(dim(x)))

#' @export
rows_along <- function(x) seq_len(nrow(x))

#' @export
cols_along <- function(x) seq_len(ncol(x))

################################################################################
#### Convert to new type ####

#' @export
setGeneric("as.BM.code", function(x, code) standardGeneric("as.BM.code"))

check_BM_code <- function(x, code) {
  if (typeof(x) != "raw") stop("Your `big.matrix` must be of type 'raw'.")
  if (length(code) != 256) stop("'code' must be of length 256.")
}

#' @export
setMethod("as.BM.code", signature(x = "big.matrix", code = "numeric"),
          function(x, code) {
            check_BM_code(x, code)
            new("BM.code", address = x@address, code = code)
          })

#' @export
setMethod("as.BM.code", signature(x = "big.matrix.descriptor",
                                  code = "numeric"),
          function(x, code) {
            check_BM_code(x, code)
            as.BM.code(attach.big.matrix(x), code)
          })

################################################################################
#### Methods for completeness ####

#' @export
setMethod("typeof", signature(x = "big.matrix.descriptor"),
          function(x) x@description$type)

#' @export
setMethod("describe", signature(x = "big.matrix.descriptor"), identity)

#' @export
setMethod("describe", signature(x = "BM.code"), function(x) {
  new("BM.code.descriptor",
      description = callNextMethod()@description,
      code = x@code)
})

#' @export
setGeneric("attach.BM", function(x) standardGeneric("attach.BM"))

#' @export
setMethod("attach.BM", signature(x = "big.matrix"), identity)

#' @export
setMethod("attach.BM", signature(x = "big.matrix.descriptor"),
          function(x) attach.big.matrix(x))

#' @export
setMethod("attach.BM", signature(x = "BM.code.descriptor"),
          function(x) as.BM.code(x, x@code))

################################################################################
#### Accessors for a `BM.code` ####

decode <- function(x, code) `if`(is.matrix(x), decodeMat, decodeVec)(x, code)

#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "missing", drop = "logical"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "numeric", drop = "missing"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "numeric", drop = "logical"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "missing", drop = "logical"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "numeric", drop = "missing"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "numeric", drop = "logical"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

#' @export
setMethod("[", signature(x = "BM.code", i = "matrix",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))

################################################################################
