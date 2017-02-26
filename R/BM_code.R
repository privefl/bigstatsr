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

#' @export
setMethod("describe", signature(x = "BM.code"), function(x) {
  new("BM.code.descriptor",
      description = callNextMethod()@description,
      code = x@code)
})

#' @export
setGeneric("as.BM.code", function(x, code) standardGeneric("as.BM.code"))

#' @export
setMethod('as.BM.code', signature(x = 'big.matrix', code = "numeric"),
          function(x, code) {
            stopifnot(typeof(x) == "raw") # further check.. length 256
            new("BM.code", address = x@address, code = code)
          })

setMethod('as.BM.code', signature(x = 'big.matrix.descriptor', code = "numeric"),
          function(x, code) {
            stopifnot(typeof(x) == "raw")
            new("BM.code", address = x@address, code = code)
          })


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

setMethod("[", signature(x = "BM.code", i = "matrix",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(callNextMethod(), x@code))
