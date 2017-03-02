################################################################################
#### New class ####

#' S4 class BM.code
#'
#' A class for storing and accessing up to 256 values using a RAW "big.matrix".
#'
#' Compared to a [big.matrix][big.matrix-class], it adds a slot `code` which
#' is used as a lookup table of size 256.
#'
#' @param x Either a [big.matrix][big.matrix-class] or
#' a [big.matrix.descriptor][big.matrix.descriptor-class].
#' @param code A numeric vector of length 256. You should contruct it with
#' `rep(NA_real_, 256)` and then replace the values
#' which are of interest for you.
#'
#' @examples
#' X <- big.matrix(10, 10, type = "raw")
#' X[] <- sample(as.raw(0:3), size = length(X), replace = TRUE)
#' X[,]
#'
#' code <- rep(NA_real_, 256)
#' code[1:3] <- c(1, 3, 5)
#'
#' X.code <- as.BM.code(X, code)
#' X.code[,]
#'
#' @aliases BM.code BM.code-class
#' @rdname BM.code
#' @export
setClass("BM.code",
         contains = "big.matrix",
         representation(code = "numeric")
)

#' @rdname BM.code
#' @export
setClass("BM.code.descriptor",
         contains = "big.matrix.descriptor",
         representation(code = "numeric")
)

################################################################################
#### New dim elements ####

#' Dimensions
#'
#' Dimensions of the described `big.matrix` object. This can be used to know
#' the dimensions of the underlying `big.matrix` without having to attach it.
#'
#' @param x A [big.matrix.descriptor][big.matrix.descriptor-class].
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' dim(X.desc)
#' dim(attach.big.matrix(X.desc))
#'
#' @rdname dim-desc
#' @export
setMethod("nrow", signature(x = "big.matrix.descriptor"),
          function(x) x@description$nrow)

#' @rdname dim-desc
#' @export
setMethod("ncol", signature(x = "big.matrix.descriptor"),
          function(x) x@description$ncol)

#' @rdname dim-desc
#' @export
setMethod("dim", signature(x = "big.matrix.descriptor"),
          function(x) c(nrow(x), ncol(x)))

#' @rdname dim-desc
#' @export
setMethod("length", signature(x = "big.matrix.descriptor"),
          function(x) prod(dim(x)))

################################################################################
#### Sequence generation ####

#' Sequence generation
#'
#' Similar to [seq_along], it creates sequences of size `nrow(x)` or `ncol(x)`.
#'
#' @param x Any object on which you can call `nrow` and `ncol`.
#'
#' @examples
#' X.desc <- big_attachExtdata()
#' dim(X.desc)
#' str(rows_along(X.desc))
#' str(cols_along(X.desc))
#'
#' @rdname seq-dim
#' @keywords internal
#' @export
rows_along <- function(x) seq_len(nrow(x))

#' @rdname seq-dim
#' @export
cols_along <- function(x) seq_len(ncol(x))

################################################################################
#### Convert to new type ####

#' @rdname BM.code
#' @export
setGeneric("as.BM.code", function(x, code) standardGeneric("as.BM.code"))

check_BM_code <- function(x, code) {
  if (typeof(x) != "raw") stop("Your `big.matrix` must be of type 'raw'.")
  if (length(code) != 256) stop("'code' must be of length 256.")
}

#' @rdname BM.code
#' @export
setMethod("as.BM.code", signature(x = "big.matrix", code = "numeric"),
          function(x, code) {
            check_BM_code(x, code)
            methods::new("BM.code", address = x@address, code = code)
          })

#' @rdname BM.code
#' @export
setMethod("as.BM.code", signature(x = "big.matrix.descriptor",
                                  code = "numeric"),
          function(x, code) {
            check_BM_code(x, code)
            as.BM.code(attach.big.matrix(x), code)
          })

################################################################################
#### Methods for completeness ####

#' Extend "bigmemory" methods
#'
#' Extend some methods for completeness, so that `big.matrix` objects
#' or their descriptor can be used interchangeably.
#'
#' @param x Either a [big.matrix][big.matrix-class] or
#' a [big.matrix.descriptor][big.matrix.descriptor-class].
#'
#' @rdname completeness-methods
#' @export
setMethod("typeof", signature(x = "big.matrix.descriptor"),
          function(x) x@description$type)

#' @rdname completeness-methods
#' @export
setMethod("describe", signature(x = "big.matrix.descriptor"), identity)

#' @rdname completeness-methods
#' @export
setMethod("describe", signature(x = "BM.code"), function(x) {
  methods::new("BM.code.descriptor",
               description = methods::callNextMethod()@description,
               code = x@code)
})

#' @rdname completeness-methods
#' @export
setGeneric("attach.BM", function(x) standardGeneric("attach.BM"))

#' @rdname completeness-methods
#' @export
setMethod("attach.BM", signature(x = "big.matrix"), identity)

#' @rdname completeness-methods
#' @export
setMethod("attach.BM", signature(x = "big.matrix.descriptor"),
          function(x) attach.big.matrix(x))

#' @rdname completeness-methods
#' @export
setMethod("attach.BM", signature(x = "BM.code.descriptor"),
          function(x) as.BM.code(x, x@code))

################################################################################
#### Accessors for a `BM.code` ####

decode <- function(x, code) `if`(is.matrix(x), decodeMat, decodeVec)(x, code)

#' Extract
#'
#' Extract replace big.matrix elements
#'
#' @param x A [BM.code].
#' @param i Indices specifying the rows.
#' @param j Indices specifying the columns.
#' @param drop Logical indication if reduce to minimum dimensions.
#' @rdname extract-methods
#' @keywords internal
#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "missing", drop = "logical"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "numeric", drop = "missing"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "missing",
                         j = "numeric", drop = "logical"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "missing", drop = "logical"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "numeric", drop = "missing"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "numeric",
                         j = "numeric", drop = "logical"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

#' @rdname extract-methods
#' @export
setMethod("[", signature(x = "BM.code", i = "matrix",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop) decode(methods::callNextMethod(), x@code))

################################################################################
