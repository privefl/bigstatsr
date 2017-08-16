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
#' @exportClass FBM.code256
#' @include FBM.R
#'
FBM.code256_RC <- methods::setRefClass(

  "FBM.code256",

  contains = "FBM",

  fields = list(
    code256 = "vector"
  ),

  methods = list(
    initialize = function(..., code = rep(NA_real_, 256)) {

      # if (length(code) != 256)
      #   stop2("'code' has to be of length 256")
      .self$code256 <- code

      callSuper(type = "unsigned char", ...)
      # super$initialize(nrow, ncol, "unsigned char",  # raw (int in [0:255])
      #                  init, backingfile = tempfile(), save)
    },

    show = function() {
      callSuper("code 256")
    }
  )
)

#' @export
#'
FBM.code256 <- function(nrow, ncol, code, ...) {

  do.call(FBM.code256_RC$new, args = c(as.list(environment()), list(...)))
}

#' @exportMethod '['
setMethod(
  '[', signature(x = "FBM.code256"),
  Extract(
    extract_vector = function(x, i) {
      decodeVec(extractVec(x$address, i), x$code256)
    },
    extract_matrix = function(x, i, j) {
      decodeMat(extractMat(x$address, i, j), x$code256)
    }
  )
)

################################################################################
#### Convert to new type ####

# TODO: find a way to keep this
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
setMethod("as.BM.code", signature(x = "big.matrix", code = "missing"),
          function(x, code) {
            code <- rep(NA_real_, 256)
            check_BM_code(x, code)
            methods::new("BM.code", address = x@address, code = code)
          })

#' @rdname BM.code
#' @export
setMethod("as.BM.code", signature(x = "big.matrix.descriptor",
                                  code = "numeric"),
          function(x, code) {
            check_BM_code(x, code)
            methods::new("BM.code.descriptor",
                         description = x@description, code = code)
          })

#' @rdname BM.code
#' @export
setMethod("as.BM.code", signature(x = "big.matrix.descriptor",
                                  code = "missing"),
          function(x, code) {
            code <- rep(NA_real_, 256)
            check_BM_code(x, code)
            methods::new("BM.code.descriptor",
                         description = x@description, code = code)
          })

# function for tests
asBMcode <- function(x) {
  x <- round(x + 100)
  tmp <- unique(as.vector(x))
  code <- rep(NA_real_, 256)
  code[tmp + 1] <- tmp - 100
  storage.mode(x) <- "raw"
  as.BM.code(as.big.matrix(x), code)
}

################################################################################

