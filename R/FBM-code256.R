#' @export
#' @include FBM.R
#'
FBM.code256_RC <- methods::setRefClass(

  "FBM.code256",

  contains = "FBM",

  fields = list(
    code256 = "vector"
  ),

  methods = list(
    initialize = function(..., code) {

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
