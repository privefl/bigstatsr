################################################################################

#' Class FBM.code256
#'
#' A reference class for storing and accessing up to 256 arbitrary different
#' values using a Filebacked Big Matrix of type `unsigned char`. Compared to a
#' [Filebacked Big Matrix][FBM-class], it adds a slot `code` which is used as
#' a lookup table of size 256.
#'
#' @param x A [FBM.code256][FBM.code256-class].
#' @param code A numeric vector (of length 256).
#' You should contruct it with `rep(NA_real_, 256)` and then replace the values
#' which are of interest for you.
#'
#' @examples
#' X <- FBM(10, 10, type = "raw")
#' X[] <- sample(as.raw(0:3), size = length(X), replace = TRUE)
#' X[]
#'
#' code <- rep(NA_real_, 256)
#' code[1:3] <- c(1, 3, 5)
#'
#' X.code <- add_code256(X, code)
#' X.code[]
#'
#' # Or directly
#' X.code2 <- FBM.code256(10, 10, code, init = sample(as.raw(0:3), 100, TRUE))
#' X.code2[]
#'
#' @include FBM.R
#'
#' @exportClass FBM.code256
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

################################################################################

#' Wrapper constructor for class `FBM.code256`.
#'
#' @rdname FBM.code256-class
#'
#' @export
#'
FBM.code256 <- function(nrow, ncol, code, ...) {

  do.call(FBM.code256_RC$new, args = c(as.list(environment()), list(...)))
}

#' Accessor method for class `FBM.code256`.
#'
#' @rdname FBM.code256-class
#'
#' @export
#'
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

#' Converter from class `FBM` to `FBM.code256`.
#'
#' @rdname FBM.code256-class
#'
#' @export
#'
add_code256 <- function(x, code, save = FALSE) {

  if (x$type != 1)
    stop2("'x' must be of type 'unsigned char'")
  # if (length(code) != 256) stop("'code' must be of length 256.")

  FBM.code256_RC$new(
    nrow = x$nrow,
    ncol = x$ncol,
    init = NULL,
    backingfile = sub("\\.bk$", "", x$backingfile),
    create_bk = FALSE,
    save = save,
    code = code
  )
}


# function for tests
asFBMcode <- function(x) {
  x <- round(x + 100)
  tmp <- unique(as.vector(x))
  code <- rep(NA_real_, 256)
  code[tmp + 1] <- tmp - 100
  storage.mode(x) <- "raw"
  add_code256(big_copy(x, type = "raw"), code)
}

################################################################################

