################################################################################

#' Class FBM.code256
#'
#' A reference class for storing and accessing up to 256 arbitrary different
#' values using a Filebacked Big Matrix of type `unsigned char`. Compared to a
#' [Filebacked Big Matrix][FBM-class], it adds a slot `code` which is used as
#' a lookup table of size 256.
#'
#' @param x A [FBM][FBM-class].
#' @param code A numeric vector (of length 256).
#' You should construct it with `rep(NA_real_, 256)` and then replace the values
#' which are of interest for you.
#' @inheritParams FBM
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
#' # Copy the FBM with another code
#' X.code3 <- X.code$copy(code = rnorm(256))
#' all.equal(X.code$code256, code)
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
    initialize = function(..., code) {
      .self$code256 <- code
      callSuper(...)
    },

    copy = function(code = .self$code256) {
      add_code256(.self, code = code)
    },

    as.FBM = function() {
      FBM(
        nrow = .self$nrow,
        ncol = .self$ncol,
        type = "unsigned char",
        init = NULL,
        backingfile = sub_bk(.self$backingfile),
        create_bk = FALSE,
        is_read_only = .self$is_read_only
      )
    },

    show = function() {
      callSuper("code 256")
    }
  )
)

################################################################################

#' Wrapper constructor for class `FBM.code256`.
#'
#' @inheritParams FBM
#'
#' @rdname FBM.code256-class
#'
#' @export
#'
FBM.code256 <- function(nrow, ncol,
                        code = rep(NA_real_, 256),
                        init = NULL,
                        backingfile = tempfile(),
                        create_bk = TRUE,
                        is_read_only = FALSE) {

  if (length(code) != 256)
    stop("'code' must be of length 256.")

  do.call(methods::new, args = c(Class = "FBM.code256",
                                 type = "unsigned char",
                                 as.list(environment())))
}

#' Converter from class `FBM` to `FBM.code256`.
#'
#' @rdname FBM.code256-class
#'
#' @export
#'
add_code256 <- function(x, code) {

  if (x$type != 1)
    stop2("'x' must be of type 'raw' (unsigned char)")

  FBM.code256(
    nrow = x$nrow,
    ncol = x$ncol,
    code = code,
    init = NULL,
    backingfile = sub_bk(x$backingfile),
    create_bk = FALSE,
    is_read_only = x$is_read_only
  )
}

################################################################################

#' Methods for the FBM.code256 class
#'
#' @name FBM.code256-methods
#'
#' @rdname FBM.code256-methods
NULL

#' Accessor method for class `FBM.code256`.
#'
#' @param x A [FBM.code256][FBM.code256-class].
#' @inheritParams FBM-methods
#'
#' @rdname FBM.code256-methods
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

#' Counts
#'
#' Counts by columns (or rows) the number of each unique element of a
#' `FBM.code256`.
#'
#' @inheritParams bigstatsr-package
#' @param byrow Count by rows rather than columns?
#'   Default is `FALSE` (columns).
#'
#' @example examples/example-counts.R
#'
#' @return A matrix of counts of K x m (or n) elements, where
#' - K is the number of unique elements of the `BM.code`,
#' - n is its number of rows,
#' - m is its number of columns.
#'
#' **Beware that K is up to 256. So, if you apply this on a Filebacked Big
#' Matrix of one million columns, you will create a matrix of nearly 1GB!**.
#' @export
big_counts <- function(X.code,
                       ind.row = rows_along(X.code),
                       ind.col = cols_along(X.code),
                       byrow = FALSE) {

  check_args(X.code = "assert_class(X.code, 'FBM.code256')")

  code <- X.code$code256
  code.uniq <- unique(code)
  ind.code <- match(code, code.uniq)

  if (byrow) {
    res <- mycount1(X.code, ind.row, ind.col, ind.code)
    assert_all(colSums(res), length(ind.col))
  } else {
    res <- mycount2(X.code, ind.row, ind.col, ind.code)
    assert_all(colSums(res), length(ind.row))
  }
  rownames(res) <- code.uniq

  res
}

################################################################################
