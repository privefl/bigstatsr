################################################################################

#' Class FBM
#'
#' A reference class for storing and accessing matrix-like data stored in files
#' on disk. This is very similar to Filebacked Big Matrices provided by the
#' **bigmemory** package. Yet, the implementation is much more lighter.
#'
#' @param x A [FBM.code256][FBM.code256-class].
#' @param code A numeric vector (of length 256).
#' You should contruct it with `rep(NA_real_, 256)` and then replace the values
#' which are of interest for you.
#'
#' @examples
#' X <- FBM(10, 10)
#' typeof(X)
#' X[]
#' X[] <- rnorm(length(X))
#' X[, 1:6]
#' X[] <- 1:100
#' X[, 1]
#' X[1, ]  # not recommended for large matrices
#' X[, -1]
#' X[, c(TRUE, FALSE)]
#' X[cbind(1:10, 1:10)] <- NA_real_
#' X[]
#'
#' @exportClass FBM
#'
FBM_RC <- methods::setRefClass(

  "FBM",

  fields = list(
    extptr = "externalptr",
    nrow = "integer",
    ncol = "integer",
    type = "integer",
    backingfile = "character",

    address = function() {
      if (identical(.self$extptr, new("externalptr"))) { # nil
        .self$extptr <- getXPtrFBM(.self$description)
      }
      .self$extptr
    },

    description = function() {
      list(
        backingfile = .self$backingfile,
        nrow        = .self$nrow,
        ncol        = .self$ncol,
        type        = .self$type
      )
    }
  ),

  methods = list(
    initialize = function(nrow, ncol,
                          type = c("double", "integer", "unsigned short",
                                   "unsigned char", "raw"),
                          init = NULL,
                          backingfile = tempfile(),
                          create_bk = TRUE,
                          save = FALSE) {

      c(nrow, ncol)  # check they are not missing
      typeBM <- match.arg(type)
      bkfile <- path.expand(paste0(backingfile, ".bk"))

      if (create_bk) {
        assert_noexist(bkfile)
        createFile(bkfile, nrow, ncol, ALL.TYPES[[typeBM]])
      } else {  # already exists
        assert_exist(bkfile)
      }

      .self$backingfile <- normalizePath(bkfile)
      .self$nrow        <- as.integer(nrow)
      .self$ncol        <- as.integer(ncol)
      .self$type        <- ALL.TYPES[typeBM]  # keep int and string

      .self$address  # connect once

      if (!is.null(init)) .self[] <- init

      if (save) .self$save()
    },

    save = function() {
      saveRDS(.self, sub("\\.bk$", ".rds", .self$backingfile))
    },

    show = function(typeBM) {
      if (missing(typeBM)) typeBM <- names(.self$type)
      print(glue::glue("A Filebacked Big Matrix of type '{typeBM}'",
                       " with {.self$nrow} rows and {.self$ncol} columns."))
      invisible(.self)
    }
  )
)
FBM_RC$lock("nrow", "ncol", "type")

################################################################################

#' Wrapper constructor for class `FBM`.
#'
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param type Type of the big.matrix (default is `double`). Either
#' - `"double"`
#' - `"integer"`
#' - `"unsigned short"`: can store integer values from 0 to 65535.
#'   It has vocation to become the basis for a `FBM.code65536` class for
#'   accessing strings.
#' - `"raw"` or `"unsigned char"`: can store integer values from 0 to 255.
#'   It is the basis for the [FBM.code256][FBM.code256-class] class for
#'   accessing 256 arbitrary different numeric values.
#'   It is used in [package **bigsnpr**](https://goo.gl/pHCCmo).
#' @param init Either a single value (e.g. `0`) or as many value as the number
#'   of elements of the FBM. **Default doesn't initialize the matrix.**
#' @param backingfile Path to the file storing the Big Matrix on disk.
#'   An extension ".bk" will be automatically added. Default stores in the
#'   temporary directory.
#' @param save Whether to save the result object in an ".rds" file alongside
#'   the backingfile. Default is `FALSE`.
#'
#' @rdname FBM-class
#'
#' @export
#'
FBM <- function(nrow, ncol,
                type = c("double", "integer", "unsigned short",
                         "unsigned char", "raw"),
                init = NULL,
                backingfile = tempfile(),
                save = FALSE) {

  do.call(FBM_RC$new, args = as.list(environment()))
}

################################################################################

#' Accessor methods for class `FBM`. You can use positive and negative indices,
#' logical indices (that are recycled) and also a matrix of indices (but only
#' positive ones).
#'
#' @rdname FBM-class
#'
#' @include crochet.R
#'
#' @export
#'
setMethod(
  '[', signature(x = "FBM"),
  Extract(
    extract_vector = function(x, i) extractVec(x$address, i),
    extract_matrix = function(x, i, j) extractMat(x$address, i, j)
  )
)

#' @rdname FBM-class
#' @export
setMethod(
  '[<-', signature(x = "FBM"),
  Replace(
    replace_vector = function(x, i, value) {
      if (length(value) == 1) {
        replaceVecOne(x$address, i, value)
      } else if (length(value) == length(i)) {
        replaceVec(x$address, i, value)
      } else {
        stop2("'value' must be unique or of the length of 'x[i]'.")
      }
    },

    replace_matrix = function(x, i, j, value) {
      if (length(value) == 1) {
        replaceMatOne(x$address, i, j, value)
      } else {
        .dim <- c(length(i), length(j))
        if (length(value) == prod(.dim)) {
          dim(value) <- .dim
          replaceMat(x$address, i, j, value)
        } else {
          stop2("'value' must be unique or of the dimension of 'x[i, j]'.")
        }
      }
    }
  )
)

################################################################################

#' Dimension and type methods for class `FBM`.
#'
#' @rdname FBM-class
#' @export
setMethod("dim",    signature(x = "FBM"), function(x) c(x$nrow, x$ncol))

#' @rdname FBM-class
#' @export
setMethod("length", signature(x = "FBM"), function(x) prod(dim(x)))

#' @rdname FBM-class
#' @export
setMethod("typeof", signature(x = "FBM"), function(x) names(x$type))

################################################################################
