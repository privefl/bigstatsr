################################################################################

#' Replace extension 'bk'
#'
#' @param path String with extension 'bk'.
#' @param replacement Replacement of '.bk'. Default replaces by nothing.
#'
#' @return String with extension '.bk' replaced by `replacement`.
#' @export
#'
#' @examples
#' path <- "toto.bk"
#' sub_bk(path)
#' sub_bk(path, ".rds")
sub_bk <- function(path, replacement = "") {
  pattern <- "\\.bk$"
  if (!grepl(pattern, path))
    stop2("Path '%s' must have 'bk' extension.", path)
  sub(pattern, replacement, path)
}

################################################################################

#' Class FBM
#'
#' A reference class for storing and accessing matrix-like data stored in files
#' on disk. This is very similar to Filebacked Big Matrices provided by the
#' **bigmemory** package. Yet, the implementation is lighter.
#'
#' @details
#' An FBM object has many field:
#'   - `$address`: address of the external pointer containing the underlying
#'     C++ object, to be used as a `XPtr<FBM>` in C++ code
#'   - `$extptr`: use `$address` instead
#'   - `$nrow`
#'   - `$ncol`
#'   - `$type`
#'   - `$backingfile` or `$bk`: File with extension 'bk' that stores the numeric
#'     data of the FBM
#'   - `$rds`: 'rds' file (that may not exist) corresponding to the 'bk' file
#'   - `$is_saved`: whether this object stored in `$rds`?
#'
#' And two methods:
#'   - `$save()`: Save the FBM object in `$rds`. Returns the FBM.
#'   - `add_columns(<ncol_add>)`: Add some columns to the FBM by appending the
#'     backingfile with some data. Returns the FBM invisibly.
#'
#' @examples
#' X <- FBM(10, 10)
#' typeof(X)
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
    nrow = "numeric",
    ncol = "numeric",
    type = "integer",
    backingfile = "character",
    is_saved = "logical",

    #### Active bindings
    # Same idea as in package phaverty/bigmemoryExtras
    address = function() {
      if (identical(.self$extptr, methods::new("externalptr"))) { # nil
        .self$extptr <- getXPtrFBM(.self$bk,
                                   .self$nrow,
                                   .self$ncol,
                                   .self$type)
      }
      .self$extptr
    },

    bk = function() .self$backingfile,
    rds = function() sub_bk(.self$bk, ".rds")
  ),

  methods = list(
    initialize = function(nrow, ncol, type, init, backingfile, create_bk) {

      assert_int(nrow)
      assert_int(ncol)
      bkfile <- path.expand(paste0(backingfile, ".bk"))

      if (create_bk) {
        assert_noexist(bkfile)
        assert_dir(dirname(bkfile))
        createFile(bkfile, nrow, ncol, ALL.TYPES[[type]])
      } else {
        assert_exist(bkfile)
      }

      .self$backingfile <- normalizePath(bkfile)
      .self$is_saved    <- FALSE
      .self$nrow        <- nrow
      .self$ncol        <- ncol
      .self$type        <- ALL.TYPES[type]  # keep int and string

      .self$address  # connect once

      if (!is.null(init)) .self[] <- init

      .self
    },

    save = function() {
      saveRDS(.self, .self$rds)
      .self$is_saved <- TRUE
      .self
    },

    add_columns = function(ncol_add) {

      assert_int(ncol_add)
      size_before <- file.size(bkfile <- .self$bk)
      addColumns(bkfile, .self$nrow, ncol_add, .self$type)

      ncol_after <- .self$ncol + ncol_add
      if ( (file.size(bkfile) / ncol_after) != (size_before / .self$ncol) )
        warning2("Inconsistency of backingfile size after adding columns.")

      .self$ncol <- ncol_after
      .self$extptr <- methods::new("externalptr")  ## reinit pointer
      if (.self$is_saved) .self$save()

      invisible(.self)
    },

    show = function(typeBM) {
      if (missing(typeBM)) typeBM <- names(.self$type)
      cat(sprintf(
        "A Filebacked Big Matrix of type '%s' with %s rows and %s columns.\n",
        typeBM, .self$nrow, .self$ncol))
      invisible(.self)
    }
  )
)
FBM_RC$lock("nrow", "type")

################################################################################

#' Wrapper constructor for class `FBM`.
#'
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param type Type of the Filebacked Big Matrix (default is `double`). Either
#' - `"double"` (double precision -- 64 bits)
#' - `"float"` (single precision -- 32 bits)
#' - `"integer"`
#' - `"unsigned short"`: can store integer values from 0 to 65535.
#'   It has vocation to become the basis for a `FBM.code65536`.
#' - `"raw"` or `"unsigned char"`: can store integer values from 0 to 255.
#'   It is the basis for class [FBM.code256][FBM.code256-class] in order to
#'   access 256 arbitrary different numeric values.
#'   It is used in [package **bigsnpr**](https://goo.gl/pHCCmo).
#' @param init Either a single value (e.g. `0`) or as many value as the number
#'   of elements of the FBM. **Default doesn't initialize the matrix.**
#' @param backingfile Path to the file storing the Big Matrix on disk.
#'   **An extension ".bk" will be automatically added.**
#'   Default stores in the temporary directory.
#' @param create_bk Whether to create a backingfile (the default) or use an
#'   existing one (which should be named by the `backingfile` parameter and have
#'   an extension ".bk"). For example, this could be used to convert a
#'   filebacked `big.matrix` from package **bigmemory** to a [FBM][FBM-class]
#'   (see [the corresponding vignette](https://privefl.github.io/bigstatsr/articles/bigstatsr-and-bigmemory.html)).
#'
#' @rdname FBM-class
#'
#' @export
#'
FBM <- function(nrow, ncol,
                type = c("double", "float", "integer",
                         "unsigned short", "unsigned char", "raw"),
                init = NULL,
                backingfile = tempfile(),
                create_bk = TRUE) {

  type <- match.arg(type)
  do.call(methods::new, args = c(Class = "FBM", as.list(environment())))
}

#' Convert to FBM
#'
#' Convert a matrix (or a data frame) to an FBM.
#'
#' @param x A matrix or an data frame (2-dimensional data).
#'
#' @rdname FBM-class
#' @export
#'
#' @seealso [big_copy]
#'
#' @examples
#' X <- FBM(150, 5)
#' X[] <- iris   ## you can replace with a df (factors -> integers)
#' X2 <- as_FBM(iris)
#' identical(X[], X2[])
as_FBM <- function(x, type = c("double", "float", "integer",
                               "unsigned short", "unsigned char", "raw"),
                   backingfile = tempfile()) {

  if (is.matrix(x) || is.data.frame(x)) {
    FBM(nrow = nrow(x), ncol = ncol(x), init = x,
        type = type, backingfile = backingfile)
  } else {
    stop2("'as_FBM()' is not implemented for class '%s'. %s",
          class(x), "Feel free to open an issue.")
  }
}

################################################################################

#' Methods for the FBM class
#'
#' @name FBM-methods
#'
#' @rdname FBM-methods
NULL

#' Accessor methods for class `FBM`. You can use positive and negative indices,
#' logical indices (that are recycled) and also a matrix of indices (but only
#' positive ones).
#'
#' @param x A [FBM][FBM-class] object.
#' @param i A vector of indices (or nothing). You can use positive and negative
#'   indices, logical indices (that are recycled) and also a matrix of indices
#'   (but only positive ones).
#' @param j A vector of indices (or nothing). You can use positive and negative
#'   indices, logical indices (that are recycled).
#' @param ... Not used. Just to make [nargs] works.
#' @param drop Whether to delete the dimensions of a matrix which have
#'   one dimension equals to 1.
#'
#' @rdname FBM-methods
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

#' @param value The values to replace. Should be of length 1 or of the same
#'   length of the subset to replace.
#' @rdname FBM-methods
#' @export
setMethod(
  '[<-', signature(x = "FBM"),
  Replace(
    replace_vector = function(x, i, value) {

      if (length(value) == 1) {
        replaceVecOne(x$address, i, value[1])
      } else if (length(value) == length(i)) {
        replaceVec(x$address, i, value)
      } else {
        stop2("'value' must be unique or of the length of 'x[i]'.")
      }
    },

    replace_matrix = function(x, i, j, value) {

      .dim <- c(length(i), length(j))
      if (is.data.frame(value)) {

        if (identical(dim(value), .dim)) {              ## data.frame
          return(replaceDF(x$address, i, j, value))
        }

      } else {

        if (length(value) == 1)                         ## scalar
          return(replaceMatOne(x$address, i, j, value[1]))

        if (is.null(dim(value))) {                      ## vector
          if (length(value) == prod(.dim)) {
            dim(value) <- .dim
            return(replaceMat(x$address, i, j, value))
          }
        } else if (identical(dim(value), .dim)) {       ## matrix
          return(replaceMat(x$address, i, j, value))
        }
      }

      stop2("'value' must be unique or of the dimension of 'x[i, j]'.")
    }
  )
)

################################################################################

#' Dimension and type methods for class `FBM`.
#'
#' @rdname FBM-methods
#' @export
setMethod("dim",    signature(x = "FBM"), function(x) c(x$nrow, x$ncol))

#' @rdname FBM-methods
#' @export
setMethod("length", signature(x = "FBM"), function(x) prod(dim(x)))

#' @rdname FBM-methods
#' @export
setMethod("typeof", signature(x = "FBM"), function(x) names(x$type))

################################################################################
