################################################################################

ALL.TYPES <- c(
  "raw"            = 1L,
  "unsigned char"  = 1L,
  "unsigned short" = 2L,
  "integer"        = 4L,
  "float"          = 6L,
  "double"         = 8L
)

ALL.SIZES <- c(
  "raw"            = 1L,
  "unsigned char"  = 1L,
  "unsigned short" = 2L,
  "integer"        = 4L,
  "float"          = 4L,
  "double"         = 8L
)

NIL_PTR <- methods::new("externalptr")

################################################################################

#' Replace extension '.bk'
#'
#' @param path String with extension '.bk'.
#' @param replacement Replacement of '.bk'. Default replaces by nothing.
#' @param stop_if_not_ext If `replacement != ""`, whether to error if
#'   replacement is not an extension (starting with a '.').
#'
#' @return String with extension '.bk' replaced by `replacement`.
#' @export
#'
#' @examples
#' path <- "toto.bk"
#' sub_bk(path)
#' sub_bk(path, ".rds")
sub_bk <- function(path, replacement = "", stop_if_not_ext = TRUE) {
  pattern <- "\\.bk$"
  if (!grepl(pattern, path))
    stop2("Path '%s' must have 'bk' extension.", path)
  if (stop_if_not_ext && nchar(replacement) > 0 && substr(replacement, 1, 1) != ".")
    stop2("Replacement must be an extension starting with '.' if provided.")
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
#'     C++ object for read-only mapping, to be used as a `XPtr<FBM>` in C++ code
#'   - `$extptr`: (internal) use `$address` instead
#'   - `$address_rw`: address of the external pointer containing the underlying
#'     C++ object for read/write mapping, to be used as a `XPtr<FBM_RW>` in C++ code
#'   - `$extptr_rw`: (internal) use `$address_rw` instead
#'   - `$nrow`: number of rows
#'   - `$ncol`: number of columns
#'   - `$type`: (internal) use `type_size` or `type_chr` instead
#'   - `$type_chr`: FBM type as character, e.g. "double"
#'   - `$type_size`: size of FBM type in bytes (e.g. "double" is 8 and "float" is 4)
#'   - `$backingfile` or `$bk`: File with extension 'bk' that stores the numeric
#'     data of the FBM
#'   - `$rds`: 'rds' file (that may not exist) corresponding to the 'bk' file
#'   - `$is_saved`: whether this object is stored in `$rds`?
#'   - `$is_read_only`: whether it is (not) allowed to modify data?
#'
#' And some methods:
#'   - `$save()`: Save the FBM object in `$rds`. Returns the FBM.
#'   - `add_columns(<ncol_add>)`: Add some columns to the FBM by appending the
#'     backingfile with some data. Returns the FBM invisibly.
#'   - `$bm()`: Get this object as a `filebacked.big.matrix`.
#'   - `$bm.desc()`: Get this object as a `filebacked.big.matrix` descriptor.
#'   - `$check_write_permissions()`: Error if the FBM is read-only.
#'
#' @examples
#' mat <- matrix(1:4, 2)
#' X_from_mat <- as_FBM(mat)
#'
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
    extptr_rw = "externalptr",
    nrow = "numeric",
    ncol = "numeric",
    type = "integer",
    backingfile = "character",
    is_read_only = "logical",

    #### Active bindings
    # Same idea as in package phaverty/bigmemoryExtras
    address = function() {

      if (identical(.self$extptr, NIL_PTR)) {
        .self$extptr <- getXPtrFBM(
          path = .self$bk,
          n    = .self$nrow,
          m    = .self$ncol,
          type = .self$type
        )
      }

      .self$extptr
    },
    address_rw = function() {

      .self$check_write_permissions()

      if (identical(.self$extptr_rw, NIL_PTR)) {
        .self$extptr_rw <- getXPtrFBM_RW(
          path = .self$bk,
          n    = .self$nrow,
          m    = .self$ncol,
          type = .self$type
        )
      }

      .self$extptr_rw
    },

    bk = function() .self$backingfile,
    rds = function() sub_bk(.self$bk, ".rds"),
    is_saved = function() file.exists(.self$rds),
    type_chr = function() names(.self$type),
    type_size = function() ALL.SIZES[[.self$type_chr]]
  ),

  methods = list(
    initialize = function(nrow, ncol, type, init, backingfile, create_bk,
                          is_read_only) {

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

      .self$backingfile  <- normalizePath(bkfile)
      .self$nrow         <- nrow
      .self$ncol         <- ncol
      .self$type         <- ALL.TYPES[type]  # keep int and string
      .self$check_dimensions()

      ## init pointers
      .self$extptr <- .self$extptr_rw <- NIL_PTR

      if (!is.null(init)) {
        if (!create_bk) # can only init new FBMs
          stop2("You can't use `init` when using `create_bk = FALSE`.")
        .self$is_read_only <- FALSE
        .self[] <- init
      }
      .self$is_read_only <- is_read_only

      .self
    },

    save = function() {
      saveRDS(.self, .self$rds)
      .self
    },

    add_columns = function(ncol_add, save_again = TRUE) {

      .self$check_write_permissions()

      assert_int(ncol_add)
      size_before <- file.size(bkfile <- .self$bk)
      addColumns(bkfile, .self$nrow, ncol_add, .self$type)

      ncol_after <- .self$ncol + ncol_add
      if ( (file.size(bkfile) / ncol_after) != (size_before / .self$ncol) )
        warning2("Inconsistency of backingfile size after adding columns.")

      .self$ncol <- ncol_after
      if (.self$is_saved && save_again) .self$save()

      ## reset pointers
      .self$extptr <- .self$extptr_rw <- NIL_PTR

      invisible(.self)
    },

    show = function(typeBM) {
      if (missing(typeBM)) typeBM <- names(.self$type)
      cat(sprintf(
        "A %sFilebacked Big Matrix of type '%s' with %s rows and %s columns.\n",
        `if`(.self$is_read_only, "read-only ", ""), typeBM, .self$nrow, .self$ncol))
      invisible(.self)
    },

    bm.desc = function() {

      if (!requireNamespace("bigmemory", quietly = TRUE))
        stop2("Please install package {bigmemory}.")

      if (.self$is_read_only)
        warning2("/!\\ This FBM is supposed to be read-only /!\\")

      dirname <- sub(file.path("", "$"), "", dirname(.self$backingfile))
      n <- .self$nrow + 0
      m <- .self$ncol + 0
      FBM_type <- typeof(.self)

      new("big.matrix.descriptor",
          description = list(
            sharedType = "FileBacked",
            filename   = basename(.self$backingfile),
            dirname    = paste0(dirname, .Platform$file.sep),
            totalRows  = n,
            totalCols  = m,
            rowOffset  = c(0, n),
            colOffset  = c(0, m),
            nrow       = n,
            ncol       = m,
            rowNames   = NULL,
            colNames   = NULL,
            type       = `if`(FBM_type == "unsigned char", "raw", FBM_type),
            separated  = FALSE
          ))
    },
    bm = function() {
      desc <- .self$bm.desc()
      bigmemory::attach.big.matrix(desc)
    },

    check_write_permissions = function() {
      if (.self$is_read_only)
        stop2("This FBM is read-only.")
      if (file.access(.self$backingfile, 2) != 0)
        stop2("You don't have write permissions for this FBM.")
    },
    check_dimensions = function() {
      size <- .self$nrow * .self$ncol
      if (file.size(.self$backingfile) != (size * .self$type_size))
        stop2("Inconsistency between size of backingfile and dimensions.")
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
#' @param is_read_only Whether the FBM is read-only? Default is `FALSE`.
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
                create_bk = TRUE,
                is_read_only = FALSE) {

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
                   backingfile = tempfile(),
                   is_read_only = FALSE) {

  if (is.matrix(x) || is.data.frame(x)) {
    FBM(nrow = nrow(x), ncol = ncol(x), init = x,
        type = type, backingfile = backingfile, is_read_only = is_read_only)
  } else {
    stop2("'as_FBM()' is not implemented for class '%s'.\n%s", class(x),
          "Try to use 'big_copy()' instead. Otherwise, you can open an issue.")
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
        replaceVecOne(x$address_rw, i, value[1])
      } else if (length(value) == length(i)) {
        replaceVec(x$address_rw, i, value)
      } else {
        stop2("'value' must be unique or of the length of 'x[i]'.")
      }
    },

    replace_matrix = function(x, i, j, value) {

      .dim <- c(length(i), length(j))
      if (is.data.frame(value)) {

        if (identical(dim(value), .dim)) {              ## data.frame
          return(replaceDF(x$address_rw, i, j, value))
        }

      } else {

        if (length(value) == 1)                         ## scalar
          return(replaceMatOne(x$address_rw, i, j, value[1]))

        if (is.null(dim(value))) {                      ## vector
          if (length(value) == prod(.dim)) {
            dim(value) <- .dim
            return(replaceMat(x$address_rw, i, j, value))
          }
        } else if (identical(dim(value), .dim)) {       ## matrix
          return(replaceMat(x$address_rw, i, j, value))
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

#' @rdname FBM-methods
#' @export
setMethod("diag", signature(x = "FBM"), function(x) {
  d <- min(dim(x))
  dseq <- seq_len(d)
  ind <- cbind(dseq, dseq)
  x[ind]
})

################################################################################
