
#' @exportClass FBM
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
                          save = FALSE) {

      c(nrow, ncol)  # check they are not missing
      typeBM <- match.arg(type)
      bkfile <- path.expand(paste0(backingfile, ".bk"))

      createFile(bkfile, nrow, ncol, ALL.TYPES[[typeBM]])

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


# TODO: change this to FBM afterwards and use dots
#' @export
new_FBM <- function(nrow, ncol,
                    type = c("double", "integer", "unsigned short",
                             "unsigned char", "raw"),
                    init = NULL,
                    backingfile = tempfile(),
                    save = FALSE) {

  do.call(FBM_RC$new, args = as.list(environment()))
}

#' @exportMethod '['
#' @include crochet.R
setMethod(
  '[', signature(x = "FBM"),
  Extract(
    extract_vector = function(x, i) extractVec(x$address, i),
    extract_matrix = function(x, i, j) extractMat(x$address, i, j)
  )
)

#' @exportMethod '[<-'
#' @include crochet.R
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


#' @exportMethod dim
setMethod(
  "dim", signature(x = "FBM"),
          function(x) {
            c(x$nrow, x$ncol)
          }
  )

#' @exportMethod length
setMethod(
  "length", signature(x="FBM"),
          function(x) {
            prod(dim(x))
          }
  )

#' @export
setMethod(
  "typeof", signature(x = "FBM"),
          function(x) {
            names(x$type)
          }
  )

#' @exportMethod as.matrix
setMethod(
  "as.matrix", signature(x = "FBM"),
  function(x) as(x, "matrix")
  )
setAs("FBM", "matrix", function(from) from[])
