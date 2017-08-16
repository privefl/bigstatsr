TYPES <- structure(c(1L, 1L, 2L, 4L, 8L),
                   names = c("raw", "unsigned char", "unsigned short",
                             "integer", "double"))


#' @exportClass FBM
FBM_RC <- setRefClass(

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
                          save = TRUE) {

      c(nrow, ncol)  # check they are not missing
      type <- match.arg(type)
      bkfile <- path.expand(paste0(backingfile, ".bk"))

      createFile(bkfile, nrow, ncol, TYPES[[type]])

      .self$backingfile <- normalizePath(bkfile)
      .self$nrow        <- as.integer(nrow)
      .self$ncol        <- as.integer(ncol)
      .self$type        <- TYPES[type]  # keep int and string

      .self$address  # connect once

      if (!is.null(init)) .self[] <- init

      if (save) .self$save()
    },

    save = function() {
      saveRDS(.self, sub("\\.bk$", ".rds", .self$backingfile))
    }

    show = function(type) {
      if (missing(type)) type <- names(.self$type)
      print(glue::glue("A Filebacked Big Matrix of type '{type}'",
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
                    save = TRUE) {

  do.call(FBM_RC$new, args = as.list(environment()))
}

# TODO: change this to big_attach afterwards
#' @export
attach_FBM <- function(rdsfile) {

  rdsfile <- normalizePath(rdsfile)
  fbm <- readRDS(rdsfile)

  if (!file.exists(fbm$backingfile <- sub("\\.rds$", ".bk", rdsfile)))
    stop2("Can't find the backingfile associated with this FBM.")

  fbm
}

#' @exportMethod '['
setMethod(
  '[', signature(x = "FBM"),
  Extract(
    extract_vector = function(x, i) extractVec(x$address, i),
    extract_matrix = function(x, i, j) extractMat(x$address, i, j)
  )
)

#' @exportMethod '[<-'
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
