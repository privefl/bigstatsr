#' @export
setClass("bigFmatrix",
         contains = "big.matrix",
         representation(backingpath = "character",
                        temp = "logical")
)

#' @export
setClass("bigFmatrix.descriptor",
         contains = "big.matrix.descriptor",
         representation(backingpath = "character",
                        temp = "logical")
)

#' @export
setGeneric("dir.name", function(x) standardGeneric("dir.name"))

setMethod("dir.name", signature(x = "bigFmatrix"), function(x) {
  x@backingpath
})

#' @export
setGeneric("path.name", function(x) standardGeneric("path.name"))

setMethod("path.name", signature(x = "bigFmatrix"), function(x) {
  file.path(dir.name(x), file.name(x))
})


#' @export
setGeneric("is.temp", function(x) standardGeneric("is.temp"))

setMethod("is.temp", signature(x = "bigFmatrix"), function(x) {
  x@temp
})

#' @export
setMethod("describe", signature(x = "bigFmatrix"), function(x) {
  new("bigFmatrix.descriptor",
      description = callNextMethod()@description,
      backingpath = x@backingpath,
      temp = x@temp)
})

#' @export
bigFmatrix <- function(nrow, ncol, type = options()$bigmemory.default.type,
                       init = NULL, backingfile,
                       backingpath = "backingfiles",
                       temp = FALSE) {
  if (!dir.exists(backingpath)) dir.create(backingpath)

  tmp <- filebacked.big.matrix(nrow, ncol, type = type, init = init,
                               backingfile = backingfile,
                               backingpath = backingpath,
                               descriptorfile = paste0(backingfile, ".desc"))

  new("bigFmatrix",
      address = tmp@address,
      backingpath = backingpath,
      temp = temp)
}

#' @export
bigTFmatrix <- function(nrow, ncol, type = options()$bigmemory.default.type,
                        init = NULL, backingfile = basename(tempfile()),
                        backingpath = file.path("backingfiles", "tmp"),
                        temp = TRUE) {
  bigFmatrix(nrow, ncol, type = type, init = init,
             backingfile = backingfile,
             backingpath = backingpath,
             temp = temp)
}

#' @export
attach.bigFmatrix = function(obj, ...)
{
  descriptorfile <- basename(obj)
  backingpath <- dirname(obj)
  tmp <- attach.big.matrix(descriptorfile, backingpath  = backingpath, ...)
  new("bigFmatrix",
      address = tmp@address,
      backingpath = backingpath,
      temp = FALSE)
}


#' @export
setGeneric('sub.bigFmatrix', function(x, firstRow = 1, lastRow = NULL,
                                      firstCol = 1, lastCol = NULL)
  standardGeneric('sub.bigFmatrix')
)


setMethod('sub.bigFmatrix', signature(x = 'bigFmatrix'),
          function(x, firstRow, lastRow, firstCol, lastCol) {
            sub.bigFmatrix(describe(x), firstRow, lastRow, firstCol, lastCol)
          })

#' @param x A descriptor object
#' @param firstRow the first row of the submatrix
#' @param lastRow the last row of the submatrix if not NULL
#' @param firstCol the first column of the submatrix
#' @param lastCol of the submatrix if not NULL
setMethod('sub.bigFmatrix', signature(x = 'bigFmatrix.descriptor'),
          function(x, firstRow, lastRow, firstCol, lastCol) {
            backingpath <- x@backingpath
            tmp <- sub.big.matrix(x, firstRow, lastRow,
                                  firstCol, lastCol,
                                  backingpath)
            new("bigFmatrix",
                address = tmp@address,
                backingpath = backingpath,
                temp = x@temp)
          })

setGeneric("rmT", function(x) standardGeneric("rmT"))

setMethod("rmT", signature(x = "bigFmatrix"), function(x) {
  if (is.sub.big.matrix(x)) {
    warning("You can't delete a sub.bigFmatrix.")
  } else {
    if (x@temp) {
      a <- deparse(substitute(x))
      path <- path.name(x)
      rm(list = a, pos = 1)
      unlink(paste0(path, c("", ".desc")))
    } else {
      warning("You can't delete a non-temporary bigFmatrix. Use only `rm` instead.")
    }
  }
})
