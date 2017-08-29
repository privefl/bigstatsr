TYPES <- structure(c(1, 1, 2, 4, 8),
                   names = c("raw", "unsigned char", "unsigned short",
                             "integer", "double"))

#' @export
#'
FBM_R6Class <- R6::R6Class(

  "FBM",

  public = list(
    print = function(type) {
      if (missing(type)) type <- names(private$type)
      print(glue::glue("A Filebacked Big Matrix of type '{type}'",
                       " with {private$nrow} rows and {private$ncol} columns."))
      invisible(self)
    },

    initialize = function(nrow, ncol,
                          type = c("double", "integer", "unsigned short",
                                   "unsigned char", "raw"),
                          init = NULL,
                          backingfile = tempfile(),
                          save = TRUE) {

      bkfile <- path.expand(paste0(backingfile, ".bk"))
      type <- match.arg(type)

      createFile(bkfile, nrow, ncol, TYPES[[type]])

      private$backingfile <- normalizePath(bkfile)
      private$nrow        <- nrow
      private$ncol        <- ncol
      private$type        <- TYPES[type]  # keep int and string

      self$address  # connect once

      if (!is.null(init)) self[] <- init

      if (save) saveRDS(self, sub("\\.bk$", ".rds", bkfile))
    },

    set_backingfile = function(backingfile) {
      private$backingfile <- normalizePath(backingfile)
      invisible(self)
    },

    extract = function(i, j, drop = TRUE) {

      n <- private$nrow
      m <- private$ncol

      print(nargs <- nargs() - !missing(drop))
      print(c(missing(i), missing(j)))

      if (nargs == 1) {  # only i
        if (missing(i)) {
          nargs <- 2  # x[] is the same as x[,]
        } else {
          return(extractVec(self$address, transform_i_only(i, n, m)))
        }
      }
      if (nargs == 3) {
        res <- extractMat(self$address,
                          transform_ind(i, n),
                          transform_ind(j, m))
        return(`if`(drop, drop(res), res))
      }
    }

    # finalize = function() {
    #   cat("Free some memory!\n")
    #   freeFBM(self$address)
    # }
  ),

  active = list(
    address = function() {
      if (identical(private$extptr, new("externalptr"))) { # nil
        private$extptr <- getXPtrFBM(self$description)
      }
      private$extptr
    },

    description = function() {
      list(
        backingfile = private$backingfile,
        nrow        = private$nrow,
        ncol        = private$ncol,
        type        = private$type
      )
    }
  ),

  private = list(
    extptr = new("externalptr"),
    nrow = NULL,
    ncol = NULL,
    type = NULL,
    backingfile = NULL
  ),

  lock_class = TRUE,
  lock_objects = TRUE
)

methods::setOldClass(c("FBM", "R6"))

#' @export
setMethod('typeof', signature(x = "FBM"),
          function(x) names(x$description$type))

#' @export
dim.FBM <- function(x) {
  desc <- x$description
  c(desc$nrow, desc$ncol)
}

#' @export
length.FBM <- function(x) {
  prod(dim(x))
}

# TODO: change this to FBM afterwards and use dots
#' @export
new_FBM <- function(nrow, ncol,
                    type = c("double", "integer", "unsigned short",
                             "unsigned char", "raw"),
                    init = NULL,
                    backingfile = tempfile(),
                    save = TRUE) {

  do.call(FBM_R6Class$new, args = as.list(environment()))
}

# TODO: change this to big_attach afterwards
#' @export
attach_FBM <- function(rdsfile) {

  rdsfile <- normalizePath(rdsfile)
  fbm <- readRDS(rdsfile)

  if (!file.exists(bkfile <- sub("\\.rds$", ".bk", rdsfile)))
    stop2("Can't find the backingfile associated with this FBM.")

  fbm$set_backingfile(bkfile)
  fbm
}
