#'
#'
#' @export
#'
FBM_R6Class <- R6::R6Class(

  "FBM",

  public = list(
    initialize = function(backingfile, nrow, ncol, type) {
      private$backingfile <- normalizePath(backingfile)
      private$nrow        <- nrow
      private$ncol        <- ncol
      private$type        <- type
    },

    set_backingfile = function(backingfile) {
      private$backingfile <- normalizePath(backingfile)
      invisible(self)
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

  lock_class = TRUE
)

#' @export
print.FBM <- function(x) {
  desc <- x$description
  cat("A Filebacked Big Matrix of type", desc$type,
      "with", desc$nrow, "rows and", desc$ncol, "columns.\n")
}

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
                    type = "double",
                    init = NULL,
                    backingfile = tempfile(),
                    save = TRUE) {

  bkfile <- path.expand(paste0(backingfile, ".bk"))
  createFile(bkfile, nrow, ncol, type)

  fbm <- FBM_R6Class$new(bkfile, nrow, ncol, type)
  fbm$address

  if (!is.null(init)) fbm[] <- init

  if (save) saveRDS(fbm, sub("\\.bk$", ".rds", bkfile))

  fbm
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

#' @export # TODO: use dots
setGeneric("as.FBM", function(x, type = "double",
                              backingfile = tempfile(),
                              save = TRUE) {
  standardGeneric("as.FBM")
})


#' @export
setMethod("as.FBM", signature(x = "matrix"),
          function(x, type = "double",
                   backingfile = tempfile(),
                   save = TRUE) {
            new_FBM(
              nrow = nrow(x),
              ncol = ncol(x),
              type = type,
              init = x,
              backingfile = backingfile,
              save = save
            )
          })


