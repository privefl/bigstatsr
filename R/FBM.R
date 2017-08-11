FBM <- R6::R6Class(

  "FBM",

  public = list(
    initialize = function(backingfile, nrow, ncol, type) {
      private$backingfile <- backingfile
      private$nrow        <- nrow
      private$ncol        <- ncol
      private$type        <- type
    }
  ),

  active = list(
    address = function() {
      if (identical(private$extptr, new("externalptr"))) { # nil
        private$extptr <- getXPtrFBM(private$description)
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
    backingfile = NULL,
    nrow = NULL,
    ncol = NULL,
    type = NULL
  ),

  lock_class = TRUE
)

print.FBM <- function(x) {
  desc <- x$description
  cat("A Filebacked Big Matrix of type", desc$type,
      "with", desc$nrow, "rows and", desc$ncol, "columns.\n")
}

dim.FBM <- function(x) {
  desc <- x$description
  c(desc$nrow, desc$ncol)
}

length.FBM <- function(x) {
  prod(dim(x))
}
