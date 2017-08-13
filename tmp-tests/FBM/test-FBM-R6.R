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
        print("GET IT")
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


test <- FBM$new("test.bin", 10, 10, "double")
test$description

print.FBM <- function(x) {
  desc <- x$description
  cat("A Filebacked Big Matrix of type", desc$type,
      "with", desc$nrow, "rows and", desc$ncol, "columns.\n")
}
test

dim.FBM <- function(x) {
  desc <- x$description
  c(desc$nrow, desc$ncol)
}
dim(test)
nrow(test)
ncol(test)

length.FBM <- function(x) {
  prod(dim(x))
}
length(test)

getXPtrFBM <- function(l) crash
test$description
test$address
test$address

# print2.FBM <- function(x) "LOL"
# print2(test) # need a generic to dispatch
