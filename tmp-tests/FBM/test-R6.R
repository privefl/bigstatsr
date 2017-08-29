Numbers <- R6::R6Class(
  "Numbers",
  public = list(
    x = 100,
    .nrow = 1,
    .ncol = 1,
    .path = "tmp-tests/test.bin",
    print = function(...) {
      cat("A number equals to", self$x, "\n")
    },
    show = function() {
      cat("A number equals to", self$x, "\n")
    },
    `[` = function(i, j, drop = FALSE) {
      self$x
    },
    finalize = function() {
      print("Finalizer has been called!")
    },
    nrow = function() {
      return(self$.nrow)
    }
  ),
  active = list(
    x2 = function(value) {
      if (missing(value)) return(self$x * 2)
      else self$x <- value/2
    },
    rand = function() rnorm(1),
    description = function() {
      list(
        path = self$.path,
        nrow = self$.nrow,
        ncol = self$.ncol
      )
    }
  ),
  lock_class = TRUE
)

n <- Numbers$new()
n
n$show()
# print.Numbers <- function(x) x$show()
# n
n[]
`[.Numbers` <- function(x, i, j, drop = FALSE) x$x
n[]
n[1]
dimnames(n)
nrow(n) # need to define S3 method


n2 <- n # same Environment
n$x <- 10
n2$x
n3 <- n$clone() # another Environment
n$x <- 1
n2$x
n3$x

doParallel::registerDoParallel(2)
library(foreach)
foreach(i = 1:2) %dopar% {
  if (i == 1) {
    n$x <- 3
    return(NULL)
  } else if (i == 2) {
    Sys.sleep(1)
    return(n$x)
  }
} # environments are copied
doParallel::stopImplicitCluster()

