library(bigstatsr)

a <- big.matrix(10, 15)
b <- big.matrix(10, 15)
mat <- rbind(a, b)


setMethod("rbind", signature(x = "big.matrix", y = "big.matrix"),
          function(x, y) x@description$ncol)


tmp <- structure(list(a, b), class = "listBM")



assert_same <- function(...) {
  if (any(diff(c(...)) != 0))
    stop2("Incompatibility between dimensions.")
}

args <- list(a, b)

assert_same(ncol(args[[1]]), ncol(args[[2]]))

rbind.big.matrix <- function(...) {
  args <- list(...)
  lapply(args, bigstatsr:::assert_class, class = 'big.matrix')
  assert_same(sapply(args, ncol))
  structure(args, class = "listBM")
}

tmp2 <- rbind(a, b)
tmp3 <- rbind(a)
