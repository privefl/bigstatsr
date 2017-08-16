tmp <- function(a, ...) {
  c(as.list(environment()), list(...))
}

tmp(2, b = 3, c = 3:4)
tmp(2)
