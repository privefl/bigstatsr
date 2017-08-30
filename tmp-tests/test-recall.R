tmp <- function(x, b = 2) {
  if (x > 10) {
    print(args <- as.list(environment()))
    args$x <- 10
    do.call(tmp, args)
  } else {
    x + b
  }
}

tmp(1)
tmp(1, 3)
tmp(10, 3)
tmp(15, 3)
