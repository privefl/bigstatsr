x <- 2:1e8
.Internal(inspect(x))
y <- x[2:1e7]
.Internal(inspect(y))

z <- x + 1L
.Internal(inspect(z))
.Internal(inspect(x))
