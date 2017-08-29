X <- big_attachExtdata()
class(X)
X[1:5, 1:10]

# Without the "decoding"
X2 <- big_copy(X)
class(X2)
X2[1:5, 1:10]

# Change the code
code <- rep(NA_real_, 256)
code[1:3] <- c(2, 5, 9)
X$code256 <- code
X[1:5, 1:10]

# by columns
big_counts(X, ind.col = 1:10)

apply(X[, 1:10], 2, table, exclude = NULL)

# by rows
big_counts(X, ind.row = 1:10, byrow = TRUE)
apply(X[1:10, ], 1, table, exclude = NULL)
