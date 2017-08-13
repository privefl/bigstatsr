b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], LETTERS[1:5])
a <- structure(b, class = "TestMatrix")

dim(a)
dimnames(a)

Rcpp::sourceCpp('tmp-tests/test-accessor2.cpp')
`[.TestMatrix` <- crochet::extract(getVec, getMat)

b
a[1]
a[1:5]
a[, 1]
a[1, ] 
a[1, 1] 
a[1:2, 1] 
a[1:2, 1:2] 
a[cbind(1:2, c(1, NA))] # NOKK
b[cbind(1:2, c(1, NA))]

a[TRUE, ]
a[c(TRUE, FALSE, FALSE, TRUE, TRUE), ]
a[, "A"]
# a[c(TRUE, NA, FALSE, TRUE, TRUE), ]


a[, 1, drop = FALSE]
a[-1]
a[-1, ]
a[25]
a[28]
# a[, 6]
# a[6, ]
a[-(1:5), ]
a[-6, ]
b[-6, ]
b[0]
# a[0]
b[-25]
b[c(NA, 2, 3, NA)]

crochet:::convertIndex(b, c(NA, 2, 3, NA), "k")
crochet:::convertIndex(b, 26, "k")
