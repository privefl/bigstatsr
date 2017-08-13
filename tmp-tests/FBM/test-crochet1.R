library(bigmemory)

setClass("BM.code",
         contains = "big.matrix",
         representation(code = "numeric")
)

setGeneric("as.BM.code", function(x, code) standardGeneric("as.BM.code"))

check_BM_code <- function(x, code) {
  if (typeof(x) != "raw") stop("Your `big.matrix` must be of type 'raw'.")
  if (length(code) != 256) stop("'code' must be of length 256.")
}
setMethod("as.BM.code", signature(x = "big.matrix", code = "numeric"),
          function(x, code) {
            check_BM_code(x, code)
            methods::new("BM.code", address = x@address, code = code)
          })

X <- big.matrix(10, 10, type = "raw")
X[] <- sample(as.raw(0:3), size = length(X), replace = TRUE)
X[]

code <- rep(NA_real_, 256)
code[1:3] <- c(1, 3, 5)

X.code <- as.BM.code(X, code)
X.code[] # same accessor as a "big.matrix"

# dimnames(X.code) <- list(letters[1:10], LETTERS[1:10])

Rcpp::sourceCpp('tmp-tests/test-accessor1.cpp')
`[.BM.code` <- crochet::extract(getVecCode, getMatCode)

X.code[1]
X.code[1:5]
X.code[, 1] # NOK
X.code[1, ] # NOK
X.code[1, 1] # NOK
X.code[1:2, 1] # NOK
X.code[1:2, 1:2] # NOK
X.code[cbind(1:2, 1)] # NOK

# X.code[TRUE, ]
X.code[c(TRUE, FALSE, FALSE, rep(TRUE, 7)), ]
# X.code[, "A"]

X.code[, 1, drop = FALSE]
# X.code[-1]
X.code[-1, ]
