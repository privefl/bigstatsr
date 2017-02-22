

X.desc <- big_attachExtdata()
A <- matrix(0, X.desc@description$ncol, 10)
A[] <- rnorm(length(A))

mult <- function(x, ind.row, ind, A) {
  x[ind.row, ind] %*% A[ind, ]
}

trueA <- X[,] %*% A

testA.1 <- big_apply(X.desc, a.FUN = mult, a.combine = '+',
                     block.size = 100, A = A, ind.row = 1:10)

testA.2 <- big_parallelize(X.desc, p.FUN = big_apply, p.combine = '+', ncores = 2,
                           a.FUN = mult, a.combine = '+', block.size = 50,
                           A = A, ind.row = 1:10)
print(all.equal(testA.2, testA.1))

testA.3 <- big_parApply(X.desc, a.FUN = mult, a.combine = '+', ncores = 2,
                        block.size = 50, A = A, ind.row = 1:10)
print(all.equal(testA.3, testA.2))
