X.desc <- big_attachExtdata()

# Brute force true result
X <- attach.big.matrix(X.desc)
true <- colSums(X[,])
rm(X)

# Use `big_apply` to get the result of a simple R function, `colSums`
colSums_sub <- function(X, ind) colSums(X[, ind])
test1 <- big_apply(X.desc, a.FUN = colSums_sub, a.combine = 'c')
all.equal(test1, true)

# Parallelize this `big_apply` call
test2 <- big_parallelize(X.desc, p.FUN = big_apply, p.combine = 'c', ncores = 2,
                         a.FUN = colSums_sub, a.combine = 'c')
all.equal(test2, true)

# Wrapper around previous call
test3 <- big_parApply(X.desc, a.FUN = colSums_sub, a.combine = 'c', ncores = 2)
all.equal(test3, true)
