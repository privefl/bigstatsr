X.desc <- big_attachExtdata()

# Comparing with tcrossprod
big_noscale <- big_scale(center = FALSE)
K <- big_crossprodSelf(X.desc, fun.scaling = big_noscale)
dim(K)

true <- crossprod(attach.BM(X.desc)[,])
all.equal(K, true, check.attributes = FALSE)

# Using only half of the data
n <- nrow(X.desc)
ind <- sort(sample(n, n/2))
K2 <- big_crossprodSelf(X.desc, fun.scaling = big_noscale,
                           ind.row = ind)
dim(K2)

true2 <- crossprod(attach.BM(X.desc)[ind, ])
all.equal(K2, true2, check.attributes = FALSE)


