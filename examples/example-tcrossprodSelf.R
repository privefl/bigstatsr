X.desc <- big_attachExtdata()

# Comparing with tcrossprod
big_noscale <- big_scale(center = FALSE)
test <- big_tcrossprodSelf(X.desc, fun.scaling = big_noscale)
dim(test$K)

true <- tcrossprod(attach.BM(X.desc)[,])
all.equal(test$K, true)

# Using only half of the data
n <- nrow(X.desc)
ind <- sort(sample(n, n/2))
test2 <- big_tcrossprodSelf(X.desc, fun.scaling = big_noscale,
                            ind.row = ind)
dim(test2$K)

true2 <- tcrossprod(attach.BM(X.desc)[ind, ])
all.equal(test2$K, true2)
