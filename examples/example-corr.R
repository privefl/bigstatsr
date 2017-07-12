X.desc <- big_attachExtdata()

# Comparing with cor
K <- big_cor(X.desc)
dim(K)

true <- cor(attach.BM(X.desc)[])
all.equal(K, true)

# Using only half of the data
n <- nrow(X.desc)
ind <- sort(sample(n, n/2))
K2 <- big_cor(X.desc, ind.row = ind)
dim(K2)

true2 <- cor(attach.BM(X.desc)[ind, ])
all.equal(K2, true2)
