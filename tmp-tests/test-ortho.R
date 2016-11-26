a <- matrix(rnorm(50 * 10), 50)
b <- svd(a, nu = 5, nv = 5)

str(b)

c <- svd(a)

str(c)
rowMeans(c$u)

a <- NA
`if`(b, 2, 3)

crossprod(c$u) # OK
tcrossprod(c$u)[1:5, 1:5] # NOK

crossprod(c$v) # OK
tcrossprod(c$v)[1:5, 1:5] # OK

crossprod(b$u)[1:5, 1:5] # OK
tcrossprod(b$u)[1:5, 1:5] # NOK

crossprod(b$v)[1:5, 1:5] # OK
tcrossprod(b$v)[1:5, 1:5] # NOK

a <- matrix(rnorm(1e5 * 200), 1e5)
print(system.time(e <- qr.Q(qr(a))))
print(system.time(b <- svd(a, nv = 0)))
all.equal(crossprod(e), diag(ncol(e)))
all.equal(crossprod(b$u), diag(ncol(e)))

