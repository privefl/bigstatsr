require(bigsnpr)
require(bigstatsr)
celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
X <- celiac$genotypes
y <- celiac$fam$affection
n <- length(y)

K <- 1
print(system.time(
  test <- big_univRegLin(X, y, covar = NULL)
)) # 14 sec
# 128 with K = 10
# 67 with K = 3
# 229 with K = 20

# print(system.time(
#   test2 <- univRegLin(X@address, covar = cbind(rep(0, n), rep(1, n), rnorm(n)),
#                        y = y, rowInd = 1:n)
# )) # 39 sec -> 48 sec with one additional covar

print(system.time(
  test2 <- univRegLin3(X@address, covar = cbind(1, matrix(rnorm(K * n), n)),
                       y = y, rowInd = 1:n)
)) # 39 sec -> 48 sec with one additional covar
# 131 with K = 10
# 70 with K = 3
# 330 with K = 20


print(all.equal(test["Slopes", ], test2$betas))
stats <- cbind(n * test["R2", ], (test2$betas / test2$std)^2)
plot(stats, pch = 19, cex = 0.5)
p_values <- cbind(pchisq(stats[, 1], df = 1, lower.tail = FALSE),
                  pf(stats[, 2], df1 = 1, df2 = n - 2, lower.tail = FALSE))
plot(p_values, pch = 19, cex = 0.5, log = "xy")
abline(0, 1, col = "red")
