celiac <- AttachBigSNP("../bigsnpr/backingfiles/celiac_impute1_sub1.bk")
X <- celiac$genotypes
y <- celiac$fam$affection
n <- length(y)

print(system.time(
  test <- big_univRegLin(X, y)
))

print(system.time(
  test <- big_univRegLin(X, y)
))

print(system.time(
  test2 <- univRegLin2(X@address, covar = cbind(rep(0, n), rep(1, n)),
                       y = y, rowInd = 1:n)
))

print(all.equal(test["Slopes", ], test2$betas))
stats <- cbind(n * test["R2", ], (test2$betas / test2$std)^2)
plot(stats, pch = 19, cex = 0.5)
p_values <- cbind(pchisq(stats[, 1], df = 1, lower.tail = FALSE),
                  pf(stats[, 2], df1 = 1, df2 = n - 2, lower.tail = FALSE))
plot(p_values, pch = 19, cex = 0.5, log = "xy")
abline(0, 1, col = "red")
