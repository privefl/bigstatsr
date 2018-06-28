N <- 1000
K <- 10
x <- matrix(rnorm(N * K), N)
y <- sample(0:1, N, TRUE)

summary(mod1 <- glm(y ~ x))

x2 <- cbind(1, x); x2[, -2] <- svd(x2[, -2])$u
summary(mod2 <- glm(y ~ x2 + 0))

rbind(mod1$coefficients, mod2$coefficients)

rbind(summary(mod1)$coeff[, 4], summary(mod2)$coeff[, 4])
