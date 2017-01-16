X2 <- matrix(0, 1000, 1000)
X2[] <- rnorm(length(X2))
n <- nrow(X2)
m <- ncol(X2)

X3 <- sweep(X2, 2, colMeans(X2), '-')
X <- sweep(X3, 2, sqrt(colSums(X3^2) / (n-1)), '/')

# parameters
h2 <- 0.8 # heritability
h2.lims <- c(0.7, 0.9)
M <- 100
K <- 0.3

# simulation
v <- 1
while (v < h2.lims[1] || v > h2.lims[2]) {
  set <- sample(m, size = M)
  effects <- rnorm(M, sd = sqrt(h2 / M))
  y.simu <- X[, set] %*% effects
  print(v <- var(y.simu))
}
y.simu <- y.simu + rnorm(n, sd = sqrt(1 - v))

require(glmnet)

print(system.time(mod <- glmnet(X, y.simu)))
print(nbBetas <- colSums(mod$beta != 0))
plot(nbBetas, mod$dev.ratio)


X.svd <- readRDS(("tmp-tests/X.svd.rds"))
plot(X.svd$d)

find_knee <- function(y, x = seq_along(y)) {
  inflection::findiplist(x, y, y[1] < tail(y, 1))#["EDE", 1]
}
find_knee(X.svd$d)

find_knee(mod$dev.ratio, nbBetas)
x <- 1:10
y <- X.svd$d

plot(y)
abline(v = k, col = "red")
findknee(x, y )

#load package inflection
library(inflection)

plot(nbBetas, log(mod$dev.ratio))

n <- length(mod$dev.ratio)
curv <- numeric(n)
size <- 10
for (i in size :n) {
  subset = i - 0:(size-1)
  mylm <- lm(mod$dev.ratio ~ poly(nbBetas, 2), subset = subset)
  coeffs <- mylm$coefficients
  curve(coeffs[3] * x^2 + coeffs[2] * x + coeffs[1], add = TRUE,
        col = "red", xlim = range(nbBetas))
  tmp <- 2 * coeffs[3] * nbBetas[subset]
  curv[i] <- tail(tmp / (1 + (tmp + coeffs[2])^2)^1.5, 1)
}

r2 <- matrix(NA, n, 2)
for (i in 2:(n-1)) {
  mylm.left <- lm(mod$dev.ratio ~ nbBetas, subset = 1:i)
  mylm.right <- lm(mod$dev.ratio ~ nbBetas, subset = i:n)
  r2[i, 1] <- summary(mylm.left)$r.squared
  r2[i, 2] <- summary(mylm.right)$r.squared
}
test <- rowSums(sweep(r2, 2, c(1, 1), "-")^2)
plot(test)

find_knee2 <- function(y, x = seq_along(y)) {
  n <- length(y)
  r2 <- matrix(NA, n, 2)
  for (i in 2:(n-1)) {
    r2[i, 1] <- summary(lm(y ~ x, subset = 1:i))$r.squared
    r2[i, 2] <- summary(lm(y ~ x, subset = i:n))$r.squared
  }
  rowSums(sweep(r2, 2, c(1, 1), '-')^2)
}
plot(find_knee2(mod$dev.ratio, nbBetas))
plot(find_knee2(log(mod$dev.ratio[-1]), nbBetas))
plot(find_knee2(X.svd$d))
which.min(find_knee2(X.svd$d))
