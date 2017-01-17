# Simulating some data
data("trees")
N <- nrow(trees)
covar <- matrix(rnorm(N * 3), N)
X <- as.big.matrix(as.matrix(trees))
y <- sample(0:1, size = N, replace = TRUE)

# without covar
print(big_univRegLog(X, y))
for (i in 1:3) {
  print(summary(glm(y ~ trees[, i], family = "binomial"))$coefficients[2, ])
}

# With all data
print(big_univRegLog(X, y, covar.train = covar))
for (i in 1:3) {
  print(summary(glm(y ~ trees[, i] + covar,
                    family = "binomial"))$coefficients[2, ])
}

# With only half of the data
ind.train <- sort(sample(N, N / 2))

print(big_univRegLog(X, y[ind.train], covar.train = covar[ind.train, ],
                     ind.train = ind.train))
for (i in 1:3) {
  print(summary(glm(y ~ trees[, i] + covar, subset = ind.train,
                    family = "binomial"))$coefficients[2, ])
}

