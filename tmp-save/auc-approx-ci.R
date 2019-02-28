x <- runif(5000)
y <- round(x + sample(0:1, 5000, TRUE) / 3)
(auc <- c <- bigstatsr::AUC(x, y))
bigstatsr::AUCBoot(x, y, nboot = 1e5)

# https://stats.stackexchange.com/questions/296748/how-to-get-approximative-confidence-interval-for-gini-and-auc
n <- sum(y == 0)
m <- sum(y == 1)
tmp <- c * (1 - c) + (m - 1) * (c / (2 - c) - c^2) + (n - 1) * (2 * c^2 / (1 + c) - c^2)
(se <- sqrt(tmp / n / m))
c + qnorm(1 - 0.05 / 2) * c(-1, 1) * se
