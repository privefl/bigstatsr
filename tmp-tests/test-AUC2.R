install.packages("pROC")
y <- rep(0:1, each = 4)
x <- c(1:4, 2:5)
pROC::auc(y, x)
bigstatsr::AUC(x, y)


pROC::auc(y, rep(1, 8))
bigstatsr::AUC(rep(1, 8), y)


x2 <- rnorm(1e3)
y2 <- sample(0:1, size = length(x2), replace = TRUE)
pROC::auc(y2, x2)
bigstatsr::AUC(x2, y2)
microbenchmark::microbenchmark(
  pROC::auc(y2, x2),
  bigstatsr::AUC(x2, y2)
)


x3 <- sample(10, size = 1e4, replace = TRUE)
x3 <- runif(1e3)
y3 <- sample(0:1, size = length(x3), replace = TRUE)
pROC::auc(y3, x3)
bigstatsr::AUC(x3, y3)
AUC3(x3, y3)
microbenchmark::microbenchmark(
  pROC::auc(y3, x3),
  bigstatsr::AUC(x3, y3),
  AUC3(x3, y3),
  times = 10
)

AUC3 <- function(x3, y3) {
  wilcox.test(x3[y3 == 0], x3[y3 == 1], alternative = "less")$statistic /
    (sum(y3 == 0) * 1 * sum(y3 == 1))
}


set.seed(1)
N <- 1e6
x4 <- c(sample(10, size = N, replace = TRUE) + runif(N),
        sample(5,  size = N, replace = TRUE) + runif(N))
y4 <- rep(0:1, each = N)
# pROC::auc(y4, x4)
# pROC::auc(factor(y4, levels = 0:1, ordered = TRUE), x4)
bigstatsr::AUC(x4, y4)
# AUC3(x4, y4)
# ROCR::performance(ROCR::prediction(x4, y4), measure="auc")@y.values[[1]]
ModelMetrics::auc(y4, x4)
microbenchmark::microbenchmark(
  # pROC::auc(y4, x4),
  bigstatsr:::AUC2(x4, as.logical(y4)),
  # AUC3(x4, y4),
  # ROCR::performance(ROCR::prediction(x4, y4), measure="auc")@y.values[[1]],
  ModelMetrics::auc(y4, x4),
  times = 20
)

# plot(pROC::roc(y4, x4))

ggplot2::qplot(x4, fill = as.factor(y4), geom = "density", alpha = I(0.5))


pROC::auc(0:1, 1:0, direction = "<")
pROC::auc(0:1, 0:1, direction = "<")

