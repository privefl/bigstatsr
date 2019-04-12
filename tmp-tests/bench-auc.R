# https://stackoverflow.com/a/50202118/6103040
# By Miron Kursa https://mbq.me
auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

set.seed(42)
score <- sample(10, 1e3, replace = TRUE)  #rnorm(1e3)
bool  <- sample(c(TRUE, FALSE), 1e3, replace = TRUE)

pROC::auc(bool, score)
mltools::auc_roc(score, bool)
ROCR::performance(ROCR::prediction(score, bool), "auc")@y.values[[1]]
auroc(score, bool)
bigstatsr::AUC(score, bool + 0L)

microbenchmark::microbenchmark(
  pROC::auc(bool, score),
  # computeAUC(score[bool], score[!bool]),
  mltools::auc_roc(score, bool),
  ROCR::performance(ROCR::prediction(score, bool), "auc")@y.values,
  auroc(score, bool),
  bigstatsr::AUC(score, bool + 0L),
  times = 5
)

auroc(score, bool)
bigstatsr::AUC(score, bool + 0L)
microbenchmark::microbenchmark(
  auroc(score, bool),
  bigstatsr::AUC(score, bool + 0L),
  times = 500
)

score <- sample(10, 1e5, replace = TRUE)  #rnorm(1e3)
bool  <- sample(c(TRUE, FALSE), 1e5, replace = TRUE)
auroc(score, bool)
bigstatsr::AUC(score, bool + 0L)
microbenchmark::microbenchmark(
  auroc(score, bool),
  bigstatsr::AUC(score, bool + 0L),
  times = 20
)

score <- rnorm(1e5)
bool  <- sample(c(TRUE, FALSE), 1e5, replace = TRUE)
bool2 <- bool + 0L
auroc(score, bool)
bigstatsr::AUC(score, bool2)
microbenchmark::microbenchmark(
  auroc(score, bool),
  bigstatsr::AUC(score, bool2),
  times = 20
)
