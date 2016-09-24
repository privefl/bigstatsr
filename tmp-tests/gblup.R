DualMultLinReg <- function(X,
                           y,
                           block.size,
                           ind.train = seq(nrow(X)),
                           vec.center = rep(0, ncol(X)),
                           vec.scale = rep(1, ncol(X)),
                           thr.eigval = 1e-3,
                           use.Eigen = TRUE,
                           progress = TRUE) {

  res <- BigXYt(X = X,
                block.size = block.size,
                ind.train = ind.train,
                vec.center = vec.center,
                vec.scale = vec.scale,
                use.Eigen = use.Eigen,
                progress = progress)

  n <- length(ind.train)
  if (((n2 <- nrow(X) - n) == 0)) {
    bigK <- res
  } else {
    bigK  <- res[[1]]
    bigK2 <- res[[2]]
  }
  rm(res)


  eig <- eigen(bigK[,], symmetric = TRUE)

  lastEig <- max(which(eig$values > (thr.eigval * ncol(X))))

  y.train <- y[ind.train]
  ind <- 1:lastEig
  tmp <- crossprod(eig$vectors[, ind], y.train - mean(y.train))
  tmp2 <- tmp / eig$values[ind]
  tmp3 <- eig$vectors[, ind] %*% tmp2

  if (n2 == 0) {
    pred <- bigK[,] %*% tmp3 + mean(y.train)
  } else {
    pred <- bigK2[,] %*% tmp3 + mean(y.train)
  }

  return(pred)
}



X <- big.matrix(10, 50, type = "char")
X[] <- rnorm(length(X), sd = 5)
y <- sample(c(-1, 1), nrow(X), TRUE)

test <- DualMultLinReg(X, y, block.size = 10,
                       ind.train = 1:5,
                       vec.center = rep(0, ncol(X)),
                       vec.scale = rep(1, ncol(X)),
                       thr.eigval = 1e-3,
                       use.Eigen = TRUE,
                       progress = TRUE)

mylm <- lm(y ~ X[,])
test2 <- X[1:5, ] %*% mylm$coefficients


X <- matrix(rnorm(200), 10, 20)
y <- sample(c(-1, 1), nrow(X), TRUE)
mylm <- lm(y ~ X)
print(mylm$coefficients)
