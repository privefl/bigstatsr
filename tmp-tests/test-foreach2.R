l <- list(a = 2)

require(foreach)

test <- function(x, ncores) {
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)

  res <- foreach(i = 1:ncores, .combine = 'c') %dopar% {
    x
  }

  parallel::stopCluster(cl)

  res
}

print(test(x = l$a, ncores = 2))

test2 <- function(x, ncores) {
  obj <- foreach(i = 1:ncores, .combine = 'c')
  expr_fun <- function(i) {
    x
  }
  foreach2(obj, expr_fun, ncores)
}

print(test2(x = l$a, ncores = 2))
