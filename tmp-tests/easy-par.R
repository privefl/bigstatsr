library(bigstatsr)
fbm <- FBM(10, 10)
plot(fbm)

test <- function(x, ...) {
  UseMethod("test")
}

test(fbm)

is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}
is_windows()

type <- `if`(is_windows(), "PSOCK", "FORK")

cluster_type <- function(type = c("FORK", "PSOCK")) {

  type <- `if`(missing(type), NULL, match.arg(type))
}
cluster_type()

par_over_rows <- function(x, fun, fun.comb, ind.row = rows_along(x)) {

  assert_args(p.FUN, "ind")
  assert_int(ind); assert_pos(ind)
  assert_cores(ncores)

  if (ncores == 1) {
    registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores, type = )
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  range.parts <- CutBySize(length(ind), nb = ncores)

  res <- foreach(ic = seq_len(ncores)) %dopar% {
    p.FUN(X, ind = ind[seq2(range.parts[ic, ])], ...)
  }

  `if`(is.null(p.combine), res, do.call(p.combine, res))
  big_parallelize(x, p.FUN = fun, p.combine = fun.comb, ind = ind.row,
                  )
}
