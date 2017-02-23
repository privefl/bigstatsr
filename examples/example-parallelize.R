X.desc <- big_attachExtdata()

### Computation on all the matrix
true <- big_colstats(X.desc)

big_colstats_sub <- function(X.desc, ind, cols) {
  big_colstats(X.desc, ind.col = cols[ind])
}
# 1. the computation is split along all the columns
# 2. for each part the computation is done, using `big_colstats`
# 3. the results (data.frames) are combined via `rbind`.
test <- big_parallelize(X.desc, p.FUN = big_colstats_sub,
                        p.combine = 'rbind', ncores = 2,
                        cols = cols_along(X.desc))
all.equal(test, true)

### Computation on a part of the matrix
n <- nrow(X.desc)
m <- ncol(X.desc)
rows <- sort(sample(n, n/2)) # sort to provide some locality in accesses
cols <- sort(sample(m, m/2)) # idem

true2 <- big_colstats(X.desc, ind.row = rows, ind.col = cols)

big_colstats_sub2 <- function(X.desc, ind, rows, cols) {
  big_colstats(X.desc, ind.row = rows, ind.col = cols[ind])
}
# This doesn't work because, by default, the computation is spread
# along all columns. We must explictly specify the `ind` parameter.
tryCatch(test2 <- big_parallelize(X.desc, p.FUN = big_colstats_sub2,
                                  p.combine = 'rbind', ncores = 2,
                                  rows = rows, cols = cols),
         error = FUN_ERROR)
# This now works, using `ind = seq_along(cols)`.
test2 <- big_parallelize(X.desc, p.FUN = big_colstats_sub2,
                         p.combine = 'rbind', ncores = 2,
                         ind = seq_along(cols),
                         rows = rows, cols = cols)
all.equal(test2, true2)
