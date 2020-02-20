library(bigstatsr)
X <- FBM(1e4, 1e4, init = rnorm(1e8))
M <- matrix(rnorm(1e5), ncol = 10)

bench::mark(
  X %*% M,
  big_prodMat(X, M),
  iterations = 10
)
# A tibble: 2 x 13
#   expression           min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result      memory    time  gc
#   <bch:expr>        <bch:> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>      <list>    <lis> <list>
# 1 X %*% M            481ms   483ms      2.07     784KB     0       10     0      4.83s <dbl[,10] ~ <df[,3] ~ <bch~ <tibbl~
# 2 big_prodMat(X, M)  857ms   996ms      1.03     765MB     1.03    10    10      9.71s <dbl[,10] ~ <df[,3] ~ <bch~ <tibbl~
big_prodMat
