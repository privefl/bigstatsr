library(bigstatsr)
X <- FBM(1e4, 1e4, init = rnorm(1e8, sd = 5))
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


X <- FBM(1e4, 1e4, init = rnorm(1e8, sd = 5), type = "integer")

ind <- sample(1e4, 5e3)
ind2 <- sample(1e4, 5e3)
bench::mark(
  X[ind, ind2] %*% M[ind2, ],
  big_prodMat(X, M[ind2, ], ind.row = ind, ind.col = ind2),
  mult_sub_int_dbl(X, M[ind2, ], ind, ind2),
  mult_sub_int_dbl2(X, M[ind2, ], ind, ind2),
  mult_sub_int_dbl_arma(X, M[ind2, ], ind, ind2),
  iterations = 10
)
#   expression                                        min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
#   <bch:expr>                                      <bch> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
# 1 X[ind, ] %*% M                                  292ms  317ms      3.15     229MB    0.786     8     2      2.54s
# 2 big_prodMat(X, M, ind.row = ind)                299ms  319ms      3.13     231MB    2.08      6     4      1.92s
# 3 mult_sub_int_dbl(X, M, ind, cols_along(X))      639ms  706ms      1.45     203KB    0        10     0      6.88s
# 4 mult_sub_int_dbl_arma(X, M, ind, cols_along(X)) 619ms  653ms      1.51     203KB    0        10     0      6.61s

#   expression                                                    min   median `itr/sec` mem_alloc `gc/sec` n_itr
#   <bch:expr>                                               <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int>
# 1 X[ind, ind2] %*% M[ind2, ]                               434.07ms 484.01ms     2.09      287MB     1.39     6
# 2 big_prodMat(X, M[ind2, ], ind.row = ind, ind.col = ind2) 464.65ms 502.38ms     1.97      288MB     1.97     5
# 3 mult_sub_int_dbl(X, M[ind2, ], ind, ind2)                972.06ms    1.13s     0.898     784KB     0       10
# 4 mult_sub_int_dbl2(X, M[ind2, ], ind, ind2)               373.21ms 392.44ms     2.49      789KB     0       10
# 5 mult_sub_int_dbl_arma(X, M[ind2, ], ind, ind2)              1.02s    1.11s     0.865     784KB     0       10
