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
  mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 99),
  mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 999),
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
# 1 X[ind, ind2] %*% M[ind2, ]                               427.97ms  478.6ms     2.13      287MB    0.911     7
# 2 big_prodMat(X, M[ind2, ], ind.row = ind, ind.col = ind2) 477.85ms 660.01ms     1.45      288MB    0.969     6
# 3 mult_sub_int_dbl(X, M[ind2, ], ind, ind2)                951.17ms    1.03s     0.924     784KB    0        10
# 4 mult_sub_int_dbl2(X, M[ind2, ], ind, ind2)                378.9ms 582.56ms     1.80      789KB    0        10
# 5 mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 99)           320.53ms    339ms     2.85      784KB    0        10
# 6 mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 999)          354.05ms 402.02ms     2.35      784KB    0        10
# 7 mult_sub_int_dbl_arma(X, M[ind2, ], ind, ind2)              1.25s    1.29s     0.773     789KB    0        10

microbenchmark::microbenchmark(
  X[ind, ind2] %*% M[ind2, ],
  big_prodMat(X, M[ind2, ], ind.row = ind, ind.col = ind2),
  mult_sub_int_dbl(X, M[ind2, ], ind, ind2),
  mult_sub_int_dbl2(X, M[ind2, ], ind, ind2),
  mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 99),
  mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 999),
  mult_sub_int_dbl_arma(X, M[ind2, ], ind, ind2),
  times = 10
)
# Unit: milliseconds
#                                                     expr       min        lq      mean    median        uq       max neval
#                               X[ind, ind2] %*% M[ind2, ]  474.2421  539.7378  614.2124  599.2641  713.3924  770.9940    10
# big_prodMat(X, M[ind2, ], ind.row = ind, ind.col = ind2)  451.8309  515.1636  613.8332  583.0536  616.8157 1004.6226    10
#                mult_sub_int_dbl(X, M[ind2, ], ind, ind2) 1002.1308 1023.6569 1107.5483 1078.6188 1165.8623 1344.8747    10
#               mult_sub_int_dbl2(X, M[ind2, ], ind, ind2)  396.9892  420.7113  589.1570  510.7337  753.4485  894.5825    10
#           mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 99)  201.5534  229.9221  268.3901  253.8328  271.9938  474.5281    10
#          mult_sub_int_dbl3(X, M[ind2, ], ind, ind2, 999)  284.2305  316.4634  384.4945  330.3973  349.6271  650.9986    10
#           mult_sub_int_dbl_arma(X, M[ind2, ], ind, ind2)  976.6248 1028.2316 1094.5684 1098.5552 1135.8651 1197.4008    10
