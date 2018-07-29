#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix replace_mat1(NumericMatrix mat) {

  int i, j, n = mat.nrow(), m = mat.ncol();
  NumericMatrix res(n, m);

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      res(i, j) = mat(i, j);

  return res;
}

// [[Rcpp::export]]
NumericMatrix replace_mat2(NumericMatrix mat) {

  size_t i, j, n = mat.nrow(), m = mat.ncol();
  NumericMatrix res(n, m);

  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      res(i, j) = mat(i, j);

  return res;
}

/*** R
mat <- matrix(0, 2000, 2000)
microbenchmark::microbenchmark(
  res1 <- replace_mat1(mat),
  res2 <- replace_mat2(mat),
  times = 500
)
# Unit: milliseconds
#                      expr      min       lq     mean   median       uq      max neval cld
# res1 <- replace_mat1(mat) 10.94342 11.44948 34.71310 12.13434 19.72417 204.1541   500   a
# res2 <- replace_mat2(mat) 10.96063 11.45130 34.86668 12.01101 19.70762 195.9419   500   a
stopifnot(identical(res2, res1))
*/
