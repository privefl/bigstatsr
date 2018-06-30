#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector find_val2(const NumericVector& x,
                        const NumericVector& p) {

  int n = x.size(), ind;
  NumericVector res(n);
  for (int i = 0; i < n; i++) {
    ind = x[i] * 65536;
    // Rcout << ind << std::endl;
    res[i] = p[ind];
  }

  return res;
}


/*** R
(x <- runif(10))
p <- 1 / (1 + exp(-seq(0, 10, length.out = 10 * 65536 + 1)))
p2 <- find_val2(x, p)
rbind(1 / (1 + exp(-x)), p2)

comp_val <- function(x) {
  p <- 1 / (1 + exp(-x))
}

find_val <- function(x, p) {
  x2ind <- x * 65536 + 1
  ind <- rbind(floor(x2ind), ceiling(x2ind))
  colMeans(matrix(p[ind], 2))
}

x <- runif(1e5)
all.equal(find_val(x, p), comp_val(x))
all.equal(find_val2(x, p), comp_val(x))
microbenchmark::microbenchmark(
  find_val2(x, p),
  find_val(x, p),
  comp_val(x)
)
*/
