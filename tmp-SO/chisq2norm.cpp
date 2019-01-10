// [[Rcpp::depends(BH, bigstatsr)]]
#include <bigstatsr/BMAcc.h>
using namespace Rcpp;

// [[Rcpp::export]]
void trans_cpp(Environment BM_in, Environment BM_out, NumericVector df) {

  XPtr<FBM> xpBM_in = BM_in["address"];
  BMAcc<double> macc_in(xpBM_in);
  XPtr<FBM> xpBM_out = BM_out["address"];
  BMAcc<double> macc_out(xpBM_out);

  size_t n = macc_in.nrow();
  size_t m = macc_in.ncol();

  double x, q, p;

  for (size_t j = 0; j < m; j++) {
    for (size_t i = 0; i < n; i++) {
      x = macc_in(i, j);
      p = R::pchisq(x, df[i], 1, 0);
      q = R::qnorm(p, 0, 1, 1, 0);
      macc_out(i, j) = q;
    }
  }
}
