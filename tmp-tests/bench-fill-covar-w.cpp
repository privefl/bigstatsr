// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;
using namespace Eigen;
using std::size_t;

/******************************************************************************/

// [[Rcpp::export]]
void fill_covar_w(Map<MatrixXd> covar_w,
                  const Map<MatrixXd> covar,
                  const Map<VectorXd> sqrt_w,
                  int n, int K) {

  for (int j = 0; j < K; j++)
    for (int i = 0; i < n; i++)
      covar_w(i, j) = covar(i, j) * sqrt_w[i];
}

// [[Rcpp::export]]
void fill_tcovar_w(Map<MatrixXd> tcovar_w,
                  const Map<MatrixXd> covar,
                  const Map<VectorXd> w,
                  int n, int K) {

  double coeff;
  for (int i = 0; i < n; i++) {
    coeff = ::sqrt(w[i]);
    for (int j = 0; j < K; j++) {
      tcovar_w(j, i) = covar(i, j) * coeff;
    }
  }
}

/*** R
N <- 1e4
K <- 20
X <- matrix(rnorm(N * K), N)
p <- runif(N)
w <- p * (1 - p)
X_w <- X + 0
tX_w <- t(X_w)
fill_covar_w(X_w, X, sqrt(w), N, K)
fill_tcovar_w(tX_w, X, w, N, K)
stopifnot(all.equal(t(X_w), tX_w))

microbenchmark::microbenchmark(
  fill_covar_w(X_w, X, sqrt(w), N, K),
  fill_tcovar_w(tX_w, X, w, N, K)
)
*/
