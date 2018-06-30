// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;
using namespace Eigen;
using std::size_t;

/******************************************************************************/

VectorXd UUt(const Map<MatrixXd> U,
             const Map<VectorXd> z) {

  return U * (U.adjoint() * z);
}

// template <class C>
// [[Rcpp::export]]
List IRLS(const NumericMatrix& macc,
          const Map<MatrixXd> U,
          const Map<VectorXd> y,
          const Map<VectorXd> z0,
          double tol = 1e-8,
          size_t maxiter = 200) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = U.cols();
  // myassert(U.rows() == n, ERROR_DIM);
  // myassert(y.elems() == n, ERROR_DIM);

  MatrixXd tcovar(K, n), cprod(K, K), cprod_inv(K, K);
  VectorXd x(n), Xb(n), p(n), z(n);
  VectorXd betas_new(K), v(K), Utz(K);
  size_t i, j, c;
  double x_i, A, a, shift, diff;

  // Coefficients
  double beta_x;
  VectorXd beta_U(K);

  // Results
  NumericVector beta(m), var(m);
  IntegerVector niter(m);

  for (j = 0; j < m; j++) {

    A = 0;
    for (i = 0; i < n; i++) {
      x_i = macc(i, j);
      x[i] = x_i;
      A += x_i * x_i;
    }

    // Precomputations
    v = U.adjoint() * x;
    a = 1 / (A - v.dot(v));

    // Initialization
    c = 1;
    Utz = U.adjoint() * z0;
    beta_x = 4 * a * x.dot(z0 - U * Utz);
    beta_U = -beta_x * v + 4 * Utz;

    do {
      c++;

      Xb = beta_x * x + U * beta_U;
      p = 1 / (1 + exp(-Xb.array()));
      z = y - p;

      Utz = U.adjoint() * z;
      shift = 4 * a * x.dot(z - U * Utz);
      beta_x += shift;
      beta_U += -shift * v + 4 * Utz;

      diff = 2 * fabs(shift) / (fabs(shift) + fabs(beta_x));
    } while (diff > tol && c < maxiter);

    beta[j] = beta_x;
    // cprod_inv = inv(cprod);
    // var[j] = cprod_inv(0, 0);
    niter[j] = c;
  }

  return List::create(_["estim"]   = beta,
                      _["std.err"] = sqrt(var),
                      _["niter"]   = niter);
}

/***R
source('~/Bureau/bigstatsr/tmp-save/LDU-decomp.R', echo=TRUE)
IRLS(as.matrix(x), U, as.double(y), z0)
*/
