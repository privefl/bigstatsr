/******************************************************************************/

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

using namespace Rcpp;
using namespace Eigen;
using std::size_t;

/******************************************************************************/

void fill_tcovar_w(MatrixXd& tcovar_w,
                   const MatrixXd& covar,
                   const VectorXd& w,
                   int n, int K) {

  double coeff;
  for (int i = 0; i < n; i++) {
    coeff = ::sqrt(w[i]);
    for (int j = 0; j < K; j++) {
      tcovar_w(j, i) = covar(i, j) * coeff;
    }
  }
}

double get_diff(const VectorXd& shift,
                const VectorXd& coeffs,
                int K) {

  NumericVector diff(K);
  for (int k = 0; k < K; k++) {
    diff[k] = fabs(shift[k]) / (fabs(shift[k]) + fabs(coeffs[k]));
  }

  return(2 * max(diff));
}

/******************************************************************************/

// template <class C>
// [[Rcpp::export]]
List IRLS2(const NumericMatrix& macc,
          Map<MatrixXd> covar,
          const Map<VectorXd> y,
          const Map<VectorXd> z0,
          const Map<VectorXd> w0,
          double tol = 1e-8,
          size_t maxiter = 200) {

  size_t n = macc.nrow();
  size_t m = macc.ncol();
  size_t K = covar.cols();
  // myassert(covar.rows() == n, ERROR_DIM);
  // myassert(y.elems() == n, ERROR_DIM);

  MatrixXd I = MatrixXd::Identity(K, K);
  MatrixXd tcovar_w(K, n), cprod(K, K), cprod_inv(K, K);
  LDLT<MatrixXd, Lower> cprod_ldlt(K);
  VectorXd x(n), Xb(n), p(n), w(n);
  VectorXd coeffs(K), shift(K);
  size_t i, j, c;
  double diff;

  // Results
  NumericVector beta(m), var(m);
  IntegerVector niter(m);

  for (j = 0; j < m; j++) {

    for (i = 0; i < n; i++) {
      covar(i, 0) = macc(i, j);
    }

    c = 1;
    fill_tcovar_w(tcovar_w, covar, w0, n, K);
    cprod.setZero().selfadjointView<Lower>().rankUpdate(tcovar_w);
    cprod_ldlt = cprod.ldlt();
    coeffs = cprod_ldlt.solve(covar.adjoint() * z0);

    do {
      c++;

      Xb = covar * coeffs;
      p = 1 / (1 + exp(-Xb.array()));
      w = p.array() * (1 - p.array());

      fill_tcovar_w(tcovar_w, covar, w, n, K);
      cprod.setZero().selfadjointView<Lower>().rankUpdate(tcovar_w);
      cprod_ldlt = cprod.ldlt();
      shift = cprod_ldlt.solve(covar.adjoint() * (y - p));
      coeffs += shift;

      diff = get_diff(shift, coeffs, K);
    } while (diff > tol && c < maxiter);

    beta[j] = coeffs[0];
    cprod_inv = cprod_ldlt.solve(I);
    var[j] = cprod_inv(0, 0);
    niter[j] = c;
  }

  return List::create(_["estim"]   = beta,
                      _["std.err"] = sqrt(var),
                      _["niter"]   = niter);
}
