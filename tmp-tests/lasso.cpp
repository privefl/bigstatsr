// [[Rcpp::depends(RcppArmadillo, bigmemory, BH)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

using namespace Rcpp;


/******************************************************************************/

// [[Rcpp::export]]
double crossprodCpp(const NumericMatrix& mat, int j,
                    const NumericVector& r) {
  int j2 = j - 1;
  double sum = 0;
  for (int i = 0; i < r.size(); i++) {
    sum += mat(i, j2) * r[i];
  }
  return sum;
}

// [[Rcpp::export]]
NumericVector& updateR(NumericVector& r,
                       const NumericMatrix& mat, int j,
                       double diff) {
  int j2 = j - 1;
  for (int i = 0; i < r.size(); i++) {
    r[i] -= mat(i, j2) * diff;
  }
  return r;
}

/******************************************************************************/

double crossprodCpp2(const NumericMatrix& mat, int j,
                     const NumericVector& r) {
  double sum = 0;
  for (int i = 0; i < r.size(); i++) {
    sum += mat(i, j) * r[i];
  }
  return sum;
}

NumericVector& updateR2(NumericVector& r,
                        const NumericMatrix& mat, int j,
                        double diff) {
  for (int i = 0; i < r.size(); i++) {
    r[i] -= mat(i, j) * diff;
  }
  return r;
}

double soft_thr(double z, double gamma) {
  if (gamma < abs(z)) {
    if (z > 0) {
      return(z - gamma);
    } else {
      return(z + gamma);
    }
  } else {
    return(0);
  }
}

// [[Rcpp::export]]
NumericVector& CD_lasso_Cpp(const NumericMatrix& mat,
                            NumericVector& r,
                            NumericVector& betas,
                            double lambda,
                            double sc, double tol) {
  int m = betas.size();
  LogicalVector remains(m, true);

  double tmpB, tmp, diff;
  int j;
  bool conv;

  do {
    conv = true;
    for (j = 0; j < m; j++) {
      if (remains[j]) {
        tmpB = crossprodCpp2(mat, j, r) / sc + betas[j];
        tmp = soft_thr(tmpB, lambda);
        diff = tmp - betas[j];
        if (abs(diff) > tol) {
          betas[j] = tmp;
          r = updateR2(r, mat, j, diff);
          if (tmp == 0) remains[j] = false;
          if (conv) conv = false;
        }
      }
    }
  } while (!conv);

  return betas;
}

// [[Rcpp::export]]
NumericVector& CD_lasso_Cpp2(const NumericMatrix& mat,
                             NumericVector& r,
                             NumericVector& betas,
                             double lambda,
                             double sc, double tol) {
  int m = betas.size();

  double tmpB, tmp, diff;
  int j;
  bool conv;

  do {
    conv = true;
    for (j = 0; j < m; j++) {
      tmpB = crossprodCpp2(mat, j, r) / sc + betas[j];
      tmp = soft_thr(tmpB, lambda);
      diff = tmp - betas[j];
      if (abs(diff) > tol) {
        betas[j] = tmp;
        r = updateR2(r, mat, j, diff);
        conv = false;
      }
    }
  } while (!conv);

  return betas;
}

/******************************************************************************/
