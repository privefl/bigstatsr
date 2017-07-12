/******************************************************************************/
/******         This is a modified version from package biglasso         ******/
/******              https://github.com/YaohuiZeng/biglasso              ******/
/******************************************************************************/

#include <RcppArmadillo.h>
#include "../inst/include/bigstatsr/SubMatCovAcc.h"
#include <time.h>

/******************************************************************************/

void print_time(bool verbose, bool end = false, int l = -1) {
  if (verbose) {
    char buff[100];
    time_t now = time(0);
    strftime(buff, 100, "%Y-%m-%d %H:%M:%S", localtime(&now));
    if (l < 0) {
      if (end) {
        Rprintf("Preprocessing end: %s\n", buff);
        Rprintf("\n-----------------------------------------------\n");
      } else {
        Rprintf("\nPreprocessing start: %s\n", buff);
      }
    } else {
      Rprintf("Lambda %d. Now time: %s\n", l, buff);
    }
  }
}

// standardize
template <class C>
void COPY_standardize_and_get_residual(NumericVector &center,
                                       NumericVector &scale,
                                       int *p_keep_ptr,
                                       vector<int> &col_idx, //columns to keep, removing columns whose scale < 1e-6
                                       vector<double> &z,
                                       double *lambda_max_ptr,
                                       C xAcc,
                                       const NumericVector &y,
                                       double lambda_min,
                                       double alpha,
                                       int n, int p) {
  double tmp, sum_xy;
  double zmax = 0.0, zj = 0.0;
  int i, j;

  double sum_y = Rcpp::sum(y);

  for (j = 0; j < p; j++) {
    sum_xy = 0.0;

    for (i = 0; i < n; i++) {
      tmp = xAcc(i, j);
      center[j] += tmp;
      scale[j] += tmp * tmp;
      sum_xy += tmp * y[i];
    }

    center[j] /= n; //center
    scale[j] = sqrt(scale[j] / n - pow(center[j], 2)); //scale

    if (scale[j] > 1e-6) {
      col_idx.push_back(j);
      zj = (sum_xy - center[j] * sum_y) / (scale[j] * n); //residual
      if (fabs(zj) > zmax) {
        zmax = fabs(zj);
      }
      z.push_back(zj);
    }
  }
  *p_keep_ptr = col_idx.size();
  *lambda_max_ptr = zmax / alpha;
}

double COPY_lasso(double z, double l1, double l2, double v) {
  double s=0;
  if (z > 0) s = 1;
  else if (z < 0) s = -1;
  if (fabs(z) <= l1) return(0);
  else return(s*(fabs(z)-l1)/(v*(1+l2)));
}

/******************************************************************************/

// check KKT conditions over features in the strong set
template <class C>
int COPY_check_strong_set(LogicalVector &in_A, const LogicalVector &in_S, vector<double> &z,
                          C xAcc, const NumericVector &beta_old,
                          const vector<int> &col_idx,
                          const NumericVector &center, const NumericVector &scale,
                          double lambda, double sumResid, double alpha,
                          const NumericVector &r, const NumericVector &m, int n, int p) {
  double sum, l1, l2;
  int i, j, jj, violations = 0;

  for (j = 0; j < p; j++) {
    if (!in_A[j] && in_S[j]) {
      jj = col_idx[j];
      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum += xAcc(i, jj) * r[i];
      }
      z[j] = (sum - center[jj] * sumResid) / (scale[jj] * n);

      l1 = lambda * m[jj] * alpha;
      l2 = lambda * m[jj] * (1 - alpha);
      if(fabs(z[j] - beta_old[j] * l2) > l1) {
        in_A[j] = true;
        violations++;
      }
    }
  }
  return violations;
}

// check KKT conditions over features in the rest set
template <class C>
int COPY_check_rest_set(LogicalVector &in_A, LogicalVector &in_S, vector<double> &z,
                        C xAcc, const NumericVector &beta_old,
                        const vector<int> &col_idx,
                        const NumericVector &center, const NumericVector &scale,
                        double lambda, double sumResid, double alpha,
                        const NumericVector &r, const NumericVector &m, int n, int p) {
  double sum, l1, l2;
  int i, j, jj, violations = 0;

  for (j = 0; j < p; j++) {
    if (!in_S[j]) {
      jj = col_idx[j];
      sum = 0.0;
      for (i = 0; i < n; i++) {
        sum += xAcc(i, jj) * r[i];
      }
      z[j] = (sum - center[jj] * sumResid) / (scale[jj] * n);

      l1 = lambda * m[jj] * alpha;
      l2 = lambda * m[jj] * (1 - alpha);
      if(fabs(z[j] - beta_old[j] * l2) > l1) {
        in_A[j] = in_S[j] = true;
        violations++;
      }
    }
  }
  return violations;
}

/******************************************************************************/

// Gaussian loss
double COPY_gLoss(const NumericVector &r, int n) {
  double l = 0;
  for (int i = 0; i < n; i++) l += pow(r[i], 2);
  return l;
}

/******************************************************************************/

// Coordinate descent for gaussian models
template <class C>
List COPY_cdfit_gaussian_hsr(C xAcc,
                             const NumericVector &y,
                             NumericVector &lambda,
                             int L,
                             int lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             int max_iter,
                             const NumericVector &m,
                             int dfmax,
                             bool verbose) {
  int n = xAcc.nrow(); // number of observations used for fitting model
  int p = xAcc.ncol();
  // printf("p = %d\n", p); //DEBUG

  NumericVector center(p);
  NumericVector scale(p);
  vector<int> col_idx;
  vector<double> z;

  int p_keep = 0; // keep columns whose scale > 1e-6
  double lambda_max = 0.0;

  print_time(verbose);

  // standardize: get center, scale; get p_keep_ptr, col_idx; get z, lambda_max;
  COPY_standardize_and_get_residual(center, scale, &p_keep, col_idx, z,
                                    &lambda_max, xAcc, y, lambda_min,
                                    alpha, n, p);

  p = p_keep;   // set p = p_keep, only loop over columns whose scale > 1e-6
  // printf("p_keep = %d\n", p); //DEBUG

  print_time(verbose, true);

  // Objects to be returned to R
  arma::sp_mat beta = arma::sp_mat(p, L); // beta
  NumericVector beta_old(p);
  NumericVector loss(L);
  IntegerVector iter(L);
  IntegerVector n_reject(L);

  double l1, l2, cutoff, shift, lam_l;
  double max_update, update, thresh, shift_scaled, cpsum;
  int i, j, jj, l, ll, violations, lstart;
  LogicalVector in_A(p); // ever active set
  LogicalVector in_S(p); // strong set
  NumericVector r = Rcpp::clone(y);
  double sumResid = Rcpp::sum(r);
  loss[0] = COPY_gLoss(r, n);
  thresh = eps * loss[0] / n;

  // set up lambda
  if (user) {
    lstart = 0;
  } else {
    if (lam_scale) { // set up lambda, equally spaced on log scale
      double log_lambda_max = log(lambda_max);
      double log_lambda_min = log(lambda_min*lambda_max);

      double delta = (log_lambda_max - log_lambda_min) / (L-1);
      for (l = 0; l < L; l++) {
        lambda[l] = exp(log_lambda_max - l * delta);
      }
    } else { // equally spaced on linear scale
      double delta = (lambda_max - lambda_min*lambda_max) / (L-1);
      for (l = 0; l < L; l++) {
        lambda[l] = lambda_max - l * delta;
      }
    }
    lstart = 1;
    n_reject[0] = p;
  }

  // Path
  for (l = lstart; l < L; l++) {
    print_time(verbose, false, l);
    lam_l = lambda[l];
    if (l != 0) {
      // Check dfmax
      if (Rcpp::sum(beta_old != 0) > dfmax) {
        for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
        return List::create(beta, center, scale, lambda, loss, iter, n_reject,
                            as<IntegerVector>(Rcpp::wrap(col_idx)));
      }
      // strong set
      cutoff = 2 * lam_l - lambda[l-1];
      for (j = 0; j < p; j++) {
        in_S[j] = (fabs(z[j]) > (cutoff * alpha * m[col_idx[j]]));
      }
    } else {
      // strong set
      cutoff = 2 * lam_l - lambda_max;
      for (j = 0; j < p; j++) {
        in_S[j] = (fabs(z[j]) > (cutoff * alpha * m[col_idx[j]]));
      }
    }

    n_reject[l] = p - Rcpp::sum(in_S);

    while(iter[l] < max_iter) {
      while(iter[l] < max_iter){
        while(iter[l] < max_iter) {
          iter[l]++;

          //solve lasso over ever-active set
          max_update = 0.0;
          for (j = 0; j < p; j++) {
            if (in_A[j]) {
              jj = col_idx[j];
              //crossprod_resid - given specific rows of X: separate computation
              cpsum = 0.0;
              for (i = 0; i < n; i++) {
                cpsum += xAcc(i, jj) * r[i];
              }
              cpsum = (cpsum - center[jj] * sumResid) / scale[jj];
              z[j] = cpsum / n + beta_old[j];

              l1 = lam_l * m[jj] * alpha;
              l2 = lam_l * m[jj] * (1-alpha);
              beta(j, l) = COPY_lasso(z[j], l1, l2, 1);

              shift = beta(j, l) - beta_old[j];
              if (shift !=0) {
                // compute objective update for checking convergence
                update = pow(shift, 2);
                if (update > max_update) {
                  max_update = update;
                }
                // update r and sum of residual
                shift_scaled = shift / scale[jj];
                for (i = 0; i < n; i++) {
                  update = shift_scaled * (xAcc(i, jj) - center[jj]);
                  r[i] -= update;
                  sumResid -= update;
                }
                beta_old[j] = beta(j, l); // update beta_old
              }
            }
          }
          // Check for convergence
          if (max_update < thresh) break;
        }

        // Scan for violations in strong set
        violations = COPY_check_strong_set(in_A, in_S, z, xAcc, beta_old, col_idx,
                                           center, scale, lam_l, sumResid, alpha, r, m, n, p);
        if (violations == 0) break;
      }

      // Scan for violations in rest set
      violations = COPY_check_rest_set(in_A, in_S, z, xAcc, beta_old, col_idx,
                                       center, scale, lam_l, sumResid, alpha, r, m, n, p);
      if (violations == 0) {
        loss[l] = COPY_gLoss(r, n);
        break;
      }
    }
  }

  return List::create(beta, center, scale, lambda, loss, iter, n_reject,
                      as<IntegerVector>(Rcpp::wrap(col_idx)));
}

// Dispatch function for COPY_cdfit_gaussian_hsr
// [[Rcpp::export]]
List COPY_cdfit_gaussian_hsr(const S4& BM,
                             const NumericVector& y,
                             const IntegerVector& row_idx,
                             const NumericMatrix& covar,
                             NumericVector& lambda,
                             int L,
                             int lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             int max_iter,
                             const NumericVector &m,
                             int dfmax,
                             bool verbose) {

  XPtr<BigMatrix> xpMat = BM.slot("address");

  if (Rf_inherits(BM, "BM.code")) {
    return COPY_cdfit_gaussian_hsr(
      RawSubMatCovAcc(*xpMat, row_idx, covar, BM.slot("code")),
      y, lambda, L, lam_scale, lambda_min,
      alpha, user, eps, max_iter, m, dfmax, verbose);
  } else {
    switch(xpMat->matrix_type()) {
    case 1:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<char>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 2:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<short>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 4:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<int>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 6:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<float>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    case 8:
      return COPY_cdfit_gaussian_hsr(
        SubMatCovAcc<double>(*xpMat, row_idx, covar),
        y, lambda, L, lam_scale, lambda_min,
        alpha, user, eps, max_iter, m, dfmax, verbose);
    default:
      throw Rcpp::exception(ERROR_TYPE);
    }
  }
}

/******************************************************************************/
