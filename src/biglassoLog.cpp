#include "../inst/include/biglasso.h"
#include <time.h>

/******************************************************************************/

void print_time2(bool verbose, bool end = false, int l = -1) {
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
template <typename T>
void COPY2_standardize_and_get_residual(NumericVector &center,
                                        NumericVector &scale,
                                        int *p_keep_ptr,
                                        vector<int> &col_idx, //columns to keep, removing columns whose scale < 1e-6
                                        vector<double> &z,
                                        double *lambda_max_ptr,
                                        SubMatrixAccessor<T> xAcc,
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

double COPY2_lasso(double z, double l1, double l2, double v) {
  double s=0;
  if (z > 0) s = 1;
  else if (z < 0) s = -1;
  if (fabs(z) <= l1) return(0);
  else return(s*(fabs(z)-l1)/(v*(1+l2)));
}

/******************************************************************************/

// check KKT conditions over features in the strong set
template <typename T>
int COPY2_check_strong_set(LogicalVector &in_A, const LogicalVector &in_S, vector<double> &z,
                           SubMatrixAccessor<T> xAcc, const NumericVector &beta_old,
                           const vector<int> &col_idx,
                           const NumericVector &center, const NumericVector &scale,
                           double lambda, double sumResid, double alpha,
                           const NumericVector &r, const NumericVector &m, int n, int p) {
  double sum, l1, l2;
  int j, jj, violations = 0;

  for (j = 0; j < p; j++) {
    if (!in_A[j] && in_S[j]) {
      jj = col_idx[j];
      sum = 0.0;
      for (int i = 0; i < n; i++) {
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
template <typename T>
int COPY2_check_rest_set(LogicalVector &in_A, LogicalVector &in_S, vector<double> &z,
                         SubMatrixAccessor<T> xAcc, const NumericVector &beta_old,
                         const vector<int> &col_idx,
                         const NumericVector &center, const NumericVector &scale,
                         double lambda, double sumResid, double alpha,
                         const NumericVector &r, const NumericVector &m, int n, int p) {
  double sum, l1, l2;
  int j, jj, violations = 0;

  for (j = 0; j < p; j++) {
    if (!in_S[j]) {
      jj = col_idx[j];
      sum = 0.0;
      for (int i = 0; i < n; i++) {
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

template <typename T>
void COPY_update_resid_eta(NumericVector &r, NumericVector &eta,
                           SubMatrixAccessor<T> xAcc, int jj, double shift,
                           double center_, double scale_, int n) {
  double shift_scaled = shift / scale_;
  double si;
  for (int i = 0; i < n; i++) {
    si = shift_scaled * (xAcc(i, jj) - center_);
    r[i] -= si;
    eta[i] += si;
  }
}

// Weighted mean
double COPY_wmean(const NumericVector &r, const NumericVector &w, int n) {
  double rw_sum = 0, w_sum = 0;
  for (int i = 0; i < n; i++) {
    w_sum += w[i];
    rw_sum += r[i] * w[i];
  }
  return rw_sum / w_sum;
}

// Weighted sum
double COPY_wsum(const NumericVector &r, const NumericVector &w, int n) {
  double rw_sum = 0;
  for (int i = 0; i < n; i++) {
    rw_sum += r[i] * w[i];
  }
  return rw_sum;
}

// Weighted cross product of y with jth column of x
template <typename T>
double COPY_wcrossprod_resid(SubMatrixAccessor<T> xAcc, int jj, const NumericVector &y,
                             double sumYW_, double center_, double scale_,
                             const NumericVector &w, int n_row) {
  double val = 0.0;
  for (int i = 0; i < n_row; i++) {
    val += xAcc(i, jj) * y[i] * w[i];
  }
  val = (val - center_ * sumYW_) / scale_;

  return val;
}

// Weighted sum of squares of jth column of X
// sum w_i * x_i ^2 = sum w_i * ((x_i - c) / s) ^ 2
// = 1/s^2 * (sum w_i * x_i^2 - 2 * c * sum w_i x_i + c^2 sum w_i)
template <typename T>
double COPY_wsqsum_bm(SubMatrixAccessor<T> xAcc, int jj, const NumericVector &w,
                      double center_, double scale_, int n_row) {
  double sum_wx_sq = 0.0;
  double sum_wx = 0.0;
  double sum_w = 0.0;
  double tmp;
  for (int i = 0; i < n_row; i++) {
    tmp = xAcc(i, jj);
    sum_wx_sq += w[i] * pow(tmp, 2);
    sum_wx += w[i] * tmp;
    sum_w += w[i]; // TODO: pre-compute SUM_W and
  }
  return (sum_wx_sq - 2 * center_ * sum_wx +
          pow(center_, 2) * sum_w) / pow(scale_, 2);
}

// Coordinate descent for logistic models
template <typename T>
List COPY_cdfit_binomial_hsr(SubMatrixAccessor<T> xAcc,
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
                             bool warn,
                             bool verbose) {
  int n = xAcc.nrow(); // number of observations used for fitting model
  int p = xAcc.ncol();

  NumericVector Dev(L);
  IntegerVector iter(L);
  IntegerVector n_reject(L);
  NumericVector beta0(L);
  NumericVector center(p);
  NumericVector scale(p);
  vector<int> col_idx;
  vector<double> z;

  int p_keep = 0; // keep columns whose scale > 1e-6
  double lambda_max = 0.0;

  print_time2(verbose);

  // standardize: get center, scale; get p_keep_ptr, col_idx; get z, lambda_max;
  COPY2_standardize_and_get_residual<T>(center, scale, &p_keep, col_idx, z, &lambda_max, xAcc,
                                        y, lambda_min, alpha, n, p);

  p = p_keep;   // set p = p_keep, only loop over columns whose scale > 1e-6

  print_time2(verbose, true);

  arma::sp_mat beta = arma::sp_mat(p, L); //beta
  NumericVector beta_old(p); //Beta from previous iteration
  double beta_old0 = 0.0; //beta0 from previous iteration
  NumericVector w(n);
  NumericVector s(n); //y_i - pi_i
  NumericVector eta(n);
  LogicalVector in_A(p); // ever active set
  LogicalVector in_S(p); // strong set
  double xwr, pi, u, v, cutoff, l1, l2, shift, si, lam_l;
  double max_update, update, thresh; // for convergence check
  int i, j, jj, l, violations, lstart;

  double ybar = Rcpp::sum(y) / n;
  beta_old0 = beta0[0] = log(ybar / (1-ybar));
  double nullDev = 0;
  NumericVector r(n);
  for (i = 0; i < n; i++) {
    r[i] = y[i];
    nullDev = nullDev - y[i]*log(ybar) - (1-y[i])*log(1-ybar);
    s[i] = y[i] - ybar;
    eta[i] = beta_old0;
  }
  thresh = eps * nullDev / n;

  double sumS = Rcpp::sum(s); // temp result sum of s
  double sumWResid = 0.0; // temp result: sum of w * r

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
    Dev[0] = nullDev;
    lstart = 1;
    n_reject[0] = p;
  }

  // Path
  for (l = lstart; l < L; l++) {
    print_time2(verbose, false, l);
    lam_l = lambda[l];
    if (l != 0) {
      // Check dfmax
      if (Rcpp::sum(beta_old != 0) > dfmax) {
        for (int ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
        return List::create(beta0, beta, center, scale, lambda, Dev, iter, n_reject,
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

    while (iter[l] < max_iter) {
      while (iter[l] < max_iter) {
        while (iter[l] < max_iter) {
          iter[l]++;
          Dev[l] = 0.0;

          for (i = 0; i < n; i++) {
            if (eta[i] > 10) {
              pi = 1;
              w[i] = .0001;
            } else if (eta[i] < -10) {
              pi = 0;
              w[i] = .0001;
            } else {
              pi = exp(eta[i]) / (1 + exp(eta[i]));
              w[i] = pi * (1 - pi);
            }
            s[i] = y[i] - pi;
            r[i] = s[i] / w[i];
            if (y[i] == 1) {
              Dev[l] = Dev[l] - log(pi);
            } else {
              Dev[l] = Dev[l] - log(1-pi);
            }
          }

          if (Dev[l] / nullDev < .01) {
            if (warn) warning("Model saturated; exiting...");
            for (int ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
            return List::create(beta0, beta, center, scale, lambda, Dev, iter, n_reject,
                                as<IntegerVector>(Rcpp::wrap(col_idx)));
          }

          // Intercept
          beta0[l] = COPY_wmean(r, w, n) + beta_old0;
          si = beta0[l] - beta_old0;
          if (si != 0) {
            beta_old0 = beta0[l];
            for (i = 0; i < n; i++) {
              r[i] -= si; //update r
              eta[i] += si; //update eta
            }
          }
          sumWResid = COPY_wsum(r, w, n); // update temp result: sum of w * r, used for computing xwr;

          max_update = 0.0;
          for (j = 0; j < p; j++) {
            if (in_A[j]) {
              jj = col_idx[j];
              xwr = COPY_wcrossprod_resid<T>(xAcc, jj, r, sumWResid, center[jj], scale[jj], w, n);
              v = COPY_wsqsum_bm<T>(xAcc, jj, w, center[jj], scale[jj], n) / n;
              u = xwr / n + v * beta_old[j];
              l1 = lam_l * m[jj] * alpha;
              l2 = lam_l * m[jj] * (1-alpha);
              beta(j, l) = COPY2_lasso(u, l1, l2, v);

              shift = beta(j, l) - beta_old[j];
              if (shift !=0) {
                // update change of objective function
                update = pow(shift, 2) * v;
                if (update > max_update) max_update = update;
                COPY_update_resid_eta<T>(r, eta, xAcc, jj, shift, center[jj], scale[jj], n); // update r
                sumWResid = COPY_wsum(r, w, n); // update temp result w * r, used for computing xwr;
                beta_old[j] = beta(j, l); // update beta_old
              }
            }
          }
          // Check for convergence
          if (max_update < thresh)  break;
        }
        // Scan for violations in strong set
        sumS = Rcpp::sum(s);
        violations = COPY2_check_strong_set<T>(in_A, in_S, z, xAcc, beta_old, col_idx,
                                               center, scale, lam_l, sumS, alpha, s, m, n, p);
        if (violations == 0) break;
      }
      // Scan for violations in rest
      violations = COPY2_check_rest_set<T>(in_A, in_S, z, xAcc, beta_old, col_idx,
                                           center, scale, lam_l, sumS, alpha, s, m, n, p);
      if (violations == 0) break;
    }
  }

  return List::create(beta0, beta, center, scale, lambda, Dev, iter, n_reject,
                      as<IntegerVector>(Rcpp::wrap(col_idx)));
}

// Dispatch function for COPY_cdfit_binomial_hsr
// [[Rcpp::export]]
List COPY_cdfit_binomial_hsr(XPtr<BigMatrix> xpMat,
                             const NumericVector &y,
                             const IntegerVector &row_idx,
                             const NumericMatrix &covar,
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
                             bool warn,
                             bool verbose) {
  switch(xpMat->matrix_type()) {
  case 1:
    return COPY_cdfit_binomial_hsr(SubMatrixAccessor<char>(*xpMat, row_idx, covar),
                                   y, lambda, L, lam_scale, lambda_min,
                                   alpha, user, eps, max_iter, m,
                                   dfmax, warn, verbose);
  case 2:
    return COPY_cdfit_binomial_hsr(SubMatrixAccessor<short>(*xpMat, row_idx, covar),
                                   y, lambda, L, lam_scale, lambda_min,
                                   alpha, user, eps, max_iter, m,
                                   dfmax, warn, verbose);
  case 4:
    return COPY_cdfit_binomial_hsr(SubMatrixAccessor<int>(*xpMat, row_idx, covar),
                                   y, lambda, L, lam_scale, lambda_min,
                                   alpha, user, eps, max_iter, m,
                                   dfmax, warn, verbose);
  case 6:
    return COPY_cdfit_binomial_hsr(SubMatrixAccessor<float>(*xpMat, row_idx, covar),
                                   y, lambda, L, lam_scale, lambda_min,
                                   alpha, user, eps, max_iter, m,
                                   dfmax, warn, verbose);
  case 8:
    return COPY_cdfit_binomial_hsr(SubMatrixAccessor<double>(*xpMat, row_idx, covar),
                                   y, lambda, L, lam_scale, lambda_min,
                                   alpha, user, eps, max_iter, m,
                                   dfmax, warn, verbose);
  default:
    throw Rcpp::exception(ERROR_TYPE);
  }
}

/******************************************************************************/
