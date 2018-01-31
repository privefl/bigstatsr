#ifndef BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED

/******************************************************************************/
/******         This is a modified version from package biglasso         ******/
/******              https://github.com/YaohuiZeng/biglasso              ******/
/******************************************************************************/

#include <RcppArmadillo.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr { namespace biglassoLog {

#include <bigstatsr/biglasso/utils.hpp>

using namespace bigstatsr::biglassoUtils;


/******************************************************************************/

template <class C>
void COPY_update_resid_eta(NumericVector& r, NumericVector& eta,
                           C xAcc, size_t jj, double shift,
                           double center_, double scale_, size_t n) {
  double shift_scaled = shift / scale_;
  double si;
  for (size_t i = 0; i < n; i++) {
    si = shift_scaled * (xAcc(i, jj) - center_);
    r[i] -= si;
    eta[i] += si;
  }
}

// Weighted mean
double COPY_wmean(const NumericVector &r, const NumericVector &w, size_t n) {
  double rw_sum = 0, w_sum = 0;
  for (size_t i = 0; i < n; i++) {
    w_sum += w[i];
    rw_sum += r[i] * w[i];
  }
  return rw_sum / w_sum;
}

// Weighted sum
double COPY_wsum(const NumericVector &r, const NumericVector &w, size_t n) {
  double rw_sum = 0;
  for (size_t i = 0; i < n; i++) {
    rw_sum += r[i] * w[i];
  }
  return rw_sum;
}


// Coordinate descent for logistic models
template <class C>
List COPY_cdfit_binomial_hsr(C xAcc,
                             const NumericVector& y,
                             NumericVector& lambda,
                             size_t L,
                             bool lam_scale,
                             double lambda_min,
                             double alpha,
                             bool user,
                             double eps,
                             int max_iter,
                             const NumericVector& m,
                             int dfmax,
                             bool warn,
                             bool verbose) {
  size_t n = xAcc.nrow(); // number of observations used for fitting model
  size_t p = xAcc.ncol();

  NumericVector Dev(L);
  IntegerVector iter(L);
  IntegerVector n_reject(L);
  NumericVector beta0(L);
  NumericVector center(p);
  NumericVector scale(p);
  std::vector<size_t> col_idx;
  std::vector<double> z;

  size_t p_keep = 0; // keep columns whose scale > 1e-6
  double lambda_max = 0.0;

  print_time(verbose);

  // standardize: get center, scale; get p_keep_ptr, col_idx; get z, lambda_max;
  COPY_standardize_and_get_residual(center, scale, &p_keep, col_idx, z, &lambda_max, xAcc,
                                    y, lambda_min, alpha, n, p);

  p = p_keep;   // set p = p_keep, only loop over columns whose scale > 1e-6

  print_time(verbose, true);

  arma::sp_mat beta = arma::sp_mat(p, L); //beta
  NumericVector beta_old(p); //Beta from previous iteration
  double beta_old0 = 0.0; //beta0 from previous iteration
  NumericVector w(n);
  NumericVector s(n); //y_i - pi_i
  NumericVector eta(n);
  LogicalVector in_A(p); // ever active set
  LogicalVector in_S(p); // strong set
  double xwr, pi, u, v, cutoff, l1, l2, shift, si, lam_l;
  double sum_wx_sq, sum_wx, sum_w, tmp, tmp2;
  double max_update, update, thresh; // for convergence check
  size_t i, j, jj, l, ll, violations, lstart;

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
    Rcout << "Iteration: " << l << std::endl;
    print_time(verbose, false, l);
    lam_l = lambda[l];
    if (l != 0) {
      // Check dfmax
      if (Rcpp::sum(beta_old != 0) > dfmax) {
        for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
        return List::create(beta0, beta, center, scale, lambda, Dev, iter, n_reject,
                            asIntVec(col_idx));
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
            for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
            return List::create(beta0, beta, center, scale, lambda, Dev, iter, n_reject,
                                asIntVec(col_idx));
          }

          // Intercept
          si = COPY_wmean(r, w, n);
          beta0[l] = si + beta_old0;
          if (si != 0) {
            beta_old0 = beta0[l];
            for (i = 0; i < n; i++) {
              r[i] -= si; //update r
              eta[i] += si; //update eta
            }
          }
          sumWResid = COPY_wsum(r, w, n); // update temp result: sum of w * r, used for computing xwr;

          max_update = 0.0;
          sum_w = Rcpp::sum(w);
          for (j = 0; j < p; j++) {
            if (in_A[j]) {
              jj = col_idx[j];
              // Weighted cross product of y with jth column of x
              // Weighted sum of squares of jth column of X
              // sum w_i * x_i ^2 = sum w_i * ((x_i - c) / s) ^ 2
              // = 1/s^2 * (sum w_i * x_i^2 - 2 * c * sum w_i x_i + c^2 sum w_i)
              xwr = sum_wx_sq = sum_wx = 0.0;
              for (i = 0; i < n; i++) {
                tmp = xAcc(i, jj);
                tmp2 = tmp * w[i];
                xwr += tmp2 * r[i];
                sum_wx += tmp2;
                sum_wx_sq += tmp * tmp2;
              }
              xwr = (xwr - center[jj] * sumWResid) / scale[jj];
              v = (sum_wx_sq - 2 * center[jj] * sum_wx +
                pow(center[jj], 2) * sum_w) / pow(scale[jj], 2) / n;
              u = xwr / n + v * beta_old[j];
              l1 = lam_l * m[jj] * alpha;
              l2 = lam_l * m[jj] * (1-alpha);
              beta(j, l) = COPY_lasso(u, l1, l2, v);

              shift = beta(j, l) - beta_old[j];
              if (shift !=0) {
                // update change of objective function
                update = pow(shift, 2) * v;
                if (update > max_update) max_update = update;
                COPY_update_resid_eta(r, eta, xAcc, jj, shift, center[jj], scale[jj], n); // update r
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
        violations = COPY_check_strong_set(in_A, in_S, z, xAcc, beta_old, col_idx,
                                           center, scale, lam_l, sumS, alpha, s, m, n, p);
        if (violations == 0) break;
      }
      // Scan for violations in rest
      violations = COPY_check_rest_set(in_A, in_S, z, xAcc, beta_old, col_idx,
                                       center, scale, lam_l, sumS, alpha, s, m, n, p);
      if (violations == 0) break;
    }
  }

  return List::create(beta0, beta, center, scale, lambda, Dev, iter, n_reject,
                      asIntVec(col_idx));
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED
