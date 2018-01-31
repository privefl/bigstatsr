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

// Weighted mean
double COPY_wmean(const NumericVector& r, const NumericVector& w, size_t n) {
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
List COPY_cdfit_binomial_hsr(C macc,
                             const NumericVector& y,
                             const NumericVector& lambda,
                             const NumericVector& center,
                             const NumericVector& scale,
                             NumericVector& resid,
                             double alpha,
                             double eps,
                             int max_iter,
                             int dfmax,
                             bool warn,
                             C macc_val,
                             const NumericVector& y_val,
                             int n_abort,
                             int nlam_min) {

  size_t n = macc.nrow(); // number of observations used for fitting model
  size_t p = macc.ncol();
  int L = lambda.size();

  size_t n_val = macc_val.nrow();
  NumericVector pred_val(n_val);
  NumericVector metrics(L, R_NegInf);
  double metric, metric_max = R_NegInf;
  int no_change = 0;

  NumericVector Dev(L);
  IntegerVector iter(L);
  NumericVector beta0(L);

  arma::sp_mat beta = arma::sp_mat(p, L); //beta
  NumericVector beta_old(p); //Beta from previous iteration
  double beta_old0 = 0; //beta0 from previous iteration
  NumericVector w(n);
  NumericVector s(n); //y_i - pi_i
  NumericVector eta(n);
  LogicalVector in_A(p); // ever active set
  LogicalVector in_S(p); // strong set
  double xwr, pi, u, v, cutoff, l1, l2, shift, shift_scaled, si, lam_l;
  double sum_wx_sq, sum_wx, sum_w, x, xw;
  double max_update, update, thresh; // for convergence check
  size_t i, j;
  int l, ll, violations;

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

  double sumWResid = 0; // temp result: sum of w * r
  Dev[0] = nullDev;

  // Path
  for (l = 1; l < L; l++) {

    // Rcout << l << std::endl; //DEBUG

    // Check dfmax
    if (Rcpp::sum(beta_old != 0) > dfmax) {
      for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
      return List::create(beta0, beta, Dev, iter, metrics);
    }

    // strong set
    lam_l = lambda[l];
    cutoff = 2 * lam_l - lambda[l - 1];
    for (j = 0; j < p; j++) {
      in_S[j] = (fabs(resid[j]) > (cutoff * alpha));
    }

    // Approx: no check of rest set
    while (iter[l] < max_iter) {
      while (iter[l] < max_iter) {
        iter[l]++;
        Dev[l] = 0;

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
          return List::create(beta0, beta, Dev, iter, metrics);
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

        max_update = 0;
        sum_w = Rcpp::sum(w);
        for (j = 0; j < p; j++) {
          if (in_A[j]) {
            // Weighted cross product of y with jth column of x
            // Weighted sum of squares of jth column of X
            // sum w_i * x_i ^2 = sum w_i * ((x_i - c) / s) ^ 2
            // = 1/s^2 * (sum w_i * x_i^2 - 2 * c * sum w_i x_i + c^2 sum w_i)
            xwr = sum_wx_sq = sum_wx = 0;
            for (i = 0; i < n; i++) {
              x = macc(i, j);
              xw = x * w[i];
              xwr += xw * r[i];
              sum_wx += xw;
              sum_wx_sq += x * xw;
            }
            xwr = (xwr - center[j] * sumWResid) / scale[j];
            v = (sum_wx_sq - 2 * center[j] * sum_wx +
              pow(center[j], 2) * sum_w) / pow(scale[j], 2) / n;
            u = xwr / n + v * beta_old[j];
            l1 = lam_l * alpha;
            l2 = lam_l - l1;
            beta(j, l) = COPY_lasso(u, l1, l2, v);

            shift = beta(j, l) - beta_old[j];
            if (shift != 0) {
              // update change of objective function
              update = pow(shift, 2) * v;
              if (update > max_update) max_update = update;

              // Update resid & eta
              shift_scaled = shift / scale[j];
              for (size_t i = 0; i < n; i++) {
                si = shift_scaled * (macc(i, j) - center[j]);
                r[i] -= si;
                eta[i] += si;
              }

              sumWResid = COPY_wsum(r, w, n); // update temp result w * r, used for computing xwr;
              beta_old[j] = beta(j, l); // update beta_old
            }
          }
        }
        // Check for convergence
        if (max_update < thresh)  break;
      }
      // Scan for violations in strong set
      violations = COPY_check_strong_set(in_A, in_S, resid, macc, beta_old,
                                         center, scale, lam_l, Rcpp::sum(s),
                                         alpha, s, n, p);
      if (violations == 0) break;
    }

    // Get prediction from beta_old
    // pred = predict(macc, beta_old, scale);
    // NumericVector blabla = pred + beta0[l] - eta;
    // Rcout << blabla << std::endl;
    pred_val = predict(macc_val, beta_old, center, scale) + beta0[l];
    pred_val = 1 / (1 + exp(-pred_val));
    metric = Rcpp::sum((1 - y_val) * log(1 - pred_val) + y_val * log(pred_val));
    // Rcout << metric << std::endl;
    metrics[l] = metric;
    if (metric > metric_max) {
      metric_max = metric;
      no_change = 0;
    } else if (metric > metrics[l - 1]) {
      if (no_change > 0) no_change--;
    } else {
      no_change++;
    }
    if (l >= nlam_min && no_change >= n_abort) {
      if (warn) Rcout << "Model doesn't improve anymore; exiting..." << std::endl;
      for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
      return List::create(beta0, beta, Dev, iter, metrics);
    }
  }

  return List::create(beta0, beta, Dev, iter, metrics);
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED
