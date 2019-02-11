#ifndef BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED

/******************************************************************************/
/******         This is a modified version from package biglasso         ******/
/******              https://github.com/YaohuiZeng/biglasso              ******/
/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr { namespace biglassoLog {

#include <bigstatsr/biglasso/utils.hpp>

using namespace bigstatsr::biglassoUtils;


/******************************************************************************/

// Weighted sum
// double COPY_wsum(const NumericVector &r, const NumericVector &w) {
//   return std::inner_product(r.begin(), r.end(), w.begin(), 0.0);
// }


// Coordinate descent for logistic models
template <class C>
List COPY_cdfit_binomial_hsr(C macc,
                             const NumericVector& y,
                             const NumericVector& base,
                             const NumericVector& lambda,
                             const NumericVector& center,
                             const NumericVector& scale,
                             NumericVector& z,
                             double alpha,
                             double beta0_old,
                             double eps,
                             int max_iter,
                             int dfmax,
                             C macc_val,
                             const NumericVector& y_val,
                             const NumericVector& base_val,
                             int n_abort,
                             int nlam_min) {

  size_t n = macc.nrow(); // number of observations used for fitting model
  size_t p = macc.ncol();
  int L = lambda.size();

  size_t n_val = macc_val.nrow();
  NumericVector pred_val(n_val);
  NumericVector metrics(L, NA_REAL);
  double metric, metric_min;
  int no_change = 0;

  NumericVector Dev(L, NA_REAL);
  IntegerVector iter(L, NA_INTEGER);
  IntegerVector nb_candidate(L, NA_INTEGER);
  IntegerVector nb_active(L, NA_INTEGER);
  double beta0_max = beta0_old;

  NumericVector beta_max(p);
  NumericVector beta_old(p); // Betas from previous iteration
  NumericVector w(n);
  NumericVector s(n); // y_i - pi_i
  NumericVector r(n);
  NumericVector eta(n);
  LogicalVector in_A(p); // ever-active set
  LogicalVector in_S(p); // strong set
  double xwr, pi, u, v, cutoff, l1, l2, shift, shift_scaled, si, lam_l, dev_l, cj, sj;
  double sumWResid, sum_s, sum_wx_sq, sum_wx, sum_w, x, xw;
  double max_update, update, thresh; // for convergence check
  size_t i, j, violations;

  // compute metric for training set
  double nullDev = 0;
  for (i = 0; i < n; i++) {
    eta[i] = beta0_old + base[i];  // prediction from null model
    pi = 1 / (1 + exp(-eta[i]));
    nullDev -= y[i] * log(pi) + (1 - y[i]) * log(1 - pi);
  }
  thresh = eps * nullDev / n;
  Dev[0] = nullDev;

  // compute metric for validation set
  metric_min = 0;
  for (i = 0; i < n_val; i++) {
    pi = 1 / (1 + exp(-(beta0_old + base_val[i])));
    metric_min -= y_val[i] * log(pi) + (1 - y_val[i]) * log(1 - pi);
  }
  metrics[0] = metric_min;
  nb_active[0] = nb_candidate[0] = iter[0] = 0;

  // Path
  for (int l = 1; l < L; l++) {

    // Rcout << l << std::endl; //DEBUG

    // Check dfmax
    if (Rcpp::sum(beta_old != 0) >= dfmax) {
      return List::create(beta0_max, beta_max, Dev, iter, metrics,
                          "Too many variables", nb_active, nb_candidate);
    }

    lam_l = lambda[l];
    l1 = lam_l * alpha;
    l2 = lam_l - l1;
    // strong set
    cutoff = (2 * lam_l - lambda[l - 1]) * alpha;
    in_S = (abs(z) > cutoff);

    // Approx: no check of rest set
    iter[l] = 0;
    while (iter[l] < max_iter) {
      while (iter[l] < max_iter) {
        iter[l]++;
        dev_l = sum_s = sum_w = 0;
        for (i = 0; i < n; i++) {
          if (eta[i] > 10) {
            pi = 1;
            w[i] = .0001;
          } else if (eta[i] < -10) {
            pi = 0;
            w[i] = .0001;
          } else {
            pi = 1 / (1 + exp(-eta[i]));
            w[i] = pi * (1 - pi);
          }
          sum_w += w[i];
          s[i] = y[i] - pi;
          sum_s += s[i];
          r[i] = s[i] / w[i];
          if (y[i] == 1) {
            dev_l -= log(pi);
          } else {
            dev_l -= log(1 - pi);
          }
        }

        if (dev_l / nullDev < .01) {
          return List::create(beta0_max, beta_max, Dev, iter, metrics,
                              "Model saturated", nb_active, nb_candidate);
        }
        Dev[l] = dev_l;

        // Intercept
        // Rcout << (COPY_wsum(r, w) - sum_s) << std::endl;
        si = sum_s / sum_w;
        if (si != 0) {
          beta0_old += si;
          for (i = 0; i < n; i++) {
            r[i] -= si;   // update r
            eta[i] += si; // update eta
          }
        }
        // update temp result: sum of w * r, used for computing xwr;
        sumWResid = 0;  // COPY_wsum(r, w); Rcout << sumWResid << std::endl;

        max_update = 0;
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
            cj = center[j];
            sj = scale[j];
            xwr = (xwr - cj * sumWResid) / sj;
            v = (sum_wx_sq - 2 * cj * sum_wx + cj * cj * sum_w) / (sj * sj * n);
            u = xwr / n + v * beta_old[j];

            shift = COPY_lasso(u, l1, l2, v) - beta_old[j];
            if (shift != 0) {
              // update change of objective function
              update = shift * shift * v;
              if (update > max_update) max_update = update;

              // Update r & eta
              shift_scaled = shift / sj;
              for (i = 0; i < n; i++) {
                si = shift_scaled * (macc(i, j) - cj);
                r[i] -= si;
                eta[i] += si;
              }
              // update temp result w * r, used for computing xwr;
              // Rcout << (COPY_wsum(r, w) - sumWResid) << std::endl;
              sumWResid += shift_scaled * (cj * sum_w - sum_wx);
              // Rcout << (shift_scaled * (cj * sum_w - sum_wx)) << std::endl;
              // update beta_old
              beta_old[j] += shift;
            }
          }
        }
        // Check for convergence
        if (max_update < thresh)  break;
      }
      // Scan for violations in strong set
      // Rcout << (Rcpp::sum(s) == sum_s) << std::endl;
      violations = COPY_check_strong_set(
        in_A, in_S, z, macc, center, scale, beta_old,
        lam_l, sum_s, alpha, s, n, p
      );
      if (violations == 0) break;
    }

    nb_active[l]    = Rcpp::sum(beta_old != 0);
    nb_candidate[l] = Rcpp::sum(in_A);

    // Get prediction from beta_old
    // pred = predict(macc, beta_old, scale);
    // NumericVector blabla = pred + beta_old - eta;
    // Rcout << blabla << std::endl;
    pred_val = predict(macc_val, beta_old, center, scale) + beta0_old;
    pred_val = 1 / (1 + exp(-(base_val + pred_val)));
    metric = -Rcpp::sum((1 - y_val) * log(1 - pred_val) + y_val * log(pred_val));
    // Rcout << metric << std::endl;
    metrics[l] = metric;
    if (metric < metric_min) {
      beta0_max = beta0_old;
      std::copy(beta_old.begin(), beta_old.end(), beta_max.begin());
      metric_min = metric;
      no_change = 0;
    }
    if (metric > metrics[l - 1]) no_change++;

    if (l >= nlam_min && no_change >= n_abort) {
      return List::create(beta0_max, beta_max, Dev, iter, metrics,
                          "No more improvement", nb_active, nb_candidate);
    }
  }

  return List::create(beta0_max, beta_max, Dev, iter, metrics,
                      "Complete path", nb_active, nb_candidate);
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_LOG_HPP_INCLUDED
