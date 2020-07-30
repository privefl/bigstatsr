#ifndef BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED

/******************************************************************************/
/******         This is a modified version from package biglasso         ******/
/******              https://github.com/YaohuiZeng/biglasso              ******/
/******************************************************************************/

#include <Rcpp.h>

using namespace Rcpp;
using std::size_t;

/******************************************************************************/

namespace bigstatsr { namespace biglassoLin {

#include <bigstatsr/biglasso/utils.hpp>

using namespace bigstatsr::biglassoUtils;


// Gaussian loss
double COPY_gLoss(const NumericVector& r) {
  return std::inner_product(r.begin(), r.end(), r.begin(), 0.0);
}

// Coordinate descent for gaussian models
template <class C>
List COPY_cdfit_gaussian_hsr(C macc,
                             const NumericVector& y,
                             const NumericVector& lambda,
                             const NumericVector& center,
                             const NumericVector& scale,
                             const NumericVector& pf,
                             NumericVector& z,
                             double alpha,
                             double eps,
                             int max_iter,
                             int dfmax,
                             C macc_val,
                             const NumericVector& y_val,
                             int n_abort,
                             int nlam_min,
                             const NumericVector& weights,
                             const NumericVector& weights_val) {

  size_t n = macc.nrow(); // number of observations used for fitting model
  size_t p = macc.ncol();
  int L = lambda.size();

  size_t n_val = macc_val.nrow();
  NumericVector pred_val(n_val);
  double metric, metric_min;
  int no_change = 0;

  NumericVector beta_old(p);
  // Objects to be returned to R
  IntegerVector iter(L, NA_INTEGER);
  NumericVector beta_max(p);
  NumericVector loss(L, NA_REAL);
  NumericVector metrics(L, NA_REAL);
  IntegerVector nb_candidate(L, NA_INTEGER);
  IntegerVector nb_active(L, NA_INTEGER);

  double l1, l2, lam_l, cutoff, shift;
  double max_update, update, thresh, shift_scaled, cpsum;
  double sum_wx, sum_wx_sq, v, x, xw, cj, sj;

  size_t i, j, violations;
  LogicalVector in_A(p); // ever-active set
  LogicalVector in_S(p); // strong set
  NumericVector r = Rcpp::clone(y);
  double sumResid = Rcpp::sum(r);  // always 0..
  double sum_w = Rcpp::sum(weights);
  // Rcout << Rcpp::sum(weights) << std::endl;
  nb_active[0] = nb_candidate[0] = iter[0] = 0;
  loss[0] = COPY_gLoss(r);
  thresh = eps * loss[0] / n;

  NumericVector pred_val_w(n_val);
  NumericVector y_val_w(n_val);
  for (int i = 0; i < n_val; i++) {
    y_val_w[i] = y_val[i] * weights_val[i];
  }
  metrics[0] = metric_min = COPY_gLoss(y_val_w);
  // Path
  for (int l = 1; l < L; l++) {

    // Rcout << "Iteration n°" << l << std::endl;

    // Check dfmax
    if (nb_active[l - 1] >= dfmax) {
      return List::create(beta_max, loss, iter, metrics, "Too many variables",
                          nb_active, nb_candidate);
    }

    lam_l = lambda[l];
    l1 = lam_l * alpha;
    l2 = lam_l - l1;
    // strong set
    cutoff = (2 * lam_l - lambda[l - 1]) * alpha;
    in_S = (abs(z) > (pf * cutoff));

    // Approx: no check of rest set
    iter[l] = 0;
    while (iter[l] < max_iter) {
      while (iter[l] < max_iter) {
        iter[l]++;

        // Solve lasso over ever-active set
        max_update = 0;
        for (j = 0; j < p; j++) {

          if (in_A[j]) {
            // Crossprod_resid - given specific rows of X: separate computation
            cpsum = sum_wx_sq = sum_wx = 0;
            for (i = 0; i < n; i++) {
              x = macc(i, j);
              xw = x * weights[i];
              cpsum += xw * r[i];
              sum_wx += xw;
              sum_wx_sq += x * xw;
            }
            cj = center[j];
            sj = scale[j];
            v = (sum_wx_sq - 2 * cj * sum_wx + cj * cj * sum_w) / (sj * sj * n);
            cpsum = cpsum - cj * sumResid;
            z[j] = cpsum / (sj * n) + beta_old[j];

            shift = COPY_lasso(z[j], l1 * pf[j], l2 * pf[j], v) - beta_old[j];
            if (shift != 0) {
              // compute objective update for checking convergence
              update = shift * shift;
              if (update > max_update) max_update = update;

              // update r (residuals)
              shift_scaled = shift / scale[j];
              sumResid = 0;
              for (i = 0; i < n; i++) {
                r[i] -= shift_scaled * (macc(i, j) - center[j]);
                sumResid += weights[i] * r[i];
              }
              // Rcout << sumResid << std::endl;
              beta_old[j] += shift; // update beta_old
            }
          }
        }
        // Check for convergence
        if (max_update < thresh) break;
      }

      // Scan for violations in strong set
      violations = COPY_check_strong_set(
        in_A, in_S, z, macc, center, scale, pf, beta_old, l1, l2, r, sumResid, weights);
      if (violations == 0) break;
    }

    loss[l] = COPY_gLoss(r);
    nb_active[l]    = Rcpp::sum(beta_old != 0);
    nb_candidate[l] = Rcpp::sum(in_A);

    pred_val = predict(macc_val, beta_old, center, scale);
    for (i = 0; i < n_val; i++) {
      pred_val_w[i] = pred_val[i] * weights_val[i];
    }

    metric = COPY_gLoss(pred_val_w - y_val_w);
    // Rcout << metric << std::endl;
    metrics[l] = metric;
    if (metric < metric_min) {
      std::copy(beta_old.begin(), beta_old.end(), beta_max.begin());
      metric_min = metric;
      no_change = 0;
    }
    if (metric > metrics[l - 1]) no_change++;

    if (l >= nlam_min && no_change >= n_abort) {
      return List::create(beta_max, loss, iter, metrics, "No more improvement",
                          nb_active, nb_candidate);
    }
  }

  return List::create(beta_max, loss, iter, metrics, "Complete path",
                      nb_active, nb_candidate);
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED
