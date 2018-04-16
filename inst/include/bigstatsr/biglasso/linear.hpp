#ifndef BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED
#define BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED

/******************************************************************************/
/******         This is a modified version from package biglasso         ******/
/******              https://github.com/YaohuiZeng/biglasso              ******/
/******************************************************************************/

#include <RcppArmadillo.h>

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
  NumericVector metrics(L, NA_REAL);
  double metric, metric_min;
  int no_change = 0;

  // Objects to be returned to R
  arma::sp_mat beta = arma::sp_mat(p, L); // beta
  NumericVector beta_old(p);
  NumericVector loss(L);
  IntegerVector iter(L);

  double l1, l2, cutoff, shift, lam_l;
  double max_update, update, thresh, shift_scaled, cpsum;
  size_t i, j;
  int l, ll, violations;
  LogicalVector in_A(p); // ever active set
  LogicalVector in_S(p); // strong set
  NumericVector r = Rcpp::clone(y);
  double sumResid = Rcpp::sum(r);
  loss[0] = COPY_gLoss(r);
  thresh = eps * loss[0] / n;
  metrics[0] = metric_min = COPY_gLoss(y_val);

  // Path
  for (l = 1; l < L; l++) {

    // Rcout << "Iteration n°" << l << std::endl;

    // Check dfmax
    if (Rcpp::sum(beta_old != 0) > dfmax) {
      for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
      return List::create(beta, loss, iter, metrics);
    }
    // strong set
    lam_l = lambda[l];
    cutoff = 2 * lam_l - lambda[l-1];
    for (j = 0; j < p; j++) {
      in_S[j] = (fabs(resid[j]) > (cutoff * alpha));
    }

    // Approx: no check of rest set
    while(iter[l] < max_iter){
      while(iter[l] < max_iter) {
        iter[l]++;

        //solve lasso over ever-active set
        max_update = 0;
        for (j = 0; j < p; j++) {
          if (in_A[j]) {
            //crossprod_resid - given specific rows of X: separate computation
            cpsum = 0;
            for (i = 0; i < n; i++) {
              cpsum += macc(i, j) * r[i];
            }
            cpsum = (cpsum - center[j] * sumResid) / scale[j];
            resid[j] = cpsum / n + beta_old[j];

            l1 = lam_l * alpha;
            l2 = lam_l - l1;
            beta(j, l) = COPY_lasso(resid[j], l1, l2, 1);

            shift = beta(j, l) - beta_old[j];
            if (shift !=0) {
              // compute objective update for checking convergence
              update = shift * shift;
              if (update > max_update) {
                max_update = update;
              }
              // update r and sum of residual
              shift_scaled = shift / scale[j];
              for (i = 0; i < n; i++) {
                update = shift_scaled * (macc(i, j) - center[j]);
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
      violations = COPY_check_strong_set(in_A, in_S, resid, macc, beta_old,
                                         center, scale, lam_l, sumResid,
                                         alpha, r, n, p);
      if (violations == 0) break;
    }

    loss[l] = COPY_gLoss(r);

    pred_val = predict(macc_val, beta_old, center, scale);
    metric = COPY_gLoss(pred_val - y_val);
    // Rcout << metric << std::endl;
    metrics[l] = metric;
    if (metric < metric_min) {
      metric_min = metric;
      no_change = 0;
    } else if (metric < metrics[l - 1]) {
      if (no_change > 0) no_change--;
    } else {
      no_change++;
    }
    if (l >= nlam_min && no_change >= n_abort) {
      if (warn) Rcout << "Model doesn't improve anymore; exiting..." << std::endl;
      for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
      return List::create(beta, loss, iter, metrics);
    }
  }

  return List::create(beta, loss, iter, metrics);
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED
