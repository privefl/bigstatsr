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
double COPY_gLoss(const NumericVector& r, size_t n) {
  double l = 0;
  for (size_t i = 0; i < n; i++) l += pow(r[i], 2);
  return l;
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
                             int dfmax) {

  size_t n = macc.nrow(); // number of observations used for fitting model
  size_t p = macc.ncol();
  size_t L = lambda.size();

  // Objects to be returned to R
  arma::sp_mat beta = arma::sp_mat(p, L); // beta
  NumericVector beta_old(p);
  NumericVector loss(L);
  IntegerVector iter(L);

  double l1, l2, cutoff, shift, lam_l;
  double max_update, update, thresh, shift_scaled, cpsum;
  size_t i, j, l, ll, violations;
  LogicalVector in_A(p); // ever active set
  LogicalVector in_S(p); // strong set
  NumericVector r = Rcpp::clone(y);
  double sumResid = Rcpp::sum(r);
  loss[0] = COPY_gLoss(r, n);
  thresh = eps * loss[0] / n;

  // Path
  for (l = 1; l < L; l++) {

    // Check dfmax
    if (Rcpp::sum(beta_old != 0) > dfmax) {
      for (ll = l; ll < L; ll++) iter[ll] = NA_INTEGER;
      return List::create(beta, loss, iter);
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
              update = pow(shift, 2);
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

    loss[l] = COPY_gLoss(r, n);
  }

  return List::create(beta, loss, iter);
}

} }

/******************************************************************************/

#endif // #ifndef BIGSTATSR_BIGLASSO_LIN_HPP_INCLUDED
