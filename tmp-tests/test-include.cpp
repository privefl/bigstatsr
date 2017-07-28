// [[Rcpp::depends(bigstatsr, bigmemory, BH)]]
#include <bigstatsr/univLinReg.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
ListOf<NumericVector> univLinReg6(const NumericMatrix& mat,
                                  const arma::mat& covar_U,
                                  const arma::vec& y) {

  return univLinReg5(mat, covar_U, y);
}


/*** R
n <- 50
mat <- matrix(0, n, 600); mat[] <- rnorm(length(mat))
covar <- matrix(0, n, 10); covar[] <- rnorm(length(covar))
U <- svd(cbind(1, covar))$u
y <- rnorm(n)
test <- univLinReg6(mat, U, y)

true <- big_univLinReg(as.big.matrix(mat), y.train = y, covar.train = covar)
all.equal(test, true[1:2], check.attributes = FALSE)
*/
