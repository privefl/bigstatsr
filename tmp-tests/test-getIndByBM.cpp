#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
void getInd(const IntegerVector& ind,
            const IntegerVector& nrows) {

  int n = ind.size();
  int i, ind_i, k;

  for (i = 0; i < n; i++) {
    ind_i = ind[i] - 1;
    k = 0;
    while (ind_i >= nrows[k]) {
      ind_i -= nrows[k++];
    }
    printf("%d, %d\n", k, ind_i);
  }

  return;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
getInd(1:10, c(3, 3, 4))
*/
