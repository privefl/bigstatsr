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
void getSizes() {

  int x = 1;
  size_t y = 1;

  Rprintf("%d ", sizeof(x));
  Rprintf("%d ", sizeof(y));
}

// // [[Rcpp::export]]
// size_t getElem() {
//
//   int x = pow(2, 20);
//
//   size_t y = x * x;
//   intptr_t z = x * x;
//   long w = x * x;
//
//   Rprintf("%ld - %d\n", x, sizeof(x));
//   Rprintf("%ld - %d\n", y, sizeof(y));
//   Rprintf("%ld - %d\n", z, sizeof(z));
//   Rprintf("%ld - %d\n", w, sizeof(w));
//
//   size_t x2 = x;
//
//   y = x2 * x2;
//   z = x2 * x2;
//   w = x2 * x2;
//
//   Rprintf("%ld - %d\n", x2, sizeof(x2));
//   Rprintf("%ld - %d\n", y, sizeof(y));
//   Rprintf("%ld - %d\n", z, sizeof(z));
//   Rprintf("%ld - %d\n", w, sizeof(w));
//
//   return y;
// }


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
getSizes()
x <- 2^33 - 1
as.integer(x)
# getElem()
*/
