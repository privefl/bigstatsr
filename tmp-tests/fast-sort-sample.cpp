#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sort_sample(int n) {
  IntegerVector tab(n);
  for (int i = 0; i < n; i++) {
    int k = n * unif_rand();
    tab[k]++;
  }
  return tab;
}

/*** R
sort_sample(42)
*/
