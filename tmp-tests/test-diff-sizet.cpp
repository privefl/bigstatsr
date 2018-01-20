#include <Rcpp.h>
using namespace Rcpp;
using std::size_t;


// [[Rcpp::export]]
int diff_size_t(size_t i, size_t j) {
  return i - j;
}


/*** R
diff_size_t(1, 2)
diff_size_t(3, 2)
diff_size_t(1, 400000)
*/
