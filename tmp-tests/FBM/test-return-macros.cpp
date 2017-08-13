#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

template <int RTYPE>
int len(const Vector<RTYPE>& x) 
{
  return static_cast<int>(x.size());
}

} // impl

// [[Rcpp::export]]
int len(RObject x) 
{
  RCPP_RETURN_VECTOR(impl::len, x);
}

/*** R
classes <- c(
  "integer", "numeric", "raw", "logical",
  "complex", "character", "list", "expression"
)
sapply(seq_along(classes), function(i) {
  x <- vector(mode = classes[i], length = i)
  all.equal(len(x), length(x))
})
*/