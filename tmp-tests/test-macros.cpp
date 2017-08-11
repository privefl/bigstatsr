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
