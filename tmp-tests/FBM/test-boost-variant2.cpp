// [[Rcpp::depends(BH)]]
#include <boost/variant/variant.hpp>
#include <Rcpp.h>
using namespace Rcpp;

template <typename T>
class Tab {
public:
  Tab(std::size_t length) : tab(length) {}
  
  T operator[](std::size_t i) const { return tab[i]; }
  // ...
private:
  std::vector<T> tab;
}; 

boost::variant<Tab<int>, Tab<double> > CreateTab(const std::string& type, std::size_t length)
{
  if (type == "int") {
    return Tab<int>(length);
  } else if (type == "double") {
    return Tab<double>(length);
  }
  throw std::runtime_error("Bad type");
}

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
