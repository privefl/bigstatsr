// [[Rcpp::depends(BH)]]
#include <boost/variant/variant.hpp>
#include <Rcpp.h>
using namespace Rcpp;

typedef boost::variant<int, double> numeric;

template <typename T>
class Tab {
public :
  
  Tab(std::size_t length) {
    T* tab_[length];
    tab = tab_;
  }
  
  T* tab;
  
  T operator[](std::size_t i) {
    return tab[i];
  }
}; 

Tab getTab(std::string type, std::size_t length) {
  
    if (type == "int") {
      Tab<int> tab(length);
    } else if (type == "double") {
      Tab<double> tab(length);
    }
    
    return tab;
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
