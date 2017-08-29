// [[Rcpp::depends(BH)]]
#include <boost/variant/variant.hpp>
#include <Rcpp.h>
using namespace Rcpp;

typedef boost::variant<int, double> numeric;
typedef boost::variant<int*, double*> numeric_ptr;

class Tab {
public :
  
  Tab(std::string type, std::size_t length) {
    if (type == "int") {
      tab = new int[length];
    } else if (type == "double") {
      tab = new double[length];
    }
  }
  
  numeric_ptr tab;
  
  numeric operator[](std::size_t i) {
    return tab[i];
  }
}; 

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
