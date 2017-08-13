#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
int SIZEOF() {
  return sizeof(T);
}

// [[Rcpp::export]]
int SIZEOF(std::string type) {
  if (type == "unsigned char") return SIZEOF<unsigned char>();
  if (type == "unsigned short") return SIZEOF<unsigned short>();
  if (type == "int") return SIZEOF<int>();
  if (type == "double") return SIZEOF<double>();
  return -1;
}


/*** R
SIZEOF("char")
SIZEOF("unsigned char")
SIZEOF("unsigned short")
SIZEOF("int")
SIZEOF("double")
*/
