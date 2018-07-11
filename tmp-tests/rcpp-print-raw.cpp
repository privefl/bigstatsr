#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void print_raw(RawVector x) {

  for (int i = 0; i < x.size(); i++) {
    Rcout << x[i] << " ";
  }
  Rcout << std::endl;
}

// [[Rcpp::export]]
void print_raw2(RawVector x) {
  for (int v : x) {
    Rcout << std::hex << std::setw(2) << std::setfill('0') << v << ' ';
  }
  Rcout << '\n';
}

// [[Rcpp::export]]
int conv_ushort(int x) {

  unsigned short y = x;
  return y;
}

/*** R
x <- as.raw(0:10)
print(x)
print_raw(x)
print_raw2(x)

conv_ushort(1)
conv_ushort(0)
conv_ushort(-1)
conv_ushort(2^16)
*/
