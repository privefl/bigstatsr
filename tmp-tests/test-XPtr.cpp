#include <Rcpp.h>
using namespace Rcpp;

class Index {
public:
  Index(int i_) : i(i_) {}
  int getI() { return i; }
private:
  int i;
};

// [[Rcpp::export]]
SEXP getXPtrIndex(int i) {
  Rcout << "getXPtrIndex: i = " << i << std::endl;
  XPtr<Index> ptr(new Index(i), true);
  Rcout << "getXPtrIndex: ind.i = " << ptr->getI() << std::endl;
  return ptr;
}

// [[Rcpp::export]]
void getXPtrIndexValue(SEXP ptr) {
  XPtr<Index> ind_ptr(ptr);
  Rcout << "getXPtrIndexValue: ind_ptr->i = " << ind_ptr->getI() << std::endl;
  Index ind = *ind_ptr;
  Rcout << "getXPtrIndexValue: ind.i = " << ind.getI() << std::endl;
}


/*** R
(extptr <- getXPtrIndex(20))
getXPtrIndexValue(extptr)
*/
