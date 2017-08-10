/******************************************************************************/

#include <bigstatsr/SubMatAcc.h>
#include <bigstatsr/colstats.hpp>

/******************************************************************************/

#include <boost/variant/variant.hpp>

typedef boost::variant<SubMatAcc<char>,
                       SubMatAcc<short>,
                       SubMatAcc<int>,
                       SubMatAcc<float>,
                       SubMatAcc<double> > SubMatAcc_T;

SubMatAcc_T CreateSubMatAcc(int type,
                            BigMatrix &bm,
                            const IntegerVector &row_ind,
                            const IntegerVector &col_ind)
{
  if (type == 1) {
    return SubMatAcc<char>(bm, row_ind, col_ind);
  } else if (type == 2) {
    return SubMatAcc<short>(bm, row_ind, col_ind);
  } else if (type == 4) {
    return SubMatAcc<int>(bm, row_ind, col_ind);
  } else if (type == 6) {
    return SubMatAcc<float>(bm, row_ind, col_ind);
  } else if (type == 8) {
    return SubMatAcc<double>(bm, row_ind, col_ind);
  }
  throw std::runtime_error("Bad type");
}


// Dispatch function for bigcolvars
// [[Rcpp::export]]
ListOf<NumericVector> bigcolvars(const S4& BM,
                                 const IntegerVector& rowInd,
                                 const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  if (Rf_inherits(BM, "BM.code")) {
    return bigstatsr::bigcolvars(RawSubMatAcc(*xpMat, rows, cols,
                                              BM.slot("code")));
  } else {
    return bigstatsr::bigcolvars(CreateSubMatAcc(
        xpMat->matrix_type(), *xpMat, rows, cols
    ));
  }
}

/******************************************************************************/
