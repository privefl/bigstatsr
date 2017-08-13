// /******************************************************************************/
//
// #include <bigstatsr/BMAcc.h>
// #include <Rcpp.h>
//
// using namespace Rcpp;
// using std::size_t;
//
// /******************************************************************************/
//
// template <class C, int RTYPE>
// Vector<RTYPE> extractVec(C macc) {
//
//   size_t K = macc.nelem();
//
//   Vector<RTYPE> res(K);
//
//   for (size_t k = 0; k < K; k++)
//     res[k] = macc[k];
//
//   return res;
// }
//
// Vector<RTYPE> extractVec(RObject xpbm,
//                          const NumericVector& elemInd) {
//
//   XPtr<FBM> xpBM(xpbm);
//   return extractVec(VecBMAcc<double>(xpBM, elemInd - 1));
// }
//
// /******************************************************************************/
//
// template <class C, int RTYPE>
// Vector<RTYPE> extractMat(C macc) {
//
//   size_t n = macc.nrow();
//   size_t m = macc.ncol();
//
//   Matrix<RTYPE> res(n, m);
//
//   for (size_t j = 0; j < m; j++)
//     for (size_t i = 0; i < n; i++)
//       res(i, j) = macc(i, j);
//
//   return res;
// }
//
// Vector<RTYPE> extractMat(RObject xpbm,
//                          const IntegerVector& rowInd,
//                          const IntegerVector& colInd) {
//
//   XPtr<FBM> xpBM(xpbm);
//   return extractMat(SubBMAcc<double>(xpBM, rowInd - 1, colInd - 1));
// }
//
// /******************************************************************************/
