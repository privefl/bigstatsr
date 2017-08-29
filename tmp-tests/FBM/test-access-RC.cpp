// [[Rcpp::depends(bigstatsr, BH)]]
#include <bigstatsr/BMCodeAcc.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double access3(SEXP sexp) {

  XPtr<FBM> xpBM(sexp);
  double res;
  if (xpBM.hasAttribute("code256")) {
    SubBMCode256Acc macc(xpBM,
                         seq_len(xpBM->nrow()) - 1,
                         seq_len(xpBM->ncol()) - 1,
                         xpBM.attr("code256"));
    res = macc(1, 1);
  } else {
    res = -1;
  }

  return res;
}

// [[Rcpp::export]]
double access4(Environment x) {

  bool isFBM = Rf_inherits(x, "FBM");
  XPtr<FBM> xpBM = x["address"];
  CharacterVector names = wrap(x.attributeNames());
  Rcout << x.exists("code256") << std::endl;
  double res;
  if (x.exists("code256")) {
    SubBMCode256Acc macc(xpBM,
                         seq_len(xpBM->nrow()) - 1,
                         seq_len(xpBM->ncol()) - 1,
                         x["code256"]);
    res = macc(1, 1);
  } else {
    res = -1;
  }

  return res;
}

/*** R
test <- new_FBM(10, 10)
access3(test$address)
access4(test)
test2 <- FBM.code256(10, 10, code = 1:256)
access3(structure(test2$address, code256 = 1:256))
access4(test2)
*/
