// [[Rcpp::depends(bigsnpr)]]
#include <bigsnpr/SubTripleAcc.h>
#include <bigstatsr/prodMatVec.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pMatVec4_triple(const S4& BM,
                              const NumericVector& x,
                              const IntegerVector& rowInd,
                              const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  return bigstatsr::pMatVec4(RawSubTripleAcc(*xpMat, rows, cols,
                                             BM.slot("code")), x);
}

// [[Rcpp::export]]
NumericVector cpMatVec4_triple(const S4& BM,
                               const NumericVector& x,
                               const IntegerVector& rowInd,
                               const IntegerVector& colInd) {

  XPtr<BigMatrix> xpMat = BM.slot("address");
  IntegerVector rows = rowInd - 1;
  IntegerVector cols = colInd - 1;

  return bigstatsr::cpMatVec4(RawSubTripleAcc(*xpMat, rows, cols,
                                              BM.slot("code")), x);
}

/*** R
# single core implementation
svds4.seq <- function(X., fun.scaling, ind.row, ind.col, k, tol, verbose) {
  n <- length(ind.row)
  m <- length(ind.col)
  X <- attach.BM(X.)

  # scaling
  ms <- fun.scaling(X, ind.row, ind.col)

  printf <- function(...) if (verbose) cat(sprintf(...))
  it <- 0
  # A
  A <- function(x, args) {
    printf("%d - computing A * x\n", it <<- it + 1)
    x <- x / ms$sd
    pMatVec4_triple(X, x, ind.row, ind.col) - crossprod(x, ms$mean)
  }
  # Atrans
  Atrans <- function(x, args) {
    printf("%d - computing At * x\n", it <<- it + 1)
    (cpMatVec4_triple(X, x, ind.row, ind.col) - sum(x) * ms$mean) / ms$sd
  }

  res <- RSpectra::svds(A, k, nu = k, nv = k, opts = list(tol = tol),
                        Atrans = Atrans, dim = c(n, 3 * m))

  res$means <- ms$mean
  res$sds <- ms$sd

  res
}

big_scaleTriple <- function(X., ind.row = rows_along(X.),
                            ind.col = cols_along(X.)) {

  X1 <- X2 <- X3 <- attach.BM(X.)
  X2@code <- as.numeric(X1@code >= 0.5)
  X3@code <- as.numeric(X1@code > 1.5)

  tmp1 <- big_colstats(X1, ind.row, ind.col)
  tmp2 <- big_colstats(X2, ind.row, ind.col)
  tmp3 <- big_colstats(X3, ind.row, ind.col)


  means <- as.vector(rbind(
    tmp1$sum,
    tmp2$sum,
    tmp3$sum
  )) / length(ind.row)

  sds <- sqrt(as.numeric(rbind(
    tmp1$var,
    tmp2$var,
    tmp3$var
  )))

  data.frame(mean = means, sd = sds)
}
*/
