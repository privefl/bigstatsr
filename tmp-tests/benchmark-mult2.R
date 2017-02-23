#' Date: 2017-02-23
#' Object: Test if need C++ matrix multiplication
#' Results: No!! Use big_apply.


require(bigstatsr)

X <- attach.big.matrix("../bigsnpr/backingfiles/popres.desc")
A <- matrix(0, ncol(X), 10)
A[] <- rnorm(length(A))

ind.row <- sample(10)

print(system.time(
  test <- big_prodMat(X, A)
)) # 5 sec

print(system.time(
  test2 <- big_prodMat2(X, A, optim = TRUE)
)) # 19 sec

print(system.time(
  test3 <- big_prodMat2(X, A, optim = TRUE, TILE = 32)
)) # 19 sec

print(system.time(
  true <- X[,] %*% A
)) # 4 sec

print(system.time(
  test <- big_prodMat(X, A, ncores2 = 2)
)) # 7 sec


# /******************************************************************************/
#
#   // [[Rcpp::export]]
# NumericMatrix prodArmaSub(XPtr<BigMatrix> xpMat,
#                           const NumericMatrix &M,
#                           const IntegerVector &rowInd,
#                           const IntegerVector &colInd,
#                           int TILE, bool use_blocks) {
#   SubMatrixAccessor<char> macc(*xpMat, rowInd-1, colInd-1);
#   int n = macc.nrow();
#   int m = macc.ncol();
#   int K = M.ncol();
#   myassert(M.nrow() == m, ERROR_DIM);
#
#   NumericMatrix res(n, K);
#
#   int i, j, k, x, y, z;
#
#   if (use_blocks) {
#     // https://www.quora.com/What-is-the-best-way-to-multiply-two-matrices-in-C++
#       /* Loop over all the tiles, stride by tile size */
#       for (j = 0; j < m; j += TILE)
#         for (i = 0; i < n; i += TILE)
#           for (k = 0; k < K; k += TILE)
#             /* Regular multiply inside the tiles */
#       for (x = j; x < min(j + TILE, m); x++)
#         for (y = i; y < min(i + TILE, n); y++)
#           for (z = k; z < min(k + TILE, K); z++)
#             res(y, z) += macc(y, x) * M(x, z);
#   } else {
#     for (j = 0; j < m; j++) {
#       for (i = 0; i < n; i++) {
#         for (k = 0; k < K; k++) {
#           res(i, k) += macc(i, j) * M(j, k);
#         }
#       }
#     }
#   }
#
#   return res;
# }


# #' @export
# #' @keywords internal
# big_prodMat2 <- function(X, A, ind.row = seq(nrow(X)),
#                          ind.col = seq(ncol(X)),
#                          TILE = 16, optim = FALSE) {
#   prodArmaSub(X@address, A, ind.row, ind.col, TILE, optim)
# }
