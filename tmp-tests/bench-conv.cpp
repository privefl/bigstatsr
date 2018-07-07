#include <Rcpp.h>
using namespace Rcpp;

std::string int2name(int RTYPE) {

  switch(RTYPE) {
  case 10: return "logical";
  case 13: return "integer";
  case 14: return "double";
  case 24: return "raw";
  default: return "unknown";
  }
}

template<typename CTYPE>
std::string type2name() {

  double x = CTYPE(1.5);
  if (x == 1.5) return "double";

  int y = CTYPE(-1);
  if (y == -1) return "integer";

  unsigned short z = CTYPE(256);
  if (z == 256) {
    return "unsigned short";
  } else {
    return "unsigned char (raw)";
  }
}

template <int RTYPE_IN, int RTYPE_OUT, typename CTYPE>
Vector<RTYPE_OUT> conv_vec(Vector<RTYPE_IN> nv) {

  int i, n = nv.size();
  Vector<RTYPE_OUT> res(n);
  res.attr("dim") = nv.attr("dim");

  for (i = 0; i < n; i++) {
    res[i] = CTYPE(nv[i]);
    if (nv[i] != res[i]) {
      std::string CTYPE_name = type2name<CTYPE>();
      double res_i = res[i];
      if (CTYPE_name == "unsigned char (raw)") res_i = int(res_i); // printing

      warning("%s (%s -> %s)\n  %s from R type '%s' to C type '%s'.",
              "At least one value changed", nv[i], res_i,
              "while converting", int2name(RTYPE_IN), CTYPE_name);
      break;
    }
  }
  for (; i < n; i++) res[i] = CTYPE(nv[i]);

  return res;
}

// [[Rcpp::export]]
SEXP conv_int(SEXP x) {

  switch(TYPEOF(x)) {
  case RAWSXP: return(as<RawVector>(x));
  case LGLSXP: return(as<LogicalVector>(x));
  case INTSXP: return(as<IntegerVector>(x));
  case REALSXP: return(conv_vec<REALSXP, INTSXP, int>(x));
  default: stop("This R type is not supported.");
  }
}

// [[Rcpp::export]]
RawVector conv_raw(SEXP x) {

  switch(TYPEOF(x)) {
  case RAWSXP: return(as<RawVector>(x));
  case LGLSXP: return(conv_vec<LGLSXP, RAWSXP, unsigned char>(x));
  case INTSXP: return(conv_vec<INTSXP, RAWSXP, unsigned char>(x));
  case REALSXP: return(conv_vec<REALSXP, RAWSXP, unsigned char>(x));
  default: stop("This R type is not supported.");
  }
}

// [[Rcpp::export]]
SEXP conv_ushort(SEXP x) {

  switch(TYPEOF(x)) {
  case RAWSXP: return(as<RawVector>(x));
  case LGLSXP: return(conv_vec<LGLSXP, INTSXP, unsigned short>(x));
  case INTSXP: return(conv_vec<INTSXP, INTSXP, unsigned short>(x));
  case REALSXP: return(conv_vec<REALSXP, INTSXP, unsigned short>(x));
  default: stop("This R type is not supported.");
  }
}

// [[Rcpp::export]]
RawVector conv_raw2(SEXP x) {
  return(as<RawVector>(x));
}

// [[Rcpp::export]]
IntegerVector conv_int2(SEXP x) {
  return as<IntegerVector>(x);
}

// [[Rcpp::export]]
IntegerVector& conv_int3(IntegerVector& x) {
  return x;
}


/*** R
x <- seq_len(1e5)
x2 <- sample(as.raw(0:255), 1e5, TRUE)
x3 <- x + 0
x4 <- x3; x4[length(x4)] <- 1.5
x5 <- as.logical(x)
microbenchmark::microbenchmark(
  test1 <- conv_int(x),
  test2 <- conv_int2(x),
  test3 <- conv_int3(x),
  test4 <- conv_int(x2),
  test5 <- conv_int2(x2),
  test6 <- conv_int(x3),
  test7 <- conv_int2(x3),
  test8 <- conv_int(x4),
  test9 <- conv_int2(x4),
  test10 <- conv_int(x5),
  test11 <- conv_int2(x5)
)

microbenchmark::microbenchmark(
  test1 <- conv_raw(x),
  test2 <- conv_raw2(x),
  test4 <- conv_raw(x2),
  test5 <- conv_raw2(x2),
  test6 <- conv_raw(x3),
  test7 <- conv_raw2(x3),
  test8 <- conv_raw(x4),
  test9 <- conv_raw2(x4),
  test10 <- conv_raw(x5),
  test11 <- conv_raw2(x5)
)

tail(conv_ushort(x))
tail(conv_ushort(x2))
tail(conv_ushort(x4))

microbenchmark::microbenchmark(
  test1 <- conv_ushort(x),
  test2 <- conv_int2(x),
  test4 <- conv_ushort(x2),
  test5 <- conv_int2(x2),
  test6 <- conv_ushort(x3),
  test7 <- conv_int2(x3),
  test8 <- conv_ushort(x4),
  test9 <- conv_int2(x4),
  test10 <- conv_ushort(x5),
  test11 <- conv_int2(x5)
)

# x3[1] <- 1.5
# head(conv_int2(x3))
# head(conv_int(x3))
(x5 <- matrix(1:4, 2))
conv_int(x5)
conv_int(x5 + 0)
x6 <- x5
storage.mode(x6) <- "raw"
conv_int(x6)
conv_ushort(x6)
conv_raw(x6)
#
#
# x3[1] <- NA
# head(conv_int(x3))
*/
