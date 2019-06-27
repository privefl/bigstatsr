#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sort_sample(int n) {
  IntegerVector tab(n);
  for (int i = 0; i < n; i++) {
    int k = n * unif_rand();
    tab[k]++;
  }
  return tab;
}

#include <Rcpp.h>
// [[Rcpp::depends(dqrng, sitmo)]]
#include <dqrng_generator.h>
#include <convert_seed.h>
#include <R_randgen.h>
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
Rcpp::IntegerVector sort_sample2(uint32_t n) {
  Rcpp::IntegerVector seed(2, dqrng::R_random_int);
  auto rng = dqrng::generator(dqrng::convert_seed<uint64_t>(seed));
  Rcpp::IntegerVector tab(n);
  for (uint32_t i = 0; i < n; i++) {
    uint32_t k = (*rng)(n);
    tab[k]++;
  }
  return tab;
}

/*** R
sort_sample(42)
plot(print(scale(rowSums(replicate(1e6, sort_sample(50))))))
plot(print(scale(rowSums(replicate(1e6, sort_sample2(50))))))
*/
