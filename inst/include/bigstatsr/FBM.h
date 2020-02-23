#ifndef FBM_H
#define FBM_H

/******************************************************************************/

#include <bigstatsr/utils.h>
#include <system_error> // for std::error_code

using std::size_t;

/******************************************************************************/

// Read-only memory-mapping
class FBM {
public:
  FBM(std::string path, size_t n, size_t m, int type) : n(n), m(m), type(type) {
    std::error_code error;
    this->ro_mmap.map(path, error);

    if (error) Rcpp::stop("Error when mapping file:\n  %s.\n", error.message());
  }

  const void* matrix() { return ro_mmap.data(); }
  size_t nrow() const { return n; }
  size_t ncol() const { return m; }
  int matrix_type() const { return type; }

private:
  mio::mmap_source ro_mmap;
  size_t n;
  size_t m;
  int type;
};

/******************************************************************************/

// Read/write memory-mapping
class FBM_RW {
public:
  FBM_RW(std::string path, size_t n, size_t m, int type) : n(n), m(m), type(type) {
    std::error_code error;
    this->rw_mmap.map(path, error);

    if (error) Rcpp::stop("Error when mapping file:\n  %s.\n", error.message());
  }

  void* matrix() { return rw_mmap.data(); }
  size_t nrow() const { return n; }
  size_t ncol() const { return m; }
  int matrix_type() const { return type; }

private:
  mio::mmap_sink rw_mmap;
  size_t n;
  size_t m;
  int type;
};

/******************************************************************************/

inline arma::mat FBM2arma(Rcpp::Environment BM) {

  Rcpp::XPtr<FBM> xpBM = BM["address"];
  myassert(xpBM->matrix_type() == 8,
           "Mapping to arma::mat is available for 'double' FBMs only.");

  return arma::mat((double*)xpBM->matrix(), xpBM->nrow(), xpBM->ncol(), false);
}

inline arma::mat FBM_RW2arma(Rcpp::Environment BM) {

  Rcpp::XPtr<FBM_RW> xpBM = BM["address_rw"];
  myassert(xpBM->matrix_type() == 8,
           "Mapping to arma::mat is available for 'double' FBMs only.");

  return arma::mat((double*)xpBM->matrix(), xpBM->nrow(), xpBM->ncol(), false);
}

/******************************************************************************/

#endif // FBM_H
