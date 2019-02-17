#ifndef FBM_H
#define FBM_H

/******************************************************************************/

#include <bigstatsr/utils.h>
#include <system_error> // for std::error_code

using std::size_t;

/******************************************************************************/

class FBM {
public:
  FBM(std::string path, size_t n, size_t m, int type) : n(n), m(m), type(type) {
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

#endif // FBM_H
