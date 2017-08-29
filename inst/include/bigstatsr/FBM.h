#ifndef FBM_H
#define FBM_H

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <Rcpp.h>

using std::size_t;

class FBM : private boost::noncopyable {
public:
  FBM(std::string path, size_t n, size_t m, int type);

  void* matrix() const { return file_data; }
  size_t nrow() const { return n; }
  size_t ncol() const { return m; }
  int matrix_type() const { return type; }

private:
  boost::interprocess::file_mapping file;
  boost::interprocess::mapped_region file_region;
  void* file_data;
  size_t n;
  size_t m;
  int type;
};

#endif // FBM_H
