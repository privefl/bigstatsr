#ifndef FBM_H
#define FBM_H

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <Rcpp.h>


class FBM : private boost::noncopyable {
public:
  FBM(std::string path, std::size_t n, std::size_t m);
  // Rcpp::IntegerVector extract_vector(Rcpp::IntegerVector i);
  // Rcpp::IntegerMatrix extract_matrix(Rcpp::IntegerVector i, Rcpp::IntegerVector j);

  std::size_t nrow() const { return n; }
  std::size_t ncol() const { return m; }
  void* matrix() const { return file_data; }

private:
  boost::interprocess::file_mapping file;
  boost::interprocess::mapped_region file_region;
  void* file_data;
  std::size_t n;
  std::size_t m;
};

#endif // FBM_H
