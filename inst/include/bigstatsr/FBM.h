#ifndef FBM_H
#define FBM_H

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/noncopyable.hpp>
#include <Rcpp.h>

template <typename T>
class FBM : private boost::noncopyable {
public:
  FBM(std::string path, std::size_t n, std::size_t p);
  // Rcpp::IntegerVector extract_vector(Rcpp::IntegerVector i);
  // Rcpp::IntegerMatrix extract_matrix(Rcpp::IntegerVector i, Rcpp::IntegerVector j);
private:
  boost::interprocess::file_mapping file;
  boost::interprocess::mapped_region file_region;
  T* file_data;
  std::size_t nrow;
  std::size_t ncol;
};

#endif // FBM_H
