// COPIED FROM https://github.com/davidgohel/fpeek/blob/master/src/fpeek.cpp

#include <Rcpp.h>
#include <fstream>

// [[Rcpp::export]]
int nlines_(std::string filename) {
  std::ifstream is(filename.c_str(), std::ifstream::binary);
  int count = 0;
  if( !is )
    Rcpp::stop("error while opening filename");

  std::istream::sentry se(is, true);
  std::streambuf* sb = is.rdbuf();

  for(;;) {
    int c = sb->sbumpc();
    if( c == '\n'){
      count++;
    } else if( c == '\r'){
      if(sb->sgetc() == '\n'){
        sb->sbumpc();
      }
      count++;
    } else if( c == std::streambuf::traits_type::eof() ){
      // count++;
      break;
    }
  }

  is.close();
  return count;
}
