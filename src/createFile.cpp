#include <fstream>
#include <stdexcept>

template <typename T>
void createFile(std::string fileName,
                std::size_t nrow,
                std::size_t ncol) {

  try {

    std::filebuf fbuf;

    fbuf.open(fileName.c_str(), std::ios_base::in |
      std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);

    fbuf.pubseekoff(nrow * ncol * sizeof(T) - 1, std::ios_base::beg);

    fbuf.sputc(0);

    fbuf.close();

  } catch(std::exception& ex) {
    throw std::runtime_error("Problem creating the backing file.");
  }

}

// [[Rcpp::export]]
void createFile(std::string fileName,
                std::size_t nrow,
                std::size_t ncol,
                std::string type) {

  if (type == "double")
    createFile<double>(fileName, nrow, ncol);
}
