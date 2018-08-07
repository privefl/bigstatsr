/******************************************************************************/

#include <fstream>
#include <stdexcept>
#include <bigstatsr/types.h>

#define ERROR_POS "Dimensions should be at least 1."

/******************************************************************************/

template <typename T>
void createFile(std::string fileName,
                std::size_t nrow,
                std::size_t ncol) {

  try {
    std::filebuf fbuf;
    fbuf.open(fileName.c_str(), std::ios_base::out | std::ios_base::binary);
    fbuf.pubseekpos(nrow * ncol * sizeof(T) - 1); fbuf.sputc(0);
    fbuf.close();
  } catch(std::exception& ex) {
    throw std::runtime_error("Problem creating the backing file.");
  }

}

#define CREATE_FILE(TYPE) return createFile<TYPE>(fileName, nrow, ncol);

// [[Rcpp::export]]
void createFile(std::string fileName,
                std::size_t nrow,
                std::size_t ncol,
                int type) {

  myassert(nrow > 0, ERROR_POS);
  myassert(ncol > 0, ERROR_POS);

  DISPATCH_TYPE(CREATE_FILE)
}

/******************************************************************************/

template <typename T>
void addColumns(std::string fileName,
                std::size_t nrow,
                std::size_t ncol_add) {

  try {
    std::fstream filestr(fileName.c_str());
    if (filestr) {
      std::streambuf* pbuf = filestr.rdbuf();
      pbuf->pubseekoff(nrow * ncol_add * sizeof(T) - 1, filestr.end);
      pbuf->sputc(0);
      filestr.close();
    }

  } catch(std::exception& ex) {
    throw std::runtime_error("Problem resizing the backing file.");
  }

}

#define ADD_COLUMNS(TYPE) return addColumns<TYPE>(fileName, nrow, ncol_add);

// [[Rcpp::export]]
void addColumns(std::string fileName,
                std::size_t nrow,
                std::size_t ncol_add,
                int type) {

  myassert(nrow > 0, ERROR_POS);
  myassert(ncol_add > 0, ERROR_POS);

  DISPATCH_TYPE(ADD_COLUMNS)
}

/******************************************************************************/
