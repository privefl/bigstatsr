/******************************************************************************/

#include <fstream>
#include <stdexcept>
#include <bigstatsr/types.h>

/******************************************************************************/

template <typename T>
void addColumns(std::string fileName,
                std::size_t nrow,
                std::size_t ncol_add) {

  try {

    std::fstream filestr(fileName.c_str());

    if (filestr) {
      std::streambuf* pbuf = filestr.rdbuf();
      size_t new_size = nrow * ncol_add * sizeof(T) - 1;
      pbuf->pubseekoff(new_size, filestr.end);
      pbuf->sputc(0);

      filestr.close();
    }

  } catch(std::exception& ex) {
    throw std::runtime_error("Problem creating the backing file.");
  }

}

/******************************************************************************/

#define ADD_COLUMNS(TYPE) return addColumns<TYPE>(fileName, nrow, ncol_add);

// [[Rcpp::export]]
void addColumns(std::string fileName,
                std::size_t nrow,
                std::size_t ncol_add,
                int type) {

  DISPATCH_TYPE(ADD_COLUMNS)
}

/******************************************************************************/

/***R
library(bigstatsr)
X <- FBM(10, 10, init = 1)
X[]
file.size(X$backingfile)
addColumns(X$backingfile, 10, 2, 8)
X[]
file.size(X$backingfile)

X2 <- FBM(10, 12, backingfile = sub("\\.bk$", "", X$backingfile),
          create_bk = FALSE)
X2[]
X2[, 1] <- 5
X[, 1]

file.create(tmp <- tempfile())
file.size(tmp)
addColumns(tmp, 20, 5, 8)
file.size(tmp)

addColumns(tmp2 <- tempfile(), 20, 5, 8)
file.size(tmp2)
*/
