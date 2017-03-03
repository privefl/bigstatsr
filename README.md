[![Travis-CI Build Status](https://travis-ci.org/privefl/bigstatsr.svg?branch=master)](https://travis-ci.org/privefl/bigstatsr)
[![Coverage Status](https://img.shields.io/codecov/c/github/privefl/bigstatsr/master.svg)](https://codecov.io/github/privefl/bigstatsr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bigstatsr)](https://cran.r-project.org/package=bigstatsr)

# bigstatsr

bigstatsr is an R package which provides statistical tools for bigmemory matrices.

## This package is still under dev

## Input format

This package now uses `big.matrix.descriptor` objects as input rather than simply `big.matrix` objects (hereinafter referred to as 'bigmatrices') for several reasons:
- it prevents the user from making common mistakes such as loading all the matrix in memory (e.g. by typing `X[,]`). This package aims at handling matrices that are too large to fit in memory. 
- it prevents the R session from crashing when re-attaching bigmatrices. Indeed, as a `big.matrix` object is an external pointer to a C++ data structure, R can't re-attach it (e.g. when restarting the R session) without any further information. The `big.matrix.descriptor` object provides this information.
- in any case, in order to use parallel computing with bigmatrices, you need to use `big.matrix.descriptor` objects and re-attach the bigmatrices in all sessions.

This means that only shared bigmatrices are handled (you can't describe a non-shared bigmatrix). Using filebacked bigmatrices seems a convenient solution as it uses only disk storage. To convert a shared bigmatrix `X` to a `big.matrix.descriptor` object `X.desc`, you only need to do `X.desc <- describe(X)`. 
