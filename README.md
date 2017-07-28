[![Travis-CI Build Status](https://travis-ci.org/privefl/bigstatsr.svg?branch=master)](https://travis-ci.org/privefl/bigstatsr)
[![Coverage Status](https://img.shields.io/codecov/c/github/privefl/bigstatsr/master.svg)](https://codecov.io/github/privefl/bigstatsr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bigstatsr)](https://cran.r-project.org/package=bigstatsr)


# bigstatsr

The R package **bigstatsr** provides functions for fast statistical analysis of large-scale data encoded as matrices. The package can handle matrices that are too large to fit in memory. The package relies on the format `big.matrix` provided by the R package [**bigmemory**](https://github.com/kaneplusplus/bigmemory) .

The package **bigstatsr** enables users with laptop to perform statistical analysis of several dozens of gigabytes of data. The package is fast and efficient because of four different reasons. First, **bigstatsr** is memory-efficient because it uses only small chunks of data at a time. Second, special care has been taken to implement effective algorithms. Third, `big.matrix` objects use memory-mapping, which provides efficient accesses to matrices. Finally, as matrices are stored on-disk, many processes can easily access them in parallel. 

[Lightning talk at UseR!2017](https://t.co/aYt0q8MeXJ)

[**LIST OF FEATURES**](https://privefl.github.io/bigstatsr/reference/index.html)

__Note that most of the algorithms of this package don't handle missing values.__


## Installation

For now, you can install this package using

```r
devtools::install_github("privefl/bigstatsr")
```


## Input format

As inputs, the package **bigstatsr** can use either `big.matrix.descriptor` objects or simply `big.matrix` objects (hereinafter referred to as 'bigmatrices'). Using filebacked bigmatrices seems a convenient solution as it uses only disk storage. Descriptors may be preferred for several reasons:
- it prevents the user from making common mistakes such as loading all the matrix in memory (e.g. by typing `X[,]` --- we recall that this package aims at handling matrices that are too large to fit in memory). 
- it prevents the R session from crashing when re-attaching bigmatrices. Indeed, as a `big.matrix` object is an external pointer to a C++ data structure, R can't re-attach it (e.g. when restarting the R session) without any further information. The `big.matrix.descriptor` object provides this information.
- in order to use parallel computing with bigmatrices, you need to use `big.matrix.descriptor` objects at a given point in time. 

Moreover, a new class is introduced: a `BM.code`. It is a bigmatrix of type `raw` (one byte unsigned integer) with an embedded lookup table (the slot `code`). This enables you to efficiently store a very large matrix with up to 256 different values. For example, this is used in [package **bigsnpr**](https://privefl.github.io/bigsnpr/reference/bigSNP-class.html) to store genotype matrices.

To facilitate the manipulation of descriptors and `BM.code` objects, some methods have been added/extended:
- `nrow`, `ncol`, `dim` and `length` of a descriptor object access the underlying dimensions of the described bigmatrix (use `typeof` to get the storage mode). 
- `describe` and `attach.BM` are used to switch between descriptors and bigmatrices. Note that, in order to standardize algorithms, describing a descriptor or attaching a bigmatrix simply returns the same object.
- `as.BM.code` to convert a bigmatrix to a `BM.code` (by specifying its lookup table).


## Bug report

Any bug report is welcomed.
If you want help using **bigmemory** or **bigstatsr**, please post on Stack Overflow with the tag *r-bigmemory*.


## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/privefl/bigstatsr/blob/master/code_of_conduct.md). 
By participating in this project you agree to abide by its terms.
