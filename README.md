[![Travis-CI Build Status](https://travis-ci.org/privefl/bigstatsr.svg?branch=master)](https://travis-ci.org/privefl/bigstatsr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/privefl/bigstatsr?branch=master&svg=true)](https://ci.appveyor.com/project/privefl/bigstatsr)
[![Coverage Status](https://img.shields.io/codecov/c/github/privefl/bigstatsr/master.svg)](https://codecov.io/github/privefl/bigstatsr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bigstatsr)](https://cran.r-project.org/package=bigstatsr)
[![CRAN_downloads](http://cranlogs.r-pkg.org/badges/grand-total/bigstatsr)](https://cran.r-project.org/package=bigstatsr)


# bigstatsr

The R package **bigstatsr** provides functions for fast statistical analysis of large-scale data encoded as matrices. The package can handle matrices that are too large to fit in memory thanks to memory-mapping to binary files on disk. This is very similar to the format `big.matrix` provided by the R package [**bigmemory**](https://github.com/kaneplusplus/bigmemory), which is **no longer used** by this package.

[Introduction to package **bigstatsr**](https://goo.gl/k3A5hb)

[**LIST OF FEATURES**](https://privefl.github.io/bigstatsr/reference/index.html)

__Note that most of the algorithms of this package don't handle missing values.__


## Installation


```r
# For the CRAN version
install.packages("bigstatsr")

# For the current development version
devtools::install_github("privefl/bigstatsr")

# For the first version (depending on package bigmemory)
devtools::install_github("privefl/bigstatsr", ref = "v-bigmemory")
```

## Input format

As inputs, package **bigstatsr** uses [Filebacked Big Matrices (FBM)](https://privefl.github.io/bigstatsr/reference/FBM-class.html). 

## Bug report

Please open an issue if you find a bug.
If you want help using **bigstatsr**, please post on Stack Overflow with the tag *bigstatsr* (not yet created). [How to make a great R reproducible example?](https://stackoverflow.com/q/5963269/6103040)


## Code of conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/privefl/bigstatsr/blob/master/code_of_conduct.md). 
By participating in this project you agree to abide by its terms.
