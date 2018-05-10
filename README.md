[![Travis-CI Build Status](https://travis-ci.org/privefl/bigstatsr.svg?branch=master)](https://travis-ci.org/privefl/bigstatsr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/privefl/bigstatsr?branch=master&svg=true)](https://ci.appveyor.com/project/privefl/bigstatsr)
[![Coverage Status](https://img.shields.io/codecov/c/github/privefl/bigstatsr/master.svg)](https://codecov.io/github/privefl/bigstatsr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bigstatsr)](https://cran.r-project.org/package=bigstatsr)
[![DOI](https://zenodo.org/badge/doi/10.1093/bioinformatics/bty185.svg)](http://dx.doi.org/10.1093/bioinformatics/bty185)


# bigstatsr

R package {bigstatsr} provides functions for fast statistical analysis of large-scale data encoded as matrices. The package can handle matrices that are too large to fit in memory thanks to memory-mapping to binary files on disk. This is very similar to the format `big.matrix` provided by [R package {bigmemory}](https://github.com/kaneplusplus/bigmemory), which is **no longer used** by this package (see [the corresponding vignette](https://privefl.github.io/bigstatsr/articles/bigstatsr-and-bigmemory.html)).

<img src="bigstatsr.png" width="130" align="right">

[Introduction to package {bigstatsr}](https://goo.gl/k3A5hb)

[**LIST OF FEATURES**](https://privefl.github.io/bigstatsr/reference/index.html)

**Note that most of the algorithms of this package don't handle missing values.**


## Installation

```r
# For the current development version
devtools::install_github("privefl/bigstatsr")
```

## Input format

As inputs, package {bigstatsr} uses [Filebacked Big Matrices (FBM)](https://privefl.github.io/bigstatsr/reference/FBM-class.html).

To memory-map character text files, see [package {mmapcharr}](https://github.com/privefl/mmapcharr).

## Bug report / Help

Please open an issue if you find a bug.
If you want help using {bigstatsr}, please post on Stack Overflow with the tag *bigstatsr* (not yet created). [How to make a great R reproducible example?](https://stackoverflow.com/q/5963269/6103040)

## Use cases

### Parallelisation

Package {bigstatsr} uses package {foreach} for its parallelization tasks. Learn more on parallelism with {foreach} with [this tuto](https://privefl.github.io/blog/a-guide-to-parallelism-in-r/).

- [Permute matrix columns in parallel](https://stackoverflow.com/q/48832010/6103040)

- [Parallelized search until found](https://stackoverflow.com/q/49056271/6103040)

### Large datasets

- [Computing the null space of a bigmatrix](https://stackoverflow.com/questions/46253537/computing-the-null-space-of-a-bigmatrix-in-r/) (works if one dimension is not too large)

- [Rowwise matrix multiplication](https://stackoverflow.com/q/48879643/6103040)

- [Operating with a big.matrix](https://stackoverflow.com/q/42111876/6103040)
