## bigstatsr 0.3.2

- you can now provide multiple $\alpha$ values (as a numeric vector) in `big_spLinReg` and `big_spLogReg`. One will be choosed by grid-search.

## bigstatsr 0.3.1

- fixed a bug in `big_prodMat` when using a dimension of 1 or 0.

## bigstatsr 0.3.0

- **Package {bigstatsr} is published in [Bioinformatics](https://doi.org/10.1093/bioinformatics/bty185)**

## bigstatsr 0.2.6

- no scaling is used by default for `big_crossprod`, `big_tcrossprod`, `big_SVD` and `big_randomSVD` (before, there was no default at all)

## bigstatsr 0.2.4

- **Integrate Cross-Model Selection and Averagind (CMSA) directly in `big_spLinReg` and `big_spLogReg`, a procedure that automatically chooses the value of the $\lambda$ hyper-parameter.**

- **Speed up `big_spLinReg` and `big_spLogReg` ([issue #12](https://github.com/privefl/bigstatsr/issues/12))**

## bigstatsr 0.2.3

- Speed up AUC computations

## bigstatsr 0.2.0

- **No longer use the `big.matrix` format of package bigmemory**


