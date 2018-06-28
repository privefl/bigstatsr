## bigstatsr 0.4.0

- possibility to add a "base predictor" for `big_spLinReg` and `big_spLogReg`.

- **don't store the whole regularization path (as a sparse matrix) in `big_spLinReg` and `big_spLogReg` anymore because it caused major slowdowns.**

- directly average the K predictions in `predict.big_sp_best_list`.

- only use the "PSOCK" type of cluster because "FORK" can leave zombies behind. You can change this with `options(bigstatsr.cluster.type = "PSOCK")`.

## bigstatsr 0.3.4

- Fix a bug in `big_spLinReg` related to the computation of summaries.

- Now provides function `plus` to be used as the `combine` argument in `big_apply` and `big_parallelize` instead of `'+'`.

## bigstatsr 0.3.3

- Before, this package used only the "PSOCK" type of cluster, which has some significant overhead. Now, it uses the "FORK" type on non-Windows systems. You can change this with `options(bigstatsr.cluster.type = "PSOCK")`.

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


