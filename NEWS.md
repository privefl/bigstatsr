## bigstatsr 0.8.4

Add `%*%`, `crossprod` and `tcrossprod` operations for 'double' FBMs.

## bigstatsr 0.8.3

Now also returns the number of non-zero variables (`$nb_active`) and the number of candidate variables (`$nb_candidate`) for each step of the regularization paths of `big_spLinReg()` and `big_spLogReg()`.

## bigstatsr 0.8.0

- Parameters `warn` and `return.all` of `big_spLinReg()` and `big_spLogReg()` are deprecated; now always return the maximum information. Now provide two methods (`summary` and `plot`) to get a quick assessment of the fitted models.

## bigstatsr 0.7.3

- Check of missing values for input vectors (indices and targets) and matrices (covariables).

- `AUC()` is now stricter: it accepts only 0s and 1s for `target`.

## bigstatsr 0.7.1

- `$bm()` and `$bm.desc()` have been added in order to get an `FBM` as a `filebacked.big.matrix`. This enables using {bigmemory} functions.

## bigstatsr 0.7.0

- Type `float` added.

## bigstatsr 0.6.2

- `big_write` added.

## bigstatsr 0.6.1

- `big_read` now has a `filter` argument to filter rows, and argument `nrow` has been removed because it is now determined when reading the first block of data.

- Removed the `save` argument from `FBM` (and others); now, you must use `FBM(...)$save()` instead of `FBM(..., save = TRUE)`.

## bigstatsr 0.6.0

- You can now fill an FBM using a data frame. Note that factors will be used as integers.

- [Package {bigreadr}](https://github.com/privefl/bigreadr) has been developed and is now used by `big_read`.

## bigstatsr 0.5.0

- There have been some changes regarding how conversion between types is checked. Before, you would get a warning for any possible loss of precision (without actually checking it). Now, any loss of precision due to conversion between types is reported as a warning, and only in this case. If you want to disable this feature, you can use `options(bigstatsr.downcast.warning = FALSE)`, or you can use `without_downcast_warning()` to disable this warning for one call.

## bigstatsr 0.4.1

- change `big_read` so that it is faster (corresponding vignette updated).

## bigstatsr 0.4.0

- possibility to add a "base predictor" for `big_spLinReg` and `big_spLogReg`.

- **don't store the whole regularization path (as a sparse matrix) in `big_spLinReg` and `big_spLogReg` anymore because it caused major slowdowns.**

- directly average the K predictions in `predict.big_sp_best_list`.

- only use the "PSOCK" type of cluster because "FORK" can leave zombies behind. You can change this with `options(bigstatsr.cluster.type = "PSOCK")`.

## bigstatsr 0.3.4

- Fix a bug in `big_spLinReg` related to the computation of summaries.

- Now provides function `plus` to be used as the `combine` argument in `big_apply` and `big_parallelize` instead of `'+'`.

## bigstatsr 0.3.3

- Before, this package used only the "PSOCK" type of cluster, which has some significant overhead. Now, it uses the "FORK" type on non-Windows systems. You can change this with `options(bigstatsr.cluster.type = "PSOCK")`. Uses "PSOCK" in 0.4.0.

## bigstatsr 0.3.2

- you can now provide multiple $\alpha$ values (as a numeric vector) in `big_spLinReg` and `big_spLogReg`. One will be chosen by grid-search.

## bigstatsr 0.3.1

- fixed a bug in `big_prodMat` when using a dimension of 1 or 0.

## bigstatsr 0.3.0

- **Package {bigstatsr} is published in [Bioinformatics](https://doi.org/10.1093/bioinformatics/bty185)**

## bigstatsr 0.2.6

- no scaling is used by default for `big_crossprod`, `big_tcrossprod`, `big_SVD` and `big_randomSVD` (before, there was no default at all)

## bigstatsr 0.2.4

- **Integrate Cross-Model Selection and Averaging (CMSA) directly in `big_spLinReg` and `big_spLogReg`, a procedure that automatically chooses the value of the $\lambda$ hyper-parameter.**

- **Speed up `big_spLinReg` and `big_spLogReg` ([issue #12](https://github.com/privefl/bigstatsr/issues/12))**

## bigstatsr 0.2.3

- Speed up AUC computations

## bigstatsr 0.2.0

- **No longer use the `big.matrix` format of package bigmemory**


