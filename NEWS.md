## bigstatsr 1.5.9

- Add global option `FBM.dir` (that defaults to `tempdir()` as before). This can be used to change the default directory used to create FBMs when calling either `FBM()`, `FBM.code256()`, `as_FBM()`, `big_copy()`, or `big_transpose()`. Note that, if not using the temporary directory anymore, you must clean up the files you do not want to keep.

## bigstatsr 1.5.8

- Enable `ARMA_64BIT_WORD`.

## bigstatsr 1.5.7

- New strategy for `$add_columns()`.

## bigstatsr 1.5.6

- Add convenience function `as_scaling_fun()` to create your own `fun.scaling` parameters.

## bigstatsr 1.5.4

- Now automatically discard covariates with no variation in `pcor()` (with a warning).

## bigstatsr 1.5.3

- `pcor()` now returns NAs (instead of 0s) for singular systems.

## bigstatsr 1.5.0

- Recode some parallel algorithms with OpenMP. For now, functions `big_prodVec()`, `big_cprodVec()`, `big_colstats()` and `big_univLinReg()` have been recoded.

## bigstatsr 1.4.0

- Now detects and errors if there is not enough disk space to create an FBM.

## bigstatsr 1.3.3

- Fix `pcor()` for singular systems, e.g. when `x` has all the same values.

## bigstatsr 1.3.2

- Fix `summary()` and `plot()` for old (< v1.3) `big_sp_list` models.

## bigstatsr 1.3.1

- Add function `pcor()` to compute partial correlations.

## bigstatsr 1.3.0

- Add two options in `big_spLinReg()` and `big_spLogReg()`; `power_scale` for using a different scaling for LASSO and `power_adaptive` for using adaptive LASSO (where larger marginal effects are penalized less). See documentation for details.

- `big_(c)prodVec()` and `big_(c)prodMat()` (re)gain a `ncores` parameter. Note that for `big_(c)prodMat()`, it might be beneficial to use the BLAS parallelism (with `bigparallelr::set_blas_ncores()`) instead of this parameter, especially when the matrix `A` is large-ish.

## bigstatsr 1.2.2

- Function `big_colstats()` can now be run in parallel (added parameter `ncores`).

## bigstatsr 1.2.1

- It is now possible to use C++ FBM accessors without linking to {RcppArmadillo}.

## bigstatsr 1.2.0

- Functions `big_(c)prodMat()` and `big_(t)crossprodSelf()` now use much less memory, and may be faster.

- Add `covar_from_df()` to convert a data frame with factors/characters to a numeric matrix using one-hot encoding.

## bigstatsr 1.1.4

- Remove some 'Suggests' dependencies.

## bigstatsr 1.1.3

- Add a new column `$all_conv` to output of `summary()` for `big_spLinReg()` and `big_spLogReg()` to check whether all models have stopped because of "no more improvement". Also add a new parameter `sort` to `summary()`.

- Now `warn` (enabled by default) if some models may not have reached a minimum when using `big_spLinReg()` and `big_spLogReg()`.

## bigstatsr 1.1.1

- Fix `In .self$nrow * .self$ncol : NAs produced by integer overflow`.

## bigstatsr 1.1.0

- Make two different memory-mappings: one that is read-only (using `$address`) and one where it is possible to write (using `$address_rw`). This enables to use file permissions to prevent modifying data.

- Also add a new field `$is_read_only` to be used to prevent modifying data (at least with `<-`) even when you have write permissions to it. Functions creating an FBM now gain a parameter `is_read_only`.

- Make vector accessors (e.g. `X[1:10]`) faster.

## bigstatsr 1.0.0

- Move some code to new packages {bigassertr} and {bigparallelr}.

- `big_randomSVD()` gains arguments related to matrix-vector multiplication.

- `assert_noNA()` is faster.

## bigstatsr 0.9.10

- Add `big_increment()`.

## bigstatsr 0.9.9

In `plot.big_SVD()`,

  - Can now plot many PCA scores (more than two) at once.
  
  - Use `coord_fixed()` when plotting PCA scores because it is good practice.
  
  - Use log-scale in scree plot to better see small differences in singular values.
  
  - Reexport `cowplot::plot_grid()` to merge multiple ggplots.

## bigstatsr 0.9.6

- `AUCBoot()` is now 6-7 times faster.

## bigstatsr 0.9.5

- Add parameters `center` and `scale` to products.

## bigstatsr 0.9.3

- Fix a bug in `big_univLogReg()` for variables with no variation. IRLS was not converging, so `glm()` was used instead. The problem is that `glm()` drops dimensions causing singularities so that Z-score of the first covariate (or intercept) was used instead of a missing value.

## bigstatsr 0.9.0

- Use *mio* instead of *boost* for memory-mapping.

- Add a parameter `base.row` to `predict.big_sp_list()` and automatically detect if needed (as well as for `covar.row`).

- Possibility to subset a `big_sp_list` without losing attributes, so that one can access one model (corresponding to one alpha) even if it is not the 'best'.

- Add parameters `pf.X` and `pf.covar` in `big_sp***Reg()` to provide different penalization for each variable (possibly no penalization at all). 

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


