# envi (development version)

# envi v0.1.13
* In the `div_plot()` and `seq_plot()` internal functions, replaced `if (class(input) == "im")` with `if (methods::is(input, "im"))`
* Added `methods` package to Imports
* Re-added `utils` package to Suggests because "zzz.R" calls the `packageDescription()` function
* Moved `spatstat.random` package from Imports to Suggests because the `rpoispp()` function is only used for tests

# envi v0.1.12
* Fixed bug in `perlrren()` function that will now keep the predicted values even if NA
* Fixed bug in `plot_perturb()` function that will now project rasters using `method = "bilinear"` for continuous values
* Streamlined "test-perlrren.R" by removing duplicate tests for "incorrectly specified n_sim"
* Fixed bug in "test-plot_cv.R" by resetting the graphics within an error check to clear a warning between tests

# envi v0.1.11
* Updated dependencies `spatstat.core` and `spatstat.linnet` packages based on feedback from the Spatstat Team (Adrian Baddeley and Ege Rubak). All random generators in `spatstat.core` were moved to a new package `spatstat.random`
  * `spatstat.geom`, `spatstat.core`, `spatstat.linnet`, and `spatstat (>=2.0-0)` are no longer Depends.
  * `spatstat.geom` and `spatstat.random` are now Imports
  * `spatstat.data` is now Suggests.
  * [See the GitHub merge pull request](https://github.com/Waller-SUSAN/envi/pull/2/commits/34b67d8a66151f609cafe4a72de39e382abe7f07).
* `maptools` is no longer Imports
* Fixed annotation typos in the `perlrren()`, `pval_correct()`, `lrren()` functions
* Fixed bug in `plot_cv()` function that will specify the size of the confidence interval in the subtitle based on the chosen alpha level

# envi v0.1.10
* Addressed ERROR on R-devel CRAN environments by setting `parallelly.makeNodePSOCK.setup_strategy = sequential` for all CRAN tests as suggested by the maintainer for the `future` and `parallelly` packages who is actively working on a solution. [See the GitHub issue](https://github.com/HenrikBengtsson/parallelly/issues/65).

# envi v0.1.9
* Removed `LazyData: true` from 'DESCRIPTION' file because the package has no data accessed via a `data()` command and has no `data/` directory (in response to CRAN NOTE: 'LazyData' is specified without a 'data' directory)
* Following advice from `future` package maintainer, now `spatial_power()` and `jitter_power()` functions reset future strategy when exiting
* Addressed ERROR in MacOS environments on rhub by setting the `parallelly.makeNodePSOCK.setup_strategy = sequential` for MacOS environments running `tcltk` until `parallelly` (>=1.26.1-9002) is on CRAN. This workaround was suggested by the `parallelly` maintainer. [See the GitHub issue](https://github.com/HenrikBengtsson/parallelly/issues/62#issuecomment-880665390).

# envi v0.1.8
* Updated `spatstat` package to new subsetted packages based on feedback from the Spatstat Team (Adrian Baddeley and Ege Rubak). Now `spatstat.geom`, `spatstat.core`, `spatstat.linnet`, and `spatstat (>= 2.0-0)` are in Depends
* Replaced `parallel` and `doParallel` packages in Imports with `doFuture`, `doRNG`, and `future` packages to allow for parallel processing in `lrren()` and `perlrren()` functions to work across all CRAN environments
* Removed `utils` package from Imports because the progress bar in `lrren()` and `perlrren()` functions is now produced with a helper function in utils.R that imports the `iterators` package
* Set default for `n_core` argument in `lrren()` and `perlrren()` to be `n_core = 2` to match documentation
* Fixed bug in `pval_correct()` when `p_correct = "FDR"` that will return the minimum p-value instead of NULL if no p-value is less than the p-critical value
* Updated `cref0` and `cref1` arguments in `plot_predict()` and `plot_perturb()` functions for PROJ6. Now calls `sp::CRS()` function within the `raster::projectRaster()` function and reformats the default argument values
* Updated testthat checks for parallelization in `lrren()` and `perlrren()` functions and testthat checks for PROJ6 updates in `plot_predict()` and `plot_perturb()` functions

# envi v0.1.7
* Updated `spatstat` package to new subsetted packages based on feedback from the Spatstat Team (Adrian Baddeley and Ege Rubak). `spatstat.geom` and `spatstat.core` packages replace `spatstat` package in Imports
* Added `p_correct` argument to `lrren()` and `perlrren()` which calls a new, internal function `pval_correct()` that calculates three types of corrections for multiple testing (FDR, Sidak, Bonferroni)
* Removed a cv output for pval surface in `lrren()` that is not used in `plot_cv()`

# envi v0.1.6
* Updated URLs in envi-package.Rd

# envi v0.1.5
* Updated URLs in envi-package.Rd
* Updated year in DESCRIPTION

# envi v0.1.4
* Fixed testthat preparation for `plot_cv()`, `plot_obs()`, and `plot_predict()` to remove `"Warning message: In graphics::par(op): calling par(new=TRUE) with no plot"`
* Removed parallel tests in testthat for `lrren()` and `perlrren()` as there is a known error with makePSOCKcluster() in R version > 4.0.0 (created error only in r-patched-solaris-x86 and r-release-macos-x86_64).
  