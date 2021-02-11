# envi (development version)

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
  