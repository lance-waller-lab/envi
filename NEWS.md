# envi (development version)

# envi v0.1.6
  * Updated URLs in envi-package.Rd

# envi v0.1.5
  * Updated URLs in envi-package.Rd
  * Updated year in DESCRIPTION

# envi v0.1.4
  * Fixed testthat preparation for plot_cv(), plot_obs(), and plot_predict() to remove "Warning message: In graphics::par(op): calling par(new=TRUE) with no plot"
  * Removed parallel tests in testthat for lrren() and perlrren() as there is a known error with makePSOCKcluster() in R version > 4.0.0 (created error only in r-patched-solaris-x86 and r-release-macos-x86_64).