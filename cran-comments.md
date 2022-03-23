## This is the twelfth resubmission

* Updates since previous submission:
  * Fixed bug in `perlrren()` function that will now keep the predicted values even if NA
  * Fixed bug in `plot_perturb()` function that will now project rasters using `method = "bilinear"` for continuous values
  * Streamlined "test-perlrren.R" by removing duplicate tests for "incorrectly specified n_sim"
  * Fixed bug in "test-plot_cv.R" by resetting the graphics within an error check to clear a warning between tests
  
* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL

## Test environments
* local OS X install, R 4.1.2
* win-builder, (devel, release, oldrelease)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Windows Server 2022, R-devel, 64 bit
  * Windows Server 2008 R2 SP1, R-release, 32⁄64 bit
  * Oracle Solaris 10, x86, 32 bit, R-release
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer
