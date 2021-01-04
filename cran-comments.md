## This is the fourth resubmission

* Actions taken regarding CRAN Package Check Results:
  * Fixed testthat preparation for plot_cv(), plot_obs(), and plot_predict() to remove "Warning message: In graphics::par(op): calling par(new=TRUE) with no plot"
  * Removed parallel tests in testthat for lrren() and perlrren() as there is a known error with makePSOCKcluster() in R version > 4.0.0 (created error only in r-patched-solaris-x86 and r-release-macos-x86_64).

## Test environments
* local OS X install, R 3.6.3
* win-builder, (devel, oldrelease, release)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Windows Server 2008 R2 SP1, R-devel, 32⁄64 bit
  * macOS 10.13.6 High Sierra, R-release, brew
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup
  * Oracle Solaris 10, x86, 32 bit, R-release
  * Oracle Solaris 10, x86, 32 bit, R-release, Oracle Developer Studio 12.6 

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer
