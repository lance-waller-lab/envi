## This is the seventh resubmission

* Updates since previous submission:
  * Updated 'spatstat' package to new subsetted packages based on feedback from the Spatstat Team (Adrian Baddeley and Ege Rubak). 'spatstat.geom' and spatstat.core' packages replace 'spatstat' package in Imports
  * Added `p_correct` argument to `lrren()` and `perlrren()` which calls a new, internal function `pval_correct()` that calculates three types of corrections for multiple testing (FDR, Sidak, Bonferroni)
  * Removed a cv output for pval surface in `lrren()` that is not used in `plot_cv()`
  
* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE in win-builder but no other environment

## Test environments
* local OS X install, R 3.6.3
* win-builder, (devel, oldrelease, release)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Windows Server 2008 R2 SP1, R-devel, 32⁄64 bit

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer
