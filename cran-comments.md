## This is the fourteenth resubmission

* Updates since previous submission:
  * Updated package URL and BugReports to renamed GitHub account "lance-waller-lab" (previously "Waller-SUSAN")
  * Replaced `methods::is()` with `inherits()` and `methods` is no longer Imports
  * Added `maptools` and `RStoolbox` to Suggests (used in the package vignette)
  * Added CITATION file
  * Fixed typos in documentation throughout
  
* Documentation for "man/envi-package", man/pval_correct.Rd", and DESCRIPTION references the following DOIs, which throws a NOTE but are valid URLs:
  * <https://doi.org/10.1002/sim.4780090616>
  * <https://doi.org/10.1002/sim.4780101112>
  * <https://doi.org/10.1002/sim.7577>
  * <https://doi.org/10.1111/j.2517-6161.1995.tb02031.x>

## Test environments
* local OS X install, R 4.2.1
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
