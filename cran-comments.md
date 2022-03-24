## This is the thirteenth resubmission

* Actions taken regarding feedback from CRAN teams' auto-check service:
  * In the `div_plot()` and `seq_plot()` internal functions, replaced `if (class(input) == "im")` with `if (methods::is(input, "im"))`
  * Added `methods` package to Imports
  
* Updates since previous submission:
  * Re-added `utils` package to Suggests because "zzz.R" calls the `packageDescription()` function
  * Moved `spatstat.random` package from Imports to Suggests because the `rpoispp()` function is only used for tests
  
* Documentation for "man/envi-packge", man/pval_correct.Rd", and DESCRIPTION references the following DOIs, which throws a NOTE but are valid URLs:
  * <https://doi.org/10.1002/sim.4780090616>
  * <https://doi.org/10.1002/sim.4780101112>
  * <https://doi.org/10.1002/sim.7577>
  * <https://doi.org/10.1111/j.2517-6161.1995.tb02031.x>

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
