## This is the eighteenth resubmission

* Updates since previous submission:
  * Fixed bug in calculation of False Discovery Rate in internal `pval_correct()` function
  * Fixed test for `plot_perturb()` function that was presenting a warning by specifying `cref0`
  * Argument `plot_cols` correctly renamed `cols` in `div_plot()` and `seq_plot()` functions
  
* CRAN Package Check Results 2024-01-22 for 'envi' v0.1.17 in r-patched-linux-x86_64, r-release-macos-arm64, r-release-macos-x86_64:
  * NOTE: Package suggested but not available for checking: 'RStoolbox'
  * SOLUTION: 'RStoolbox' recently updated 2024-01-17 so will remain as SUGGESTS for 'envi' v0.1.18
  
* Documentation for "envi-package.Rd", "pval_correct.Rd", DESCRIPTION, and vignette references the following DOIs, which throws a NOTE but are valid URLs:
  * <https://doi.org/10.1002/sim.4780090616>
  * <https://doi.org/10.1002/sim.4780101112>
  * <https://doi.org/10.1002/sim.7577>
  * <https://doi.org/10.1111/j.2517-6161.1995.tb02031.x>
  * <https://doi.org/10.1111/j.1475-4991.2008.00309.x>

## Test environments
* local Windows install, R 4.2.1
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
