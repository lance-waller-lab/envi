## This is the tenth resubmission

* Updates since previous submission:
  * Addressed ERROR on R-devel CRAN environments by setting `parallelly.makeNodePSOCK.setup_strategy = sequential` for all CRAN tests as suggested by the maintainer for the `future` and `parallelly` packages who is actively working on a solution <https://github.com/HenrikBengtsson/parallelly/issues/65>
  
* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL

## Test environments
* local OS X install, R 4.1.0
* win-builder, (devel, oldrelease, release)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Windows Server 2008 R2 SP1, R-devel, 32⁄64 bit
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer
