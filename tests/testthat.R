library(testthat)
library(envi)

## WORKAROUND: Avoid R bug 18119 [1] that is trigger when for instance the
## 'tcltk' package is loaded on macOS, or when running in the RStudio Console
## [1] https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18119
options(parallelly.makeNodePSOCK.setup_strategy = "sequential")

test_check("envi")
