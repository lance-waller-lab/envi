# envi: Environmental Interpolation using Spatial Kernel Density Estimation 
<img src='man/figures/envi.png' width='120' align='right' />

<!-- badges: start -->
[![R-CMD-check](https://github.com/lance-waller-lab/envi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lance-waller-lab/envi/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/envi)](https://cran.r-project.org/package=envi)
[![CRAN version](https://www.r-pkg.org/badges/version-ago/envi)](https://cran.r-project.org/package=envi)
[![CRAN RStudio mirror downloads total](https://cranlogs.r-pkg.org/badges/grand-total/envi?color=blue)](https://r-pkg.org/pkg/envi)
[![CRAN RStudio mirror downloads monthly ](https://cranlogs.r-pkg.org/badges/envi)](https://www.r-pkg.org:443/pkg/envi)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/license/apache-2-0)
![GitHub last commit](https://img.shields.io/github/last-commit/lance-waller-lab/envi)
[![](https://img.shields.io/badge/DOI-10.32614/CRAN.package.envi-1f57b6?style=flat&link=https://doi.org/10.32614/CRAN.package.envi)](https://doi.org/10.32614/CRAN.package.envi)
<!-- badges: end -->

**Date repository last updated**: August 29, 2025

<h2 id='overview'>

Overview

</h2>

The `envi` package is a suite of `R` functions to estimate the ecological niche of a species and predict the spatial distribution of the ecological niche -- a version of environmental interpolation -- with spatial kernel density estimation techniques. A two-group comparison (e.g., presence and absence locations of a single species) is conducted using the spatial relative risk function that is estimated using the [sparr](https://CRAN.R-project.org/package=sparr) package. Internal cross-validation and basic visualization are also supported. 

<h2 id='install'>

Installation

</h2>

To install the release version from CRAN:

    install.packages('envi')

To install the development version from GitHub:

    devtools::install_github('lance-waller-lab/envi')

<h2 id='available-functions'>

Available functions

</h2>

<table>
<colgroup>
<col width='30%'/>
<col width='70%'/>
</colgroup>
<thead>
<tr class='header'>
<th>Function</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<td><a href='R/lrren.R'><code>lrren</code></a></td>
<td>Main function. Estimate an ecological niche using the spatial relative risk function and predict its location in geographic space.</td>
</tr>
<td><a href='R/perlrren.R'><code>perlrren</code></a></td>
<td>Sensitivity analysis for <a href='R/lrren.R'><code>lrren</code></a> whereby observation locations are spatially perturbed ('jittered') with specified radii, iteratively.</td>
</tr>
<td><a href='R/plot_obs.R'><code>plot_obs</code></a></td>
<td>Display multiple plots of the estimated ecological niche from <a href='R/lrren.R'><code>lrren</code></a> output.</td>
</tr>
<td><a href='R/plot_predict.R'><code>plot_predict</code></a></td>
<td>Display multiple plots of the predicted spatial distribution from <a href='R/lrren.R'><code>lrren</code></a> output.</td>
</tr>
<td><a href='R/plot_cv.R'><code>plot_cv</code></a></td>
<td>Display multiple plots of internal k-fold cross-validation diagnostics from <a href='R/lrren.R'><code>lrren</code></a> output.</td>
</tr>
<td><a href='R/plot_perturb.R'><code>plot_perturb</code></a></td>
<td>Display multiple plots of output from <a href='R/perlrren.R'><code>perlrren</code></a> including predicted spatial distribution of the summary statistics.</td>
</tr>
<td><a href='R/div_plot.R'><code>div_plot</code></a></td>
<td>Called within <a href='R/plot_obs.R'><code>plot_obs</code></a>, <a href='R/plot_predict.R'><code>plot_predict</code></a>, and <a href='R/plot_perturb.R'><code>plot_perturb</code></a>, provides functionality for basic visualization of surfaces with diverging color palettes.</td>
</tr>
<td><a href='R/seq_plot.R'><code>seq_plot</code></a></td>
<td>Called within <a href='R/plot_perturb.R'><code>plot_perturb</code></a>, provides functionality for basic visualization of surfaces with sequential color palettes.</td>
</tr>
<td><a href='R/pval_correct.R'><code>pval_correct</code></a></td>
<td>Called within <a href='R/lrren.R'><code>lrren</code></a> and <a href='R/perlrren.R'><code>perlrren</code></a>, calculates various multiple testing corrections for the alpha level.</td>
</tr>
</tbody>
</table>

<h2 id='authors'>

Authors

</h2>

* **Ian D. Buller** - *DLH, LLC (formerly Social & Scientific Systems, Inc.), Bethesda, Maryland (current)* - *Occupational and Environmental Epidemiology Branch, Division of Cancer Epidemiology and Genetics, National Cancer Institute, National Institutes of Health, Rockville, Maryland (former)* - *Environmental Health Sciences, James T. Laney School of Graduate Studies, Emory University, Atlanta, Georgia. (original)* - [GitHub](https://github.com/idblr) - [ORCID](https://orcid.org/0000-0001-9477-8582)

See also the list of [contributors](https://github.com/lance-waller-lab/envi/graphs/contributors) who participated in this package, including:

* **Lance A. Waller** - *Biostatistics and Bioinformatics, Emory University, Atlanta, Georgia.* - [GitHub](https://github.com/lance-waller) - [ORCID](https://orcid.org/0000-0001-5002-8886)

## Usage

### For the lrren() function

```r
set.seed(1234) # for reproducibility

# ------------------ #
# Necessary packages #
# ------------------ #

library(envi)
library(spatstat.data)
library(spatstat.random)

# -------------- #
# Prepare inputs #
# -------------- #

# Using the 'bei' and 'bei.extra' data within {spatstat.data}

# Environmental Covariates
elev <- bei.extra[[1]]
grad <- bei.extra[[2]]
elev$v <- scale(elev)
grad$v <- scale(grad)
elev_raster <- rast(elev)
grad_raster <- rast(grad)

# Presence data
presence <- bei
marks(presence) <- data.frame(
  'presence' = rep(1, presence$n),
  'lon' = presence$x,
  'lat' = presence$y
)
marks(presence)$elev <- elev[presence]
marks(presence)$grad <- grad[presence]

# (Pseudo-)Absence data
absence <- rpoispp(0.008, win = elev)
marks(absence) <- data.frame(
  'presence' = rep(0, absence$n),
  'lon' = absence$x,
  'lat' = absence$y
)
marks(absence)$elev <- elev[absence]
marks(absence)$grad <- grad[absence]

# Combine
obs_locs <- superimpose(presence, absence, check = FALSE)
obs_locs <- marks(obs_locs)
obs_locs$id <- seq(1, nrow(obs_locs), 1)
obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]

# Prediction Data
predict_xy <- crds(elev_raster)
predict_locs <- as.data.frame(predict_xy)
predict_locs$elev <- extract(elev_raster, predict_xy)[ , 1]
predict_locs$grad <- extract(grad_raster, predict_xy)[ , 1]

# ----------- #
# Run lrren() #
# ----------- #

test1 <- lrren(
  obs_locs = obs_locs,
  predict_locs = predict_locs,
  predict = TRUE,
  verbose = TRUE,
  cv = TRUE
)
              
# -------------- #
# Run plot_obs() #
# -------------- #

plot_obs(test1)

# ------------------ #
# Run plot_predict() #
# ------------------ #

plot_predict(
  test1,
  cref0 = 'EPSG:5472',
  cref1 = 'EPSG:4326'
)

# ------------- #
# Run plot_cv() #
# ------------- #

plot_cv(test1)
```

![](man/figures/plot_obs1.png)
![](man/figures/plot_obs2.png)
![](man/figures/plot_obs3.png)
![](man/figures/plot_predict1.png)
![](man/figures/plot_predict2.png)
![](man/figures/plot_cv1.png)

```r 
# -------------------------------------- #
# Run lrren() with Bonferroni correction #
# -------------------------------------- #

test2 <- lrren(
  obs_locs = obs_locs,
  predict_locs = predict_locs,
  predict = TRUE,
  p_correct = 'Bonferroni'
)

# Note: Only showing third plot
plot_obs(test2)

# Note: Only showing second plot
plot_predict(
  test2,
  cref0 = 'EPSG:5472',
  cref1 = 'EPSG:4326'
)

# Note: plot_cv() will display the same results because cross-validation only performed for the log relative risk estimate
```

![](man/figures/plot_obs4.png)
![](man/figures/plot_predict3.png)

### For the perlrren() function

```r
set.seed(1234) # for reproducibility

# ------------------ #
# Necessary packages #
# ------------------ #

library(envi)
library(spatstat.data)
library(spatstat.random)

# -------------- #
# Prepare inputs #
# -------------- #

# Using the 'bei' and 'bei.extra' data within {spatstat.data}

# Scale environmental covariates
ims <- bei.extra
ims[[1]]$v <- scale(ims[[1]]$v)
ims[[2]]$v <- scale(ims[[2]]$v)

# Presence data
presence <- bei
marks(presence) <- data.frame(
  'presence' = rep(1, presence$n),
  'lon' = presence$x,
  'lat' = presence$y
)

# (Pseudo-)Absence data
absence <- rpoispp(0.008, win = ims[[1]])
marks(absence) <- data.frame(
  'presence' = rep(0, absence$n),
  'lon' = absence$x,
  'lat' = absence$y
)

# Combine and create 'id' and 'levels' features
obs_locs <- superimpose(presence, absence, check = FALSE)
marks(obs_locs)$id <- seq(1, obs_locs$n, 1)
marks(obs_locs)$levels <- as.factor(rpois(obs_locs$n, lambda = 0.05))
marks(obs_locs) <- marks(obs_locs)[ , c(4, 2, 3, 1, 5)]

# -------------- #
# Run perlrren() #
# -------------- #

# Uncertainty in observation locations
## Most observations within 10 meters
## Some observations within 100 meters
## Few observations within 500 meters

test3 <- perlrren(
  obs_ppp = obs_locs,
  covariates = ims,
  radii = c(10, 100, 500),
  verbose = FALSE, # may not be availabe if parallel = TRUE
  parallel = TRUE,
  n_sim = 100
)
                 
# ------------------ #
# Run plot_perturb() #
# ------------------ #

plot_perturb(
  test3,
  cref0 = 'EPSG:5472',
  cref1 = 'EPSG:4326',
  cov_labs = c('elev', 'grad')
)
```

![](man/figures/plot_perturb1.png)
![](man/figures/plot_perturb2.png)
![](man/figures/plot_perturb3.png)
![](man/figures/plot_perturb4.png)
![](man/figures/plot_perturb5.png)
![](man/figures/plot_perturb6.png)
![](man/figures/plot_perturb7.png)
![](man/figures/plot_perturb8.png)

### Funding

This package was developed while the author was originally a doctoral student in the [Environmental Health Sciences doctoral program](https://sph.emory.edu/degrees-programs/phd/environmental-health-sciences) at [Emory University](https://www.emory.edu/home/index.html) and later as a postdoctoral fellow supported by the [Cancer Prevention Fellowship Program](https://cpfp.cancer.gov/) at the [National Cancer Institute](https://www.cancer.gov/). Any modifications since December 05, 2022 were made while the author was an employee of [DLH, LLC](https://www.dlhcorp.com) (formerly Social & Scientific Systems, Inc.).

### Acknowledgments

When citing this package for publication, please follow:

    citation('envi')

### Questions? Feedback?

For questions about the package, please contact the maintainer [Dr. Ian D. Buller](mailto:ian.buller@alumni.emory.edu) or [submit a new issue](https://github.com/lance-waller-lab/envi/issues).
