context("plot_perturb")

#########################
# plot_perturb testthat #
#########################

# Generate testing data
## Environmental Covariates
library(envi)
library(raster)
library(spatstat)
library(spatstat.data)
set.seed(1234)

# -------------- #
# Prepare inputs #
# -------------- #

# Using the `bei` and `bei.extra` data from {spatstat.data}

# Scale environmental Covariates
ims <- spatstat.data::bei.extra
ims[[1]]$v <- scale(ims[[1]]$v)
ims[[2]]$v <- scale(ims[[2]]$v)

# Presence locations
presence <- spatstat.data::bei
spatstat::marks(presence) <- data.frame("presence" = rep(1, presence$n),
                                             "lon" = presence$x,
                                             "lat" = presence$y)

# (Pseudo-)Absence locations
absence <- spatstat::rpoispp(0.008, win = ims[[1]])
spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                            "lon" = absence$x,
                                            "lat" = absence$y)

# Combine into readable format
obs_locs <- spatstat::superimpose(presence, absence, check = FALSE)
spatstat::marks(obs_locs)$id <- seq(1, obs_locs$n, 1)
spatstat::marks(obs_locs) <- spatstat::marks(obs_locs)[ , c(4, 2, 3, 1)]

# Specify categories for varying degrees of spatial uncertainty
## Creates three groups
spatstat::marks(obs_locs)$levels <- as.factor(stats::rpois(obs_locs$n, lambda = 0.05))

# Run perlrren
test_perlrren <-  perlrren(obs_ppp = obs_locs,
                           covariates = ims,
                           radii = c(10, 100, 500),
                           n_sim = 10)

test_that("plot_perturb throws error with invalid arguments", {
  
  # plot_perturb without perlrren output
  expect_error(
    plot_perturb(input = NULL)
  )
  
  # incorrect length mean_cols
  expect_error(
    plot_perturb(input = test_perlrren,
             mean_cols = c("#8b3a3a", "#cccccc"))
  )
  
  # incorrect length var_cols
  expect_error(
    plot_perturb(input = test_perlrren,
             var_cols = c("#cccccc"))
  )
  
  # incorrect length cov_labs
  expect_error(
    plot_perturb(input = test_perlrren,
             cov_labs = c("V1"))
  )
  
}
) 

test_that("plot_obs works", {
  skip_on_cran()
  expect_silent(
    plot_perturb(input = test_perlrren)
  )
  
  # cref0 = NULL
  expect_silent(
    plot_perturb(input = test_perlrren,
                 cref0 = NULL)
  )
  
  # With spatial transformation
  expect_silent(
    plot_perturb(input = test_perlrren,
                 cref0 = "+init=epsg:5472",
                 cref1 = "+init=epsg:4326")
  )
  
  # Without prediction
  expect_silent(
    plot_perturb(input = test_perlrren,
                 predict = FALSE)
  )
}
)