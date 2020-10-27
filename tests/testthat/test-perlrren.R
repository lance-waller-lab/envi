context("perlrren")

#####################
# perlrren testthat #
#####################

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

obs_locs1 <- obs_locs

# Specify categories for varying degrees of spatial uncertainty
## Creates three groups
spatstat::marks(obs_locs)$levels <- as.factor(stats::rpois(obs_locs$n, lambda = 0.05))

# Incorrect inputs
obs_locs2 <- obs_locs
spatstat::marks(obs_locs2)$levels <- NULL

test_that("perlrren throws error with invalid arguments", {
  
  # Incorrectly specified level
  expect_error(
    perlrren(obs_ppp = obs_locs1,
             covariates = ims,
             radii = c(10, 100, 500),
             alpha = 0,
             n_sim = 10)
  )
  
  expect_error(
    perlrren(obs_ppp = obs_locs2,
             covariates = ims,
             radii = c(10, 100, 500),
             alpha = 0,
             n_sim = 10)
  )
  
  # Incorrect length of radii
  expect_error(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 500),
             n_sim = 10)
  )
  
  # A radius of 0
  expect_error(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(0, 100, 500),
             n_sim = 10)
  )
  
  # Incorrect length of ims
  expect_error(
    perlrren(obs_ppp = obs_locs,
             covariates = ims[[1]],
             radii = c(10, 100, 500),
             n_sim = 10)
  )
  
  # Incorrectly specified n_sim
  expect_error(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             n_sim = 0)
  )
  
  # Incorrectly specified n_sim
  expect_error(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             n_sim = 0)
  )
  
  # Incorrectly specified alpha
  expect_error(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             alpha = 0,
             n_sim = 10)
  )
  
}
)

test_that("perlrren produces progress messages", {
  expect_message(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             n_sim = 10, 
             verbose = TRUE)
  )
}
)

test_that("perlrren works", {
  
  # Prediction TRUE
  expect_named(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             n_sim = 10)
  )
  
  # Prediction FALSE
  expect_named(
    perlrren(obs_ppp = obs_locs,
             predict = FALSE,
             covariates = ims,
             radii = c(10, 100, 500),
             n_sim = 10)
  )
  
  # Alpha small
  expect_named(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             alpha = 0.01,
             n_sim = 10)
  )
  
  # Parallel
  expect_named(
    perlrren(obs_ppp = obs_locs,
             covariates = ims,
             radii = c(10, 100, 500),
             parallel = TRUE,
             n_core = 2,
             n_sim = 10)
  )
  
}
)
