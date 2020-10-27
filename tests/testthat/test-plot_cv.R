context("plot_cv")

####################
# plot_cv testthat #
####################

# Generate testing data
## Environmental Covariates
elev <- spatstat.data::bei.extra$elev
grad <- spatstat.data::bei.extra$grad
elev$v <- scale(elev)
grad$v <- scale(grad)
elev_raster <- raster::raster(elev)
grad_raster <- raster::raster(grad)

## Presence Locations
presence <- spatstat.data::bei
spatstat::marks(presence) <- data.frame("presence" = rep(1, presence$n),
                                   "lon" = presence$x,
                                   "lat" = presence$y)
spatstat::marks(presence)$elev <- elev[presence]
spatstat::marks(presence)$grad <- grad[presence]

# (Pseudo-)Absence Locations
set.seed(1234) # for reproducibility
absence <- spatstat::rpoispp(0.008, win = elev)
spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                       "lon" = absence$x,
                                       "lat" = absence$y)
spatstat::marks(absence)$elev <- elev[absence]
spatstat::marks(absence)$grad <- grad[absence]

# Combine
obs_locs <- spatstat::superimpose(presence, absence, check = FALSE)
obs_locs <- spatstat::marks(obs_locs)
obs_locs$id <- seq(1, nrow(obs_locs), 1)
obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]

# Prediction Data
predict_locs <- data.frame(raster::rasterToPoints(elev_raster))
predict_locs$layer2 <- raster::extract(grad_raster, predict_locs[, 1:2])

# Run lrren
test_lrren <- envi::lrren(obs_locs = obs_locs,
                          predict = FALSE,
                          predict_locs = NULL,
                          conserve = TRUE,
                          cv = TRUE,
                          kfold = 10,
                          balance = TRUE,
                          parallel = FALSE,
                          n_core = NULL,
                          poly_buffer = NULL,
                          obs_window = NULL,
                          verbose = FALSE)

test_lrren1 <- envi::lrren(obs_locs = obs_locs,
                          predict = FALSE,
                          predict_locs = NULL,
                          conserve = TRUE,
                          cv = FALSE,
                          kfold = 10,
                          balance = TRUE,
                          parallel = FALSE,
                          n_core = NULL,
                          poly_buffer = NULL,
                          obs_window = NULL,
                          verbose = FALSE)

test_lrren2 <- envi::lrren(obs_locs = obs_locs,
                           predict = FALSE,
                           predict_locs = NULL,
                           conserve = TRUE,
                           cv = TRUE,
                           kfold = 10,
                           balance = FALSE,
                           parallel = FALSE,
                           n_core = NULL,
                           poly_buffer = NULL,
                           obs_window = NULL,
                           verbose = FALSE)


test_that("plot_cv throws error with invalid arguments", {
  
  # plot_cv without lrren output
  expect_error(
    plot_cv(input = NULL, 
             alpha = 0.05)
    )
  
  # plot_cv with lrren output where cv = FALSE
  expect_error(
    plot_cv(input = test_lrren1, 
            alpha = 0.05)
  )
  
  # incorrect alpha
  expect_error(
    plot_cv(input = test_lrren, 
            alpha = 0)
  )
}
) 

test_that("plot_cv works", {
  skip_on_cran()
  
  # Balanced sampling
  expect_silent(
    plot_cv(input = test_lrren, 
            alpha = 0.05)
  )
  
  # Unbalanced sampling
  expect_silent(
    plot_cv(input = test_lrren2, 
            alpha = 0.05)
  )
}
)
