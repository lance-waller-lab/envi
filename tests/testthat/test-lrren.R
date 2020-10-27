context("lrren")

##################
# lrren testthat #
##################

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

# Test custom window
custom_chull <- grDevices::chull(x = obs_locs[ , 5], y = obs_locs[ , 6])
custom_chull_pts <- obs_locs[c(custom_chull, custom_chull[1]), 5:6]
custom_chull_pts <- rbind(custom_chull_pts, custom_chull_pts[1, ])
custom_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(custom_chull_pts)), 1)))
#add small buffer around polygon to include boundary points
custom_poly <- custom_chull_poly@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon
custom_owin <- spatstat::owin(poly = list(x = rev(custom_poly[ , 1]),
                                               y = rev(custom_poly[ , 2])))



test_that("lrren throws error with invalid arguments", {

  # Predict without predict_locs
  expect_error(
    lrren(obs_locs = obs_locs,
          predict = TRUE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = FALSE)
  )

  # Fewer than 1 fold
  expect_error(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = TRUE,
          kfold = 0,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = FALSE)
  )

  # poly_buffer not of class 'owin'
  expect_error(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = custom_poly,
          verbose = FALSE)
  )
  
  # If conserve = FALSE, predict_locs must be specified
  expect_error(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = FALSE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = custom_poly,
          verbose = FALSE)
  )
}
)

test_that("lrren throws warning for points lying outside the specified window", {

  # Only estimates ecological niche
  ## No poly_buffer
  expect_warning(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = 0,
          obs_window = NULL,
          verbose = FALSE)
  )
}
)

test_that("lrren produces progress messages", {
  expect_message(
    lrren(obs_locs = obs_locs,
          predict = TRUE,
          predict_locs = predict_locs,
          conserve = TRUE,
          cv = TRUE,
          kfold = 10,
          balance = TRUE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = TRUE)
  )
}
)

test_that("lrren works", {

  # Only estimates ecological niche
  ## Conserved estimation
  expect_named(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = FALSE)
  )

  # Only estimates ecological niche
  ## Unconserved estimation
  expect_named(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = predict_locs,
          conserve = FALSE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = FALSE)
  )

  # Only estimates ecological niche
  ## Large poly_buffer
  expect_named(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = predict_locs,
          conserve = FALSE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = 1,
          obs_window = NULL,
          verbose = FALSE)
  )

  # Only estimates ecological niche
  ## Custom obs_window
  expect_named(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = custom_owin,
          verbose = FALSE)
  )

  # Estimate and cross-validation
  ## Unbalanced sampling
  ## Not parallel
  expect_named(
    lrren(obs_locs = obs_locs,
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
  )

  # Estimate and cross-validation
  ## Balanced sampling
  ## Not parallel
  expect_named(
    lrren(obs_locs = obs_locs,
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
  )

  # Estimate and cross-validation
  ## Unbalanced sampling
  ## Parallel (n = 2 cores)
  expect_named(
    lrren(obs_locs = obs_locs,
          predict = FALSE,
          predict_locs = NULL,
          conserve = TRUE,
          cv = TRUE,
          kfold = 10,
          balance = TRUE,
          parallel = TRUE,
          n_core = 2,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = FALSE)
  )

  # Estimate and predict
  expect_named(
    lrren(obs_locs = obs_locs,
          predict = TRUE,
          predict_locs = predict_locs,
          conserve = TRUE,
          cv = FALSE,
          kfold = 10,
          balance = FALSE,
          parallel = FALSE,
          n_core = NULL,
          poly_buffer = NULL,
          obs_window = NULL,
          verbose = FALSE)
  )

}
)
