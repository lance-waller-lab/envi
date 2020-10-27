install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
devtools::has_devel()
library(roxygen2)
library(testthat)

devtools::load_all()
getOption("envi")


# Convert roxygen components to .Rd files
devtools::document()
?lrren

# Create Vignette
install()
build()

# Testing
use_testthat()
use_test()
test()

# NAMESPACE
document()
install()

# Check
check()

# Ignore .R files from /build directory
usethis::use_build_ignore(c("build"))

# rhub
rhub::check_for_cran()

# README.md example
# ------------------ #
# Necessary packages #
# ------------------ #

library(envi)
library(raster)
library(spatstat.core)
library(spatstat.data)

# -------------- #
# Prepare inputs #
# -------------- #

# Using the `bei` and `bei.extra` data from {spatstat.data}

# Environmental Covariates
elev <- spatstat.data::bei.extra$elev
grad <- spatstat.data::bei.extra$grad
elev$v <- scale(elev)
grad$v <- scale(grad)
elev_raster <- raster::raster(elev)
grad_raster <- raster::raster(grad)

# Presence locations
bei <- spatstat.data::bei
spatstat.core::marks(bei) <- data.frame("presence" = rep(1, bei$n),
                                        "lon" = bei$x,
                                        "lat" = bei$y)
spatstat.core::marks(bei)$elev <- elev[bei]
spatstat.core::marks(bei)$grad <- grad[bei]

# (Pseudo-)Absence locations
set.seed(1234) # for reproducibility
absence <- spatstat.core::rpoispp(0.008, win = elev)
spatstat.core::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                            "lon" = absence$x,
                                            "lat" = absence$y)
spatstat.core::marks(absence)$elev <- elev[absence]
spatstat.core::marks(absence)$grad <- grad[absence]

# Combine
obs_locs <- spatstat.core::superimpose(bei, absence, check = FALSE)
obs_locs <- spatstat.core::marks(obs_locs)
obs_locs$id <- seq(1, nrow(obs_locs), 1)
obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]

# Prediction Data
predict_locs <- data.frame(raster::rasterToPoints(elev_raster))
predict_locs$layer2 <- raster::extract(grad_raster, predict_locs[, 1:2])

# ----------- #
# Run lrren() #
# ----------- #

test <- lrren(obs_locs = obs_locs,
              predict_locs = predict_locs,
              predict = TRUE,
              cv = TRUE)

# -------------- #
# Run plot_obs() #
# -------------- #

plot_obs(test)

# ------------------ #
# Run plot_predict() #
# ------------------ #

plot_predict(test, cref0 = "+init=epsg:5472", cref1 = "+init=epsg:4326")

# ------------- #
# Run plot_cv() #
# ------------- #

plot_cv(test)

# Example in VIGNETTE
# Packages
library(spatstat.core)
library(raster)

# Environmental Covariates
slopeangle <- spatstat.data::gorillas.extra$slopeangle
waterdist <- spatstat.data::gorillas.extra$waterdist
slopeangle$v <- scale(slopeangle)
waterdist$v <- scale(waterdist)
slopeangle_raster <- raster(slopeangle)
waterdist_raster <- raster(waterdist)

# Presence
gorillas <- unmark(spatstat.data::gorillas)
spatstat.core::marks(gorillas) <- data.frame("presence" = rep(1, gorillas$n),
                                        "lon" = gorillas$x,
                                        "lat" = gorillas$y)
spatstat.core::marks(gorillas)$slopeangle <- slopeangle[gorillas]
spatstat.core::marks(gorillas)$waterdist <- waterdist[gorillas]

# Absence
set.seed(1234)
absence <- spatstat.core::rpoispp(0.00004, win = slopeangle)
spatstat.core::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                       "lon" = absence$x,
                                       "lat" = absence$y)
spatstat.core::marks(absence)$slopeangle <- slopeangle[absence]
spatstat.core::marks(absence)$waterdist <- waterdist[absence]

# Combine
obs_locs <- spatstat.core::superimpose(gorillas, absence, check = FALSE)
obs_locs <- spatstat.core::marks(obs_locs)
obs_locs$id <- seq(1, nrow(obs_locs), 1)
obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]

# Prediction Data
predict_locs <- data.frame(raster::rasterToPoints(slopeangle_raster))
predict_locs$layer2 <- raster::extract(waterdist_raster, predict_locs[, 1:2])

# Run lrren
test <- lrren(obs_locs = obs_locs,
              predict_locs = predict_locs,
              verbose = T, conserve = T, cv = T,
              parallel = F, balance = F, nfold = 10)

# Plot lrren
plot_obs(test, alpha = 0.05)
plot_predict(test, alpha = 0.05)
plot_cv(test, alpha = 0.05)





## MISCELLANEOUS

elevation <- spatstat.data::gorillas.extra$elevation
slopeangle <- spatstat.data::gorillas.extra$slopeangle
waterdist <- spatstat.data::gorillas.extra$waterdist
elevation$v <- scale(spatstat.data::gorillas.extra$elevation)
slopeangle$v <- scale(spatstat.data::gorillas.extra$slopeangle)
waterdist$v <- scale(spatstat.data::gorillas.extra$waterdist)

raster(spatstat.data::gorillas.extra$aspect)

elevation_raster <- raster(elevation)
slopeangle_raster <- raster(slopeangle)
waterdist_raster <- raster(waterdist)

pca <- RStoolbox::rasterPCA(stack(elevation_raster,slopeangle_raster,waterdist_raster))
summary(pca$model) # PCA components
pca$model$loadings # PCA loadings

pcs <- pca$map
pc1 <- pcs[[1]] # PC1
pc2 <- pcs[[2]] # PC2
pc3 <- pcs[[3]] # PC3

obs_locs <- spatstat.core::marks(gorillas)
obs_locs$lon <- gorillas$x
obs_locs$lat <- gorillas$y
obs_locs$pc1 <- raster::extract(pc1, obs_locs[,4:5])
obs_locs$pc2 <- raster::extract(pc2, obs_locs[,4:5])
obs_locs$pc3 <- raster::extract(pc3, obs_locs[,4:5])
obs_locs$id <- seq(1, nrow(obs_locs), 1)

names(obs_locs)
obs_locs <- obs_locs[,c(9,4,5,1,6,7)]
obs_locs$group <- ifelse(obs_locs$group == "minor", 1, 0)


predict_locs <- as.data.frame(dplyr::data_frame(lon = rasterToPoints(pc1)[,1],
                                  lat = rasterToPoints(pc1)[,2]
))
predict_locs$pc1 <- raster::extract(pc1, predict_locs[,1:2])
predict_locs$pc2 <- raster::extract(pc2, predict_locs[,1:2])
predict_locs$pc3 <- raster::extract(pc3, predict_locs[,1:2])


test <- lrren(obs_locs = obs_locs,
              predict_locs = predict_locs,
              verbose = T, conserve = T, cv = T,
              parallel = F, balance = T, nfold = 10)

plot_obs(test, alpha = 0.05)
plot_predict(test, alpha = 0.05)
plot_cv(test, alpha = 0.05)


alpha <- 0.05
predict_risk <-  dplyr::data_frame(x = test$out$predict$predict_locs[ , 1],
                                   y = test$out$predict$predict_locs[ , 2],
                                   v = test$out$predict$rr)
naband <- predict_risk # save for next step
sp::coordinates(predict_risk) <- ~ x + y # coordinates
sp::gridded(predict_risk) <- TRUE # gridded
predict_risk_raster <- raster::raster(predict_risk)
raster::crs(predict_risk_raster) <- cref0
if (!is.null(cref1)) {
  predict_risk_raster <- raster::projectRaster(predict_risk_raster,
                                               crs = cref1,
                                               method = "ngb")
}

# Create separate layer for NAs (if any)
naband$v <- ifelse(is.na(naband$v), 9999, naband$v)
sp::coordinates(naband) <- ~ x + y # coordinates
sp::gridded(naband) <- TRUE # gridded
NA_risk_raster <- raster::raster(naband)
raster::crs(NA_risk_raster) <- cref0
if (!is.null(cref1)) {
  NA_risk_raster <- raster::projectRaster(NA_risk_raster,
                                          crs = cref1,
                                          method = "ngb")
}
naband_reclass <- raster::reclassify(NA_risk_raster, c(-Inf, 9998, NA,
                                                       9998, Inf, 1))

# Convert to geospatial raster
predict_tol <- dplyr::data_frame(x = test$out$predict$predict_locs[ , 1],
                                 y = test$out$predict$predict_locs[ , 2],
                                 v = test$out$predict$pval)
sp::coordinates(predict_tol) <- ~ x + y # coordinates
sp::gridded(predict_tol) <- TRUE # gridded
predict_tol_raster <- raster::raster(predict_tol)
raster::crs(predict_tol_raster) <- cref0
if (!is.null(cref1)) {
  predict_tol_raster <- raster::projectRaster(predict_tol_raster,
                                              crs = cref1,
                                              method = "ngb")
}

reclass_tol <- raster::cut(predict_tol_raster,
                           breaks = c(-Inf, alpha / 2, 1 - alpha / 2, Inf),
                           right = FALSE)

# Plot 1: log relative risk
plot_cols = c("#0000cd", "#cccccc", "#8b3a3a", "#ffff00")
rrp <- lrr_raster(input = predict_risk_raster,
                  cols = plot_cols[c(3, 2, 1)],
                  midpoint = 0)

plot.ppp(gorillas, use.marks = F)
fields::image.plot(rrp$v,
                   add = TRUE,
                   breaks = rrp$breaks,
                   col = rrp$cols,
                   axes = TRUE,
                   horizontal = TRUE,
                   main = "log relative risk",
                   xlab = "Longitude",
                   ylab = "Latitude",
                   axis.args = list(at = rrp$at,
                                    labels = rrp$labels,
                                    cex.axis = 0.67))
raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
plot.ppp(gorillas, use.marks = F, add = T)

# Plot 2: Significant p-values
if (all(raster::values(reclass_tol)[!is.na(raster::values(reclass_tol))] == 2)) {
  pcols <- plot_cols[2]
  brp <- c(1, 3)
  atp <- 2
  labp <- "Insignificant"
} else {
  pcols <- plot_cols[c(3, 2, 1)]
  brp <- c(1, 1.67, 2.33, 3)
  atp <- c(1.33, 2, 2.67)
  labp <- c("Presence", "Insignificant", "Absence")
}

plot.ppp(gorillas, use.marks = F)
fields::image.plot(reclass_tol,
                   add = TRUE,
                   horizontal = TRUE,
                   breaks = brp,
                   col = pcols,
                   axes = TRUE,
                   main = paste("Significant p-values\nalpha =", alpha, sep = " "),
                   xlab = "Longitude",
                   ylab = "Latitude",
                   axis.args = list(at = atp,
                                    labels = labp,
                                    las = 0,
                                    cex.axis = 0.67))
raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
plot.ppp(gorillas, use.marks = F, add = T, col = "black")




class(elevation_raster)

spatstat.data::bei
spatstat.data::bei.extra

set.seed(1234)
bei <- spatstat.data::bei
spatstat.core::marks(bei)$group <- rep(1, bei$n)
absence <- spatstat.core::rpoispp(0.01, win = bei)
spatstat.core::marks(absence)$group <- rep(0, absence$n)
obs_locs <- spatstat.core::superimpose(bei, absence)

spatstat.core::marks(obs_locs)$id <- seq(1, obs_locs$n, 1)


# perlrr example
# ------------------ #
# Necessary packages #
# ------------------ #

library(envi)
library(raster)
library(spatstat.core)
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
spatstat.core::marks(presence) <- data.frame("presence" = rep(1, bei$n),
                                             "lon" = bei$x,
                                             "lat" = bei$y)

# (Pseudo-)Absence locations
absence <- spatstat.core::rpoispp(0.008, win = ims[[1]])
spatstat.core::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                            "lon" = absence$x,
                                            "lat" = absence$y)

# Combine into readable format
obs_locs <- spatstat.core::superimpose(presence, absence, check = FALSE)
spatstat.core::marks(obs_locs)$id <- seq(1, obs_locs$n, 1)
spatstat.core::marks(obs_locs) <- spatstat.core::marks(obs_locs)[ , c(4, 2, 3, 1)]

# Specify categories for varying degrees of spatial uncertainty
## Creates three groups
spatstat.core::marks(obs_locs)$levels <- as.factor(stats::rpois(obs_locs$n, lambda = 0.05))

# Run perlrren
  test <- perlrren(obs_ppp = obs_locs,
                 covariates = ims,
                 predict = TRUE,
                 radii = c(10,100,500),
                 parallel = FALSE,
                 alpha = 0.01,
                 n_sim = 10)

plot_perturb(test, predict = T)


plot(test[[1]])
plot(test[[2]])
plot(test[[3]])
plot(test[[4]])

# If adding threshold category for plot_perturb
if (is.null(prop_thresh)) {
  pval_prop <- raster::cut(pval_prop,
                           breaks = c(-Inf, prop_thresh, Inf),
                           right = FALSE)
  values(pval_prop) <- factor(values(pval_prop),
                              labels = c("sufficient", "insufficient"))
  
}


