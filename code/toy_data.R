# -------------------------------------------------------------- #
# Toy data for calculating overlap in niche of mulitiple species
#
# Created by: Ian Buller, Ph.D., M.A. (Github: @idblr)
# Created on: August 4, 2018
#
# Recently edited by: @idblr
# Recently edited on: September 3, 2020
#
# Notes:
# A) Toy data to estimate niche overlap and reproject to geographic space
# B) Toy data is random normal, no induced clustering initially
# C) Environmental variable chosen is PRISM data (http://www.prism.oregonstate.edu/)
# D) Variables are standardized using the range transformation
# E) Dimensionality reduction perfomed using Principal Component Analysis (non-spatial)
# F) 09/02/2020 (@idblr) - Updated example to run updates to lrren() function and new plotting functions
# -------------------------------------------------------------- #

### Set seed
set.seed(42) # the answer to life, the universe and everything

# -------- ####
# Packages #
# -------- ####
library(prism) # prism data
library(sp) # spatial data manipulation
library(raster) # raster data manipulation
library(RStoolbox) # PCA of rasters
library(maps) # visualize geographical data
library(rgl) # 3-D visualization

# ---- ####
# Data #
# ---- ####
## Observation Data (Random normal, more complicated clustering can be achieved with other packages)
# Set unbalanced prevalence
samplesize_case <- 100
samplesize_control <- 900
samplesize_case / sum(samplesize_case, samplesize_control)* 100 # Prevalence (%)

case_locs <- as.data.frame(dplyr::data_frame( # case (presence) locations
  lon = rnorm(samplesize_case)*0.6 - 105.5,
  lat = rnorm(samplesize_case)*0.6 + 39
))

control_locs <- as.data.frame(dplyr::data_frame( # control (negative) locations
  lon = rnorm(samplesize_control)*0.6 - 105.5,
  lat = rnorm(samplesize_control)*0.6 + 39
))

obs_coords <- cbind(c(case_locs$lon, control_locs$lon), c(case_locs$lat, control_locs$lat)) # in degrees latitude and longitude, can convert to northing and easting, if desired

## Window (for geographical visualization and raster masking)
bbox_grid <- sp::bbox(obs_coords)
bbox_grid[1,1] <- sp::bbox(obs_coords)[1,1] - 0.5
bbox_grid[1,2] <- sp::bbox(obs_coords)[1,2] + 0.5
bbox_grid[2,1] <- sp::bbox(obs_coords)[2,1] - 0.5
bbox_grid[2,2] <- sp::bbox(obs_coords)[2,2] + 0.5

## Environmental Data
# Here, PRISM 30-Year Average Normals (can use bioclim or others)
# Download files
options(prism.path = "~/prismtmp")
prism::get_prism_normals(type= "tmean"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)
prism::get_prism_normals(type= "tmax"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)
prism::get_prism_normals(type= "tmin"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)
prism::get_prism_normals(type= "ppt"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)
prism::get_prism_normals(type= "vpdmin"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)
prism::get_prism_normals(type= "vpdmax"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)
prism::get_prism_normals(type= "tdmean"
                         ,resolution="4km"
                         ,annual = TRUE
                         ,keepZip=FALSE)

# Convert to Rasters
ppt <- prism::ls_prism_data(absPath=T)[1,2]
ppt_rast <- raster::raster(ppt)
tdmean <- prism::ls_prism_data(absPath=T)[2,2]
tdmean_rast <- raster::raster(tdmean)
tmax <- prism::ls_prism_data(absPath=T)[3,2]
tmax_rast <- raster::raster(tmax)
tmean <- prism::ls_prism_data(absPath=T)[4,2]
tmean_rast <- raster::raster(tmean)
tmin <- prism::ls_prism_data(absPath=T)[5,2]
tmin_rast <- raster::raster(tmin)
vpdmax <- prism::ls_prism_data(absPath=T)[6,2]
vpdmax_rast <- raster::raster(vpdmax)
vpdmin <- prism::ls_prism_data(absPath=T)[7,2]
vpdmin_rast <- raster::raster(vpdmin)

# Reproject PRISM Rasters
crs_us <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
reproj_ppt_rast <- raster::projectRaster(ppt_rast, crs = crs(crs_us))
reproj_tdmean_rast <- raster::projectRaster(tdmean_rast, crs = crs(crs_us))
reproj_tmax_rast <- raster::projectRaster(tmax_rast, crs = crs(crs_us))
reproj_tmean_rast <- raster::projectRaster(tmean_rast, crs = crs(crs_us))
reproj_tmin_rast <- raster::projectRaster(tmin_rast, crs = crs(crs_us))
reproj_vpdmax_rast <- raster::projectRaster(vpdmax_rast, crs = crs(crs_us))
reproj_vpdmin_rast <- raster::projectRaster(vpdmin_rast, crs = crs(crs_us))

# Scale Rasters by Range Transformation (can do other scaling)
scaled_reproj_ppt_rast <- (reproj_ppt_rast-min(na.omit(reproj_ppt_rast@data@values)))/(max(na.omit(reproj_ppt_rast@data@values))-min(na.omit(reproj_ppt_rast@data@values)))
scaled_reproj_tdmean_rast <- (reproj_tdmean_rast-min(na.omit(reproj_tdmean_rast@data@values)))/(max(na.omit(reproj_tdmean_rast@data@values))-min(na.omit(reproj_tdmean_rast@data@values)))
scaled_reproj_tmax_rast <- (reproj_tmax_rast-min(na.omit(reproj_tmax_rast@data@values)))/(max(na.omit(reproj_tmax_rast@data@values))-min(na.omit(reproj_tmax_rast@data@values)))
scaled_reproj_tmean_rast <- (reproj_tmean_rast-min(na.omit(reproj_tmean_rast@data@values)))/(max(na.omit(reproj_tmean_rast@data@values))-min(na.omit(reproj_tmean_rast@data@values)))
scaled_reproj_tmin_rast <- (reproj_tmin_rast-min(na.omit(reproj_tmin_rast@data@values)))/(max(na.omit(reproj_tmin_rast@data@values))-min(na.omit(reproj_tmin_rast@data@values)))
scaled_reproj_vpdmax_rast <- (reproj_vpdmax_rast-min(na.omit(reproj_vpdmax_rast@data@values)))/(max(na.omit(reproj_vpdmax_rast@data@values))-min(na.omit(reproj_vpdmax_rast@data@values)))
scaled_reproj_vpdmin_rast <-(reproj_vpdmin_rast-min(na.omit(reproj_vpdmin_rast@data@values)))/(max(na.omit(reproj_vpdmin_rast@data@values))-min(na.omit(reproj_vpdmin_rast@data@values)))

# Mask scaled rasters by study area (window)
# Example window = Colorado, USA
bbox_coords <- matrix(c(bbox_grid[1,1], bbox_grid[2,1],
                        bbox_grid[1,1], bbox_grid[2,2],
                        bbox_grid[1,2], bbox_grid[2,2],
                        bbox_grid[1,2], bbox_grid[2,1],
                        bbox_grid[1,1], bbox_grid[2,1]),
                      ncol = 2, byrow = T)
bbox_poly <- sp::Polygon(bbox_coords) # convert to polygon
bbox_poly_reproj <- sp::SpatialPolygons(list(Polygons(list(bbox_poly), ID = "a")), proj4string=CRS(crs_us))

# Mask rasters to study area
mask_ppt_rast_scale <- raster::mask(scaled_reproj_ppt_rast,bbox_poly_reproj)
mask_tdmean_rast_scale <- raster::mask(scaled_reproj_tdmean_rast,bbox_poly_reproj)
mask_tmax_rast_scale <- raster::mask(scaled_reproj_tmax_rast,bbox_poly_reproj)
mask_tmean_rast_scale <- raster::mask(scaled_reproj_tmean_rast,bbox_poly_reproj)
mask_tmin_rast_scale <- raster::mask(scaled_reproj_tmin_rast,bbox_poly_reproj)
mask_vpdmax_rast_scale <- raster::mask(scaled_reproj_vpdmax_rast,bbox_poly_reproj)
mask_vpdmin_rast_scale <- raster::mask(scaled_reproj_vpdmin_rast,bbox_poly_reproj)

# Raster Stack for PCA
mask_rasters_scaled <- raster::stack(mask_ppt_rast_scale, mask_tdmean_rast_scale,mask_tmax_rast_scale,mask_tmean_rast_scale,mask_tmin_rast_scale,mask_vpdmax_rast_scale,mask_vpdmin_rast_scale)

# Spatial PCA
pca1 <- RStoolbox::rasterPCA(mask_rasters_scaled)
summary(pca1$model) # PCA components
pca1$model$loadings # PCA loadings

# Extract Bands from PCA
pc1 <- pca1$map
pc1_b1 <- pc1[[1]] # PC1
pc1_b2 <- pc1[[2]] # PC2
pc1_b3 <- pc1[[3]] # PC3

## Extract Predictor Data to Observation Data

# Case locations
case_locs <- data.frame(case_locs, raster::extract(pc1_b1, case_locs), raster::extract(pc1_b2,case_locs), raster::extract(pc1_b3,case_locs))
names(case_locs) <- c("lon", "lat", "pc1", "pc2", "pc3")

# Control locations
control_locs <- data.frame(control_locs, raster::extract(pc1_b1, control_locs), raster::extract(pc1_b2,control_locs), raster::extract(pc1_b3,control_locs))
names(control_locs) <- c("lon", "lat", "pc1", "pc2", "pc3")

# Prediction Data (area to predict)
predict_locs <- as.data.frame(dplyr::data_frame(
  lon = rasterToPoints(mask_ppt_rast_scale)[,1],
  lat = rasterToPoints(mask_ppt_rast_scale)[,2]
))
predict_locs <- data.frame(predict_locs, raster::extract(pc1_b1, predict_locs), raster::extract(pc1_b2,predict_locs), raster::extract(pc1_b3,predict_locs))
names(predict_locs) <- c("lon", "lat", "pc1", "pc2", "pc3")

### Visualize Data
# Case-Control Data
PA <- as.data.frame(cbind(c(case_locs$lon, control_locs$lon), c(case_locs$lat, control_locs$lat), c(rep(1, nrow(case_locs)), rep(0,nrow(control_locs)))))
names(PA) <- c("x", "y", "Species")
PA$x <- as.numeric(PA$x)
PA$y <- as.numeric(PA$y)
pa_data <- sp::SpatialPointsDataFrame(PA[,c("x", "y")], PA, proj4string=crs(mask_rasters_scaled))

# Visualize species data in geographic space
plot(pc1_b1, ext = bbox_grid) # plot raster data (e.g. PCA band 1)
points(pa_data[which(pa_data$Species==0), ], col="blue", pch = 1, cex = 0.5) # add absence points
points(pa_data[which(pa_data$Species==1), ], col="red", pch = 4, cex = 0.5) # add presence points
legend("bottomright", legend=c("Presence","Absence"), col=c(2, 4), pch=c(1,1), bty="n")
maps::map("county", add = T, regions="colorado")

# Visualize (test) prediction data in geographic space
plot(pc1_b1, ext = bbox_grid) # plot raster data or can plot a PCA band
points(predict_locs$lon, predict_locs$lat, col="black", pch = 1, cex = 0.5) # add raster centroid data

# Visualize locations in preditor space (PC1, PC2)
par(pty="s")
plot(predict_locs$pc1, predict_locs$pc2, col="black", pch = 16, cex = 0.2,
     xlab = "PC1", ylab = "PC2")
points(control_locs$pc1, control_locs$pc2, col="blue", pch = 1, cex = 0.5)
points(case_locs$pc1, case_locs$pc2, col="red", pch = 4, cex = 0.5)

# Visualize locations in preditor space (PC1, PC3)
plot(predict_locs$pc1, predict_locs$pc3, col="black", pch = 16, cex = 0.2,
     xlab = "PC1", ylab = "PC3")
points(control_locs$pc1, control_locs$pc3, col="blue", pch = 1, cex = 0.5)
points(case_locs$pc1, case_locs$pc3, col="red", pch = 4, cex = 0.5)

# Visualize locations in preditor space (PC2, PC3)
plot(predict_locs$pc2, predict_locs$pc3, col="black", pch = 16, cex = 0.2,
     xlab = "PC2", ylab = "PC3")
points(control_locs$pc2, control_locs$pc3, col="blue", pch = 1, cex = 0.5)
points(case_locs$pc2, case_locs$pc3, col="red", pch = 4, cex = 0.5)

## Visualize in 3-D predictor space
# Data preparation
dat_3d <- data.frame("pc1" = c(case_locs$pc1,control_locs$pc1,predict_locs$pc1),
                     "pc2" = c(case_locs$pc2,control_locs$pc2,predict_locs$pc2),
                     "pc3" = c(case_locs$pc3,control_locs$pc3,predict_locs$pc3))
dat_3d$mark <- NULL
dat_3d$mark <- c(rep(3,nrow(case_locs)), rep(2,nrow(control_locs)),rep(1,nrow(predict_locs)))

# 3-D Visualization
rgl::plot3d(x = dat_3d$pc1, y = dat_3d$pc2, z = dat_3d$pc3,
            xlab = "PC1", ylab = "PC2", zlab = "PC3", pch = 16, cex = 0.2, col = c("black", "blue", "red")[dat_3d$mark], cex = c(rep(0.2, nrow(predict_locs)), rep(1, nrow(control_locs)+nrow(case_locs))))

# Test updated lrren() function
## Data preparation
case_locs$mark <- rep(1, nrow(case_locs))
case_locs$id <- seq(1, nrow(case_locs), 1)
control_locs$mark <- rep(0, nrow(control_locs))
control_locs$id <- rep(1, nrow(control_locs), 1)
obs_locs <- rbind(case_locs[ , c(7,1,2,6,3,4)], control_locs[ , c(7,1,2,6,3,4)])

## lrren
set.seed(1234)
test <- lrren(obs_locs = obs_locs,
              predict_locs = predict_locs,
              verbose = T, conserve = T, cv = T,
              parallel = F, balance = T)

## lrren plots
plot_obs(test, alpha = 0.05)
plot_predict(test, alpha = 0.05)
plot_cv(test, alpha = 0.05)

# https://www.farb-tabelle.de/en/rgb2hex.htm?q=yellow
# blue3: #0000cd
# cornflowerblue: #6495ed
# grey80: #cccccc
# indianred1: #ff6a6a
# indianred4: #8b3a3a
# yellow: #ffff00

# ------------------------ END OF CODE ------------------------- #
