## jitter_lrren TEST

## Packages
library(dplyr)
library(maps)
library(raster)
library(prism)
library(sp)
library(rgdal) #projectRaster()
library(rgeos)

source("code/R_functions/jitter_KnD.R")
source("code/R_functions/jitter_lrren.R")

# Environmental Data
us <- raster::getData("GADM", country="USA", level=1)
wstates <- c("California", "Oregon", "Washington", "Idaho", "Nevada", "Arizona", "Utah", "Montana", "Wyoming", "Colorado", "New Mexico", "Texas", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "North Dakota")
wabrev <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "MT", "WY", "CO", "NM", "TX", "OK", "KS", "NE", "SD", "ND")
wus <- us[match(toupper(wstates),toupper(us$NAME_1)),]

# Extract outline in order to create buffer to capture all of PRISM
# region union kills the data frame so don't overwrite 'wus'
library(rgeos)
regs <- gUnaryUnion(wus)
# takes way too long to plot without simplifying the polygons
regs <- gSimplify(regs, 0.05, topologyPreserve = TRUE)
#plot(regs)
# Add 0.1 degree buffer to capture all of PRISM
wus_buffer <- gBuffer(regs, width=0.1, byid=TRUE)
# Check with plot
#ext <- drawExtent() # Create WUS extent for clearer plots
#plot(ppt_rast, ext = ext)
#plot(wus, add = TRUE)
#plot(wus_buffer, add = TRUE)

#####
## Download PRISM 30-Year Average Normals
#####

library(prism)
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
ppt <- ls_prism_data(absPath=T)[1,2]
ppt_rast <- raster::raster(ppt)
tdmean <- ls_prism_data(absPath=T)[2,2]
tdmean_rast <- raster(tdmean)
tmax <- ls_prism_data(absPath=T)[3,2]
tmax_rast <- raster(tmax)
tmean <- ls_prism_data(absPath=T)[4,2]
tmean_rast <- raster(tmean)
tmin <- ls_prism_data(absPath=T)[5,2]
tmin_rast <- raster(tmin)
vpdmax <- ls_prism_data(absPath=T)[6,2]
vpdmax_rast <- raster(vpdmax)
vpdmin <- ls_prism_data(absPath=T)[7,2]
vpdmin_rast <- raster(vpdmin)

# Reproject PRISM Rasters
reproj_ppt_rast <- projectRaster(ppt_rast, crs = crs(us))
reproj_tdmean_rast <- projectRaster(tdmean_rast, crs = crs(us))
reproj_tmax_rast <- projectRaster(tmax_rast, crs = crs(us))
reproj_tmean_rast <- projectRaster(tmean_rast, crs = crs(us))
reproj_tmin_rast <- projectRaster(tmin_rast, crs = crs(us))
reproj_vpdmax_rast <- projectRaster(vpdmax_rast, crs = crs(us))
reproj_vpdmin_rast <- projectRaster(vpdmin_rast, crs = crs(us))

# Scale Rasters by Range Transformation
scaled_reproj_ppt_rast <- (reproj_ppt_rast-min(na.omit(reproj_ppt_rast@data@values)))/(max(na.omit(reproj_ppt_rast@data@values))-min(na.omit(reproj_ppt_rast@data@values)))
scaled_reproj_tdmean_rast <- (reproj_tdmean_rast-min(na.omit(reproj_tdmean_rast@data@values)))/(max(na.omit(reproj_tdmean_rast@data@values))-min(na.omit(reproj_tdmean_rast@data@values)))
scaled_reproj_tmax_rast <- (reproj_tmax_rast-min(na.omit(reproj_tmax_rast@data@values)))/(max(na.omit(reproj_tmax_rast@data@values))-min(na.omit(reproj_tmax_rast@data@values)))
scaled_reproj_tmean_rast <- (reproj_tmean_rast-min(na.omit(reproj_tmean_rast@data@values)))/(max(na.omit(reproj_tmean_rast@data@values))-min(na.omit(reproj_tmean_rast@data@values)))
scaled_reproj_tmin_rast <- (reproj_tmin_rast-min(na.omit(reproj_tmin_rast@data@values)))/(max(na.omit(reproj_tmin_rast@data@values))-min(na.omit(reproj_tmin_rast@data@values)))
scaled_reproj_vpdmax_rast <- (reproj_vpdmax_rast-min(na.omit(reproj_vpdmax_rast@data@values)))/(max(na.omit(reproj_vpdmax_rast@data@values))-min(na.omit(reproj_vpdmax_rast@data@values)))
scaled_reproj_vpdmin_rast <-(reproj_vpdmin_rast-min(na.omit(reproj_vpdmin_rast@data@values)))/(max(na.omit(reproj_vpdmin_rast@data@values))-min(na.omit(reproj_vpdmin_rast@data@values)))

# Spatial PCA
#install.packages("RStoolbox")
library(RStoolbox)
#library(raster)

# Raster Stack for PCA
mask_rasters_scaled <- stack(scaled_reproj_ppt_rast, scaled_reproj_tdmean_rast,
                             scaled_reproj_tmax_rast,scaled_reproj_tmean_rast,
                             scaled_reproj_tmin_rast,scaled_reproj_vpdmax_rast,scaled_reproj_vpdmin_rast)

# PCA (rasters masked by window)
pca1 <- rasterPCA(mask_rasters_scaled)
summary(pca1$model)
pca1$model$loadings

# Extract Bands from PCA
pc1 <- pca1$map
pc1_b1 <- pc1[[1]] # PC1
pc1_b2 <- pc1[[2]] # PC2
#pc1_b3 <- pc1[[3]] # PC3

# MASK to WUS
pc1_b1_mask <- mask(pc1_b1, wus)
pc1_b2_mask <- mask(pc1_b2, wus)

# Scale Up so minimum value is less than or equal to 0 for K&D test to work
pc1_scaled <- (pc1_b1_mask + abs(pc1_b1_mask@data@min))
pc2_scaled <- (pc1_b2_mask + abs(pc1_b2_mask@data@min))
#pc3_scaled <- pc1_b3 + abs(pc1_b3@data@min)

# Western US Extraction Points
extract_points_wus <- raster::rasterToPoints(pc1_scaled)
extract_points_wus <- extract_points_wus[,1:2]
predict_locs <- data.frame(coordinates(extract_points_wus), raster::extract(pc1_scaled,extract_points_wus), raster::extract(pc2_scaled,extract_points_wus))
names(predict_locs) <- c("lon", "lat", "pc1_scaled", "pc2_scaled")
summary(predict_locs[,3])

## Observation Data
usda_coyote_raw <- read.csv("/Users/IDB/Box Sync/Dissertation/Data/US_COYOTE_ANALYSIS/Buller_coyote plague_data_latlong_V5.csv")
usda_coyote <- usda_coyote_raw

usda_coyote_plague <- subset(usda_coyote, PlagueResults == 1)
usda_coyote_neg <- subset(usda_coyote, PlagueResults == 0)

# Remove Alaska
usda_l48 <- subset(usda_coyote, State != "AK")
# Western US Only
usda_wus <- subset(usda_l48, State %in% wabrev)

# Restructure for jitter_KnD function
sim_dat <- usda_wus[,c(1,7,6,12,10)]
# Recode levels
sim_dat$Sex <- as.integer(ifelse(sim_dat$Sex == "Female", 1, ifelse(sim_dat$Sex == "Male", 2,3)))
names(sim_dat) <- c("id", "lon", "lat", "mark", "levels")
str(sim_dat)

# Restructure for jitter_KnD function
raster_dat <- stack(pc1_scaled, pc2_scaled)

# matrix of distances for each level (levels x 2 matrix)
distances_mat <- matrix(data = c(500,0,2000,0,6000,0), nrow = 3, ncol = 2, byrow = T)

# Choice of coordinates reference system
crs_wgs84 <- "+proj=longlat +datum=WGS84"

# Number of simulations
sim_num <- 3


## Function Run
test_jitterKnD <- NULL
test_jitterKnD <- jitterKnD(data = sim_dat, rasters = raster_dat, sim = sim_num, distances = distances_mat, crs = crs_wgs84)
str(test_jitterKnD)
sim_dat_lrren <- test_jitterKnD[[2]]
str(sim_dat_lrren)

plot(sim_dat$lon, sim_dat$lat, col = "black", pch = 16)
points(test_jitterKnD[[1]]$lon, test_jitterKnD[[1]]$lat, col = "red", pch = 1)
points(test_jitterKnD[[2]]$lon, test_jitterKnD[[2]]$lat, col = "blue", pch = 1)
points(predict_locs[,1], predict_locs[,2], col = "black", pch = 15, cex = 0.2)

## Function Run
source("code/R_functions/jitter_lrren.R")
test_jitter_llren <- jitter_llren(sim_locs = sim_dat_lrren, predict_locs = predict_locs, predict = T, plot = T, tolerate = T, adapt = F, resolution = 200)
