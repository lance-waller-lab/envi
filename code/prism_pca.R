## ------------------------------------- #
## Principal Component Analysis of raster data
## -------
## PRISM Climate Annual Average Normals 
## Masked for California, USA
## 
## Author: Ian Buller (@idblr)
## Date created: November, 8 2018
##
## Most Recently Modified on: December 3, 2018
## Most Recently Modified by: Ian Buller
##
## Modifications:
# A) 12/03/2018: Added two other variable standardization options
# B) 12/03/2018: Added third principal component
## ------------------------------------- #

# -------- ####
# PACKAGES #
# -------- ####
library(prism) # prism data
library(sp) # spatial data manipulation
library(raster) # raster data manipulation
library(RStoolbox) # PCA of rasters
library(maps) # visualize geographical data
library(rgeos) # calculate buffers

### Set seed
set.seed(42) 

# ---- ####
# DATA #
# ---- ####
# Environmental Data
# PRISM 30-Year Average Normals
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

# Set Projection of PRISM Rasters
crs_us <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # original CRS
reproj_ppt_rast <- raster::projectRaster(ppt_rast, crs = crs(crs_us))
reproj_tdmean_rast <- raster::projectRaster(tdmean_rast, crs = crs(crs_us))
reproj_tmax_rast <- raster::projectRaster(tmax_rast, crs = crs(crs_us))
reproj_tmean_rast <- raster::projectRaster(tmean_rast, crs = crs(crs_us))
reproj_tmin_rast <- raster::projectRaster(tmin_rast, crs = crs(crs_us))
reproj_vpdmax_rast <- raster::projectRaster(vpdmax_rast, crs = crs(crs_us))
reproj_vpdmin_rast <- raster::projectRaster(vpdmin_rast, crs = crs(crs_us))

## Standardize rasters for more appropriate comparisons

# Standardize Rasters by Range Transformation
stand_reproj_ppt_rast <- (reproj_ppt_rast-min(na.omit(reproj_ppt_rast@data@values)))/(max(na.omit(reproj_ppt_rast@data@values))-min(na.omit(reproj_ppt_rast@data@values)))
stand_reproj_tdmean_rast <- (reproj_tdmean_rast-min(na.omit(reproj_tdmean_rast@data@values)))/(max(na.omit(reproj_tdmean_rast@data@values))-min(na.omit(reproj_tdmean_rast@data@values)))
stand_reproj_tmax_rast <- (reproj_tmax_rast-min(na.omit(reproj_tmax_rast@data@values)))/(max(na.omit(reproj_tmax_rast@data@values))-min(na.omit(reproj_tmax_rast@data@values)))
stand_reproj_tmean_rast <- (reproj_tmean_rast-min(na.omit(reproj_tmean_rast@data@values)))/(max(na.omit(reproj_tmean_rast@data@values))-min(na.omit(reproj_tmean_rast@data@values)))
stand_reproj_tmin_rast <- (reproj_tmin_rast-min(na.omit(reproj_tmin_rast@data@values)))/(max(na.omit(reproj_tmin_rast@data@values))-min(na.omit(reproj_tmin_rast@data@values)))
stand_reproj_vpdmax_rast <- (reproj_vpdmax_rast-min(na.omit(reproj_vpdmax_rast@data@values)))/(max(na.omit(reproj_vpdmax_rast@data@values))-min(na.omit(reproj_vpdmax_rast@data@values)))
stand_reproj_vpdmin_rast <-(reproj_vpdmin_rast-min(na.omit(reproj_vpdmin_rast@data@values)))/(max(na.omit(reproj_vpdmin_rast@data@values))-min(na.omit(reproj_vpdmin_rast@data@values)))

# # Standardize Rasters by Z-Transformation
# stand_reproj_ppt_rast <- (reproj_ppt_rast-mean(na.omit(reproj_ppt_rast@data@values)))/sd(na.omit(reproj_ppt_rast@data@values))
# stand_reproj_tdmean_rast <- (reproj_tdmean_rast-mean(na.omit(reproj_tdmean_rast@data@values)))/sd(na.omit(reproj_tdmean_rast@data@values))
# stand_reproj_tmax_rast <- (reproj_tmax_rast-mean(na.omit(reproj_tmax_rast@data@values)))/sd(na.omit(reproj_tmax_rast@data@values))
# stand_reproj_tmean_rast <- (reproj_tmean_rast-mean(na.omit(reproj_tmean_rast@data@values)))/sd(na.omit(reproj_tmean_rast@data@values))
# stand_reproj_tmin_rast <- (reproj_tmin_rast-mean(na.omit(reproj_tmin_rast@data@values)))/sd(na.omit(reproj_tmin_rast@data@values))
# stand_reproj_vpdmax_rast <- (reproj_vpdmax_rast-mean(na.omit(reproj_vpdmax_rast@data@values)))/sd(na.omit(reproj_vpdmax_rast@data@values))
# stand_reproj_vpdmin_rast <- (reproj_vpdmin_rast-mean(na.omit(reproj_vpdmin_rast@data@values)))/sd(na.omit(reproj_vpdmin_rast@data@values))

# # Standardize Rasters by scale function
# stand_reproj_ppt_rast <- scale(reproj_ppt_rast, center=TRUE, scale=TRUE)
# stand_reproj_tdmean_rast <- scale(reproj_tdmean_rast, center=TRUE, scale=TRUE)
# stand_reproj_tmax_rast <- scale(reproj_tmax_rast, center=TRUE, scale=TRUE)
# stand_reproj_tmean_rast <- scale(reproj_tmean_rast, center=TRUE, scale=TRUE)
# stand_reproj_tmin_rast <- scale(reproj_tmin_rast, center=TRUE, scale=TRUE)
# stand_reproj_vpdmax_rast <- scale(reproj_vpdmax_rast, center=TRUE, scale=TRUE)
# stand_reproj_vpdmin_rast <- scale(reproj_vpdmin_rast, center=TRUE, scale=TRUE)

# ---------------------------- ####
# PRINCIPAL COMPONENT ANALYSIS #
# ---------------------------- ####
# Raster Stack for PCA
rasters_scaled <- raster::stack(scaled_reproj_ppt_rast, scaled_reproj_tdmean_rast,scaled_reproj_tmax_rast,scaled_reproj_tmean_rast,scaled_reproj_tmin_rast,scaled_reproj_vpdmax_rast,scaled_reproj_vpdmin_rast)

# Spatial PCA
pca1 <- RStoolbox::rasterPCA(rasters_scaled)
summary(pca1$model) # PCA components
pca1$model$loadings # PCA loadings

# Extract Bands from PCA
pc1 <- pca1$map
pc1_b1 <- pc1[[1]] # PC1
pc1_b2 <- pc1[[2]] # PC2
pc1_b3 <- pc1[[3]] # PC3

# ------------------ ####
# DATA VISUALIZATION #
# ------------------ ####
# Mask standardized rasters by study area (window)
# Here, example using California, USA
us <- raster::getData("GADM", country="USA", level=1) # get US polygon data
ca <- us[match(toupper("California"),toupper(us$NAME_1)),] # get California polygon
# Extract outline in order to create buffer to capture all of PRISM
# region union kills the data frame so don't overwrite 'wus'
regs <- rgeos::gUnaryUnion(ca)
# takes way too long to plot without simplifying the polygons
regs <- rgeos::gSimplify(regs, 0.05, topologyPreserve = TRUE)
# Add 0.1 degree buffer to capture all of PRISM
ca_buffer <- rgeos::gBuffer(regs, width=0.1, byid=TRUE) # same projection as crs_us

# Mask for California
mask_pc1 <- mask(pc1_b1, ca_buffer)
mask_pc2 <- mask(pc1_b2, ca_buffer)
mask_pc3 <- mask(pc1_b3, ca_buffer)

# Plot Principal Components
plot(mask_pc1, ext = ca_buffer)
plot(mask_pc2, ext = ca_buffer)
plot(mask_pc3, ext = ca_buffer)

# ----- End of Code ----- #
