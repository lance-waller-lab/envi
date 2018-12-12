## llren tutorial for method chapter
#
# using CDPH data

### Packages
library(prism) # prism data
library(sp) # spatial data manipulation
library(raster) # raster data manipulation
library(RStoolbox) # PCA of rasters
library(maps) # visualize geographical data
library(rgeos) # calculate buffers

### Set seed
set.seed(42) # the answer to life, the universe and everything

## Environmental Data
# Here, PRISM 30-Year Average Normals (can use bioclim or others)
library(prism)

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

# Mask scaled rasters by study area (window)
us <- raster::getData("GADM", country="USA", level=1)
ca <- us[match(toupper("California"),toupper(us$NAME_1)),]
# Extract outline in order to create buffer to capture all of PRISM
# region union kills the data frame so don't overwrite 'wus'
regs <- rgeos::gUnaryUnion(ca)
# takes way too long to plot without simplifying the polygons
regs <- rgeos::gSimplify(regs, 0.05, topologyPreserve = TRUE)
#plot(regs)
# Add 0.1 degree buffer to capture all of PRISM
ca_buffer <- rgeos::gBuffer(regs, width=0.1, byid=TRUE) # same projection as crs_us

# Mask for California
mask_pc1 <- mask(pc1_b1, ca_buffer)
mask_pc2 <- mask(pc1_b2, ca_buffer)

plot(mask_pc1, ext = ca_buffer)
plot(mask_pc2, ext = ca_buffer)

# Data translation to positive values for K&D calculation
pc1_scaled <- (mask_pc1 + abs(mask_pc1@data@min))
pc2_scaled <- (mask_pc2 + abs(mask_pc2@data@min))

# Prediction Data
extract_points_ca <- rasterToPoints(mask_pc1)
extract_points_ca <- extract_points_ca[,1:2]
predict_locs <- data.frame(coordinates(extract_points_ca), extract(pc1_scaled,extract_points_ca), extract(pc2_scaled,extract_points_ca))
names(predict_locs) <- c("lon", "lat", "pc1_scaled", "pc2_scaled")

# CDPH data
load("CDPH_coyote.RData")

obs_dat <- CDPH_coyote[,c(1,49,48,29)]
names(obs_dat) <- c("id", "lon", "lat", "mark")
obs_dat$mark <- ifelse(obs_dat$mark == "NEG", 0, 1)

obs_dat$pc1_scaled <- raster::extract(pc1_scaled,obs_dat[,2:3])
obs_dat$pc2_scaled <- raster::extract(pc2_scaled,obs_dat[,2:3])


plot(obs_dat$lon[obs_dat$mark == 0], obs_dat$lat[obs_dat$mark == 0], col="black", cex = 0.2, ylab = "Latitude", xlab = "Longitude")
points(obs_dat$lon[obs_dat$mark == 1], obs_dat$lat[obs_dat$mark == 1], col="red", cex = 0.2)
plot(ca, add = T)

plot(pc1_scaled, ext = ca)
plot(pc2_scaled, ext = ca)


plot(obs_dat$pc1_scaled[obs_dat$mark == 0], obs_dat$pc2_scaled[obs_dat$mark == 0], col="black", cex = 0.2, ylab = "PC2", xlab = "PC1",
       ylim =c(min(out$out$outer_poly[,2]), max(out$out$outer_poly[,2])),
     xlim =c(min(out$out$outer_poly[,1]), max(out$out$outer_poly[,1])))
points(obs_dat$pc1_scaled[obs_dat$mark == 1], obs_dat$pc2_scaled[obs_dat$mark == 1], col="red", cex = 0.2)
polygon(out$out$outer_poly)
polygon(out$out$inner_poly)


# Model Run
out <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
               tolerate = T, adapt = F, edge = "diggle", predict = T,
               conserve = T, cv = T, nfold = 25, balance = T, plot = T
)

# Elevation
elev_rast <- raster("/Users/IDB/Box Sync/Dissertation/Data/Environmental/PRISM/PRISM_us_dem_4km_asc/PRISM_us_dem_4km_asc.asc")
reproj_elev_rast <- projectRaster(elev_rast, crs = crs(crs_us))
mask_elev_rast_ca <- mask(reproj_elev_rast,ca_buffer)

# Univariate Plots

out_univar <- as.data.frame(dplyr::data_frame(
  rr = out$out$predict$rr,
  pval = out$out$predict$pval,
  ppt = raster::extract(reproj_ppt_rast, predict_locs[,1:2]),
  tmax = raster::extract(reproj_tmax_rast, predict_locs[,1:2]),
  tmean = raster::extract(reproj_tmean_rast, predict_locs[,1:2]),
  tmin = raster::extract(reproj_tmin_rast, predict_locs[,1:2]),
  tdmean = raster::extract(reproj_tdmean_rast, predict_locs[,1:2]),
  vpdmax = raster::extract(reproj_vpdmax_rast, predict_locs[,1:2]),
  vpdmin = raster::extract(reproj_vpdmin_rast, predict_locs[,1:2]),
  elev = raster::extract(reproj_elev_rast, predict_locs[,1:2])
))

hist(out_univar$pval)
hist(out_univar$rr)
hist(out_univar$elev)

rr <- NULL
ppt <- NULL


out_univar <- na.omit(out_univar[is.finite(out_univar$rr),]) # remove NAs

# Significance Categories
out_univar$pclass <- ifelse(out_univar$pval < 0.025, 1, ifelse(out_univar$pval > 0.975, 0, NA)) # One = plague positive, 0 = plague negative
summary(out_univar$pclass)
table(out_univar$pclass)

# UNIVARIATE PLOTS
library(mgcv)
plot(gam(rr~s(ppt), data = out_univar), pages=1,residuals=F,all.terms=F,shade=TRUE, col = "red", shade.col="blue", xlab = "Precipitation (millimeters)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(ppt), data = out_univar))[1]
     )
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(pval~ppt, data = out_univar, cex = 0.5)
plot(gam(pval~s(ppt), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Precipitation (millimeters)", ylab = "p-value", select = 1, shift = coef(gam(pval~s(ppt), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(tdmean), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Dew Point Temperature (Degrees Celcius)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(tdmean), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(gam(pval~s(tdmean), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Dew Point Temperature (Degrees Celcius)", ylab = "p-value", select = 1, shift = coef(gam(pval~s(tdmean), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(tmax), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Maximum Temperature (Degrees Celcius)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(tmax), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(gam(pval~s(tmax), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Maximum Temperature (Degrees Celcius)", ylab = "p value", select = 1, shift = coef(gam(pval~s(tmax), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(tmean), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Mean Temperature (Degrees Celcius)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(tmean), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(gam(pval~s(tmean), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Mean Temperature (Degrees Celcius)", ylab = "p value", select = 1, shift = coef(gam(pval~s(tmean), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(tmin), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Minimum Temperature (Degrees Celcius)", ylab = "log-relative risk", select = 1, shift = coef(gam(rr~s(tmin), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(gam(pval~s(tmin), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Minimum Temperature (Degrees Celcius)", ylab = "p value", select = 1, shift = coef(gam(pval~s(tmin), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(vpdmax), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Maximum Vapor Pressure (Hectopascal)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(vpdmax), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)


plot(gam(pval~s(vpdmax), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Maximum Vapor Pressure (Hectopascal)", ylab = "p-value", select = 1, shift = coef(gam(pval~s(vpdmax), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(vpdmin), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Minimum Vapor Pressure (Hectopascal)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(vpdmin), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(gam(pval~s(vpdmin), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Minimum Vapor Pressure (Hectopascal)", ylab = "p value", select = 1, shift = coef(gam(pval~s(vpdmin), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)

plot(gam(rr~s(elev), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Elevation (meters)", ylab = "log relative risk", select = 1, shift = coef(gam(rr~s(elev), data = out_univar))[1])
abline(h=0, col = "black", lwd = 2, lty = 2)

plot(gam(pval~s(elev), data = out_univar),pages=1,residuals=T,all.terms=TRUE,shade=TRUE,shade.col="red", xlab = "Elevation (meters)", ylab = "p-value", select = 1, shift = coef(gam(pval~s(vpdmin), data = out_univar))[1], ylim = c(0,1))
abline(h=0.05, col = "black", lwd = 2, lty = 2)
abline(h=0.95, col = "black", lwd = 2, lty = 2)
abline(v=1500, col = "black")
abline(v=2600, col = "black")

reclass_elev_rast <- reclassify(mask_elev_rast_ca,
                                c(-Inf,1499.99999999999,1,
                                  1500,2600,2,
                                  2600.00000000000001,Inf,1))

plot(reclass_elev_rast, ext = ca_buffer)



# t-tests

t_ppt <- t.test(out_univar$ppt ~ out_univar$pclass)

par(mfrow=c(2,1), mai=c(0.8,1,1,1))
#par(mfrow=c(4,2), oma=c(5,2,6,1))
# Precipitation
plot(density(na.omit(out_univar$ppt)), col = "black", ylim = c(0, 0.002), main = "Precipitation Average Normal", xlab = "Precipitation (mm)")
lines(density(na.omit(out_univar$ppt[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$ppt[out_univar$pclass == 1])), col = "red")
legend("topright", bty = "n", legend = c("Significant Difference in Means",
                                         paste(sprintf("%.1f",round(as.numeric(diff(t_ppt$estimate))), digits = 1), " (CI: ", sprintf("%.1f",abs(round(t_ppt$conf.int[1], digits = 1))) ," - ", sprintf("%.1f",abs(round(t_ppt$conf.int[2], digits = 1))),")", sep = ""),
                                         "Suitable Habitat",
                                         "Unsuitable Habitat",
                                         "All Habitat"
                                         ),
       lty = c(NA,NA,1,1,1),
       col = c("black", "black", "red", "blue", "black")
       )
dev.off()


boxplot(out_univar$ppt ~ out_univar$pclass, names = c("Unsuitable Habitat", "Suitable Habitat"), horizontal = T)


tapply(out_univar$ppt,out_univar$pclass, mean, na.rm = T)
tapply(out_univar$ppt,out_univar$pclass, sd, na.rm = T)

#
t_ppt <- t.test(out_univar$ppt ~ out_univar$pclass)
t_tmax <- t.test(out_univar$tmax ~ out_univar$pclass)
t_tmean <- t.test(out_univar$tmean ~ out_univar$pclass)
t_tmin <- t.test(out_univar$tmin ~ out_univar$pclass)
t_tdmean <- t.test(out_univar$tdmean ~ out_univar$pclass)
t_vpdmax <- t.test(out_univar$vpdmax ~ out_univar$pclass)
t_vpdmin <- t.test(out_univar$vpdmin ~ out_univar$pclass)
t_elev <- t.test(out_univar$elev ~ out_univar$pclass)

layout(matrix(c(1,2,3,4,5,6,7,8,9,9), ncol=2, byrow=TRUE), heights=c(rep(1,4),rep(0.5,1))
       #heights = rep(1,5)

       )

#layout.show(layout(matrix(c(1,2,3,4,5,6,7,8,9,9), ncol=2, byrow=TRUE), heights=c(rep(1,4),rep(0.8,1)))

par(pty="m", oma=c(0,0,0,0), mar=c(5.1,4.1,1.1,2.1))
#par(oma=c(2,0,2,0),mar=c(5.1,4.1,4.1,2.1), pty="s", mai = c(0.1,0.2,0.2,0.2))

plot(density(na.omit(out_univar$ppt)), col = "black", ylim = c(0, 0.0015),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_ppt$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_ppt$conf.int[2], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_ppt$conf.int[1], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "precipitation (millimeter)\n")
lines(density(na.omit(out_univar$ppt[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$ppt[out_univar$pclass == 1])), col = "red")

plot(density(na.omit(out_univar$tmax)), col = "black", ylim = c(0, 0.32),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_tmax$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_tmax$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_tmax$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "temperature maximum (degrees Celsius)\n")
lines(density(na.omit(out_univar$tmax[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$tmax[out_univar$pclass == 1])), col = "red")


plot(density(na.omit(out_univar$tmean)), col = "black", ylim = c(0, 0.32),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_tmean$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_tmean$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_tmean$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "temperature mean (degrees Celsius)\n")
lines(density(na.omit(out_univar$tmean[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$tmean[out_univar$pclass == 1])), col = "red")

plot(density(na.omit(out_univar$tmin)), col = "black", ylim = c(0, 0.32),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_tmin$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_tmin$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_tmin$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "temperature minimum (degrees Celsius)\n")
lines(density(na.omit(out_univar$tmin[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$tmin[out_univar$pclass == 1])), col = "red")

plot(density(na.omit(out_univar$tdmean)), col = "black", ylim = c(0, 0.32),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_tdmean$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_tdmean$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_tdmean$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "dew point temperature (degrees Celsius)\n")
lines(density(na.omit(out_univar$tdmean[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$tdmean[out_univar$pclass == 1])), col = "red")

plot(density(na.omit(out_univar$vpdmax)), col = "black", ylim = c(0, 0.25),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_vpdmax$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_vpdmax$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_vpdmax$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "maximum vapor pressure deficit (hectopascal)\n")
lines(density(na.omit(out_univar$vpdmax[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$vpdmax[out_univar$pclass == 1])), col = "red")

plot(density(na.omit(out_univar$vpdmin)), col = "black", ylim = c(0, 0.5),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_vpdmin$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_vpdmin$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_vpdmin$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "minimum vapor pressure deficit (hectopascal)\n")
lines(density(na.omit(out_univar$vpdmin[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$vpdmin[out_univar$pclass == 1])), col = "red")


plot(density(na.omit(out_univar$elev)), col = "black", ylim = c(0, 0.002),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_elev$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_elev$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_elev$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "elevation (meter)\n")
lines(density(na.omit(out_univar$elev[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$elev[out_univar$pclass == 1])), col = "red")

plot.new()
par(pty="m", oma=c(0,0,0,0), mar=c(5.1,4.1,4.1,2.1)*2)
legend(x="top", inset = 0,
       legend = c("Suitable plague habitat",
                  "Unsuitable plague habitat",
                  "All habitat (reference)"),
       lty = 1,
       col = c("red", "blue", "black"),
       bty = "n", horiz = T)




plot(density(na.omit(out_univar$elev)), col = "black", ylim = c(0, 0.002),
     main = paste("Difference in means ", sprintf("%.2f",abs(round(as.numeric(diff(t_elev$estimate)), digits = 2))), " (0.95 CI: ", sprintf("%.2f",abs(round(t_elev$conf.int[1], digits = 2))) ," - ", sprintf("%.2f",abs(round(t_elev$conf.int[2], digits = 2))),")", sep = ""),
     #main = "",
     cex.main = 1, font.main = 1,
     xlab = "elevation (meter)\n")
lines(density(na.omit(out_univar$elev[out_univar$pclass == 0])), col = "blue")
lines(density(na.omit(out_univar$elev[out_univar$pclass == 1])), col = "red")










sim_dat <- CDPH_coyote[,c(1,49,48,29)]
head(sim_dat)
names(sim_dat) <- c("id", "lon", "lat", "mark")
sim_dat$mark <- ifelse(sim_dat$mark == "NEG", 0, 1)
sim_dat$levels <- NULL
sim_dat$levels <- 1

# Restructure for jitter_KnD function
raster_dat <- stack(pc1_scaled, pc2_scaled)

# matrix of distances for each level (levels x 2 matrix)
distances_mat <- matrix(data = c(10000,5000), nrow = 1, ncol = 2, byrow = T)

# Choice of coordinates reference system
crs_wgs84 <- "+proj=longlat +datum=WGS84"

# Number of simulations
sim_num <- 1


## Function Run
test_jitterKnD <- jitterKnD(data = sim_dat, rasters = raster_dat, sim = sim_num, distances = distances_mat, crs = crs_wgs84)
cdph_sim <- test_jitterKnD[[1]]

# Create windows for ppp
geo_owin<-as(ca_buffer, "owin")

outer_chull <- concaveman::concaveman(cbind(predict_locs[,3], predict_locs[,4]))
outer_chull_pts <- sp::coordinates(outer_chull)
outer_chull_pts <- rbind(outer_chull_pts, outer_chull_pts[1,]) #repeats the first coordinate to complete the polygon
outer_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(outer_chull_pts)),1))) # convert points to polygon
if(is.null(poly_buffer)){
  poly_buffer <- max(outer_chull_pts)/1000
} else {poly_buffer <- poly_buffer
}
outer_chull_poly_buffer <- rgeos::gBuffer(outer_chull_poly, width=poly_buffer, byid=TRUE) #add small buffer around polygon to include boundary points
outer_poly <- outer_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon

pre_owin<-owin(poly = list(x=rev(outer_poly[,1]),y=rev(outer_poly[,2])))

cdph_sim$mark_char <- ifelse(cdph_sim$mark == 1, "case", "control")


ca_geo <- ppp(cdph_sim$lon, cdph_sim$lat, window = geo_owin, marks = cdph_sim$mark_char)
plot(ca_geo, cols = c("red", "black"), cex = c(0.5,0.2), pch = c(16,1), main = "Cases and Controls in California")

ca_pre <- ppp(cdph_sim$PC1, cdph_sim$PC2, window = pre_owin, marks = cdph_sim$mark_char)
plot(ca_pre, cols = c("red", "black"), cex = c(0.5,0.2), pch = c(16,1), main = "Cases and Controls in Predictor Space")

case_locs <- cdph_sim[cdph_sim$mark == 1,]
control_locs <- cdph_sim[cdph_sim$mark == 0,]
names(case_locs)

nrow(cdph_sim[cdph_sim$mark == 0,])
nrow(control_locs)
case_locs <- na.omit(case_locs[,c(2,3,7,8)])
control_locs <- na.omit(control_locs[,c(2,3,7,8)])
predict_locs <- na.omit(predict_locs)

str(control_locs)




out <- lrrencv(case_locs = case_locs, control_locs = control_locs,
               predict_locs = predict_locs, tolerate = T, cv = F, nfold = 25, predict = T, balance = T, plot = T
               )

out <- lrrencv(case_locs = case_locs, control_locs = control_locs, adapt = F, conserve = F, edge = "diggle",
               predict_locs = predict_locs, tolerate = T, cv = T, nfold = 25, predict = T, balance = T, plot = T
)


test_jitterKnD <- jitterKnD(data = sim_dat, rasters = raster_dat, sim = sim_num, distances = distances_mat, crs = crs_wgs84)
str(test_jitterKnD)


obs_dat <- na.omit(test_jitterKnD[[1]][,c(1,2,3,4,7,8)])
case_locs <- obs_dat[obs_dat$mark == 1,][,c(1,2,3,5,6)]
control_locs <- obs_dat[obs_dat$mark == 0,][,c(1,2,3,5,6)]
predict_locs <- na.omit(predict_locs)

str(obs_dat)
obs_dat[,6]

out <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
               adapt = F, conserve = F, edge = "diggle",
               tolerate = T, cv = T, nfold = 25, predict = T,
               balance = T, plot = T, h0 = 0.1
)

# Adaptive Runs
out_adapt <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
             tolerate = T, adapt = T, edge = "diggle", predict = T,
             conserve = T, cv = T, nfold = 25, balance = T, plot = T
)

out_adapt_uncon <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
                         tolerate = T, adapt = T, edge = "diggle", predict = T,
                         conserve = F, cv = T, nfold = 25, balance = T, plot = T, h0 = 0.05236957/2
)

# Manual Bandwidth Runs
out_half <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
                   tolerate = T, adapt = F, edge = "diggle", predict = T,
                   conserve = T, cv = T, nfold = 25, balance = T, plot = T, h0 = 0.05236957/2
)

out_half_uncon <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
                  tolerate = T, adapt = F, edge = "diggle", predict = T,
                  conserve = F, cv = T, nfold = 25, balance = T, plot = T, h0 = 0.05236957/2
)

out_double <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
                  tolerate = T, adapt = F, edge = "diggle", predict = T,
                  conserve = T, cv = T, nfold = 25, balance = T, plot = T, h0 = 0.05236957*2
)

out_double_uncon <- lrren(obs_locs = obs_dat, predict_locs = predict_locs,
                        tolerate = T, adapt = F, edge = "diggle", predict = T,
                        conserve = F, cv = T, nfold = 25, balance = T, plot = T, h0 = 0.05236957*2
)




#
# plot(pc1_scaled, ext = ca_buffer)
# points(sim_dat$lon, sim_dat$lat, col = "black", pch = 16, cex = 0.5)
# points(cdph_sim$lon, cdph_sim$lat, col = "red", pch = 1, cex = 0.5)
# maps::map("county", region= "california", add = T)
#
# plot(pc1_scaled, ext = ca_buffer, col = "transparent", legend = F,
#      xlab = "Longitude", ylab = "Latitude",
#      main = "")
# #points(sim_dat$lon, sim_dat$lat, col = "black", pch = 16)
# points(cdph_sim$lon[cdph_sim$mark == 0], cdph_sim$lat[cdph_sim$mark == 0], col = "black", pch = 1, cex = 0.5)
# points(cdph_sim$lon[cdph_sim$mark == 1], cdph_sim$lat[cdph_sim$mark == 1], col = "red", pch = 16, cex = 0.5)
# maps::map("county", region= "california", add = T)


)

hist(out$out$predict$rr)

cref0 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
cref1 <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# Convert to geospatial raster
predict_risk <- as.data.frame(dplyr::data_frame(
  x = out$out$predict$lon ,
  y = out$out$predict$lat,
  rr = out$out$predict$rr
))
naband <- predict_risk # save for next step
coordinates(predict_risk) <- ~ x + y # coordinates
gridded(predict_risk) <- TRUE # gridded
predict_risk_raster <- raster(predict_risk)
crs(predict_risk_raster) <- cref0
predict_risk_raster <- projectRaster(predict_risk_raster, crs = cref1, method = "ngb")
predict_risk_reclass <- predict_risk_raster
predict_risk_reclass[predict_risk_reclass <= -predict_risk_raster@data@max] <- -predict_risk_raster@data@max



upperhalf = length(predict_risk_reclass@data@values[predict_risk_raster@data@values>0 & !is.na(predict_risk_reclass@data@values)])
lowerhalf = length(predict_risk_reclass@data@values[predict_risk_raster@data@values<0 & !is.na(predict_risk_reclass@data@values)])

max_absolute_value=max(predict_risk_reclass@data@values[is.finite(predict_risk_reclass@data@values)]) #what is the maximum absolute value of raster?
min_absolute_value=min(predict_risk_reclass@data@values[is.finite(predict_risk_reclass@data@values)]) #what is the maximum absolute value of raster?
Thresh = 0
## Make vector of colors for values below threshold
rc1 = colorRampPalette(colors = c(col.99, col.50), space="Lab")(lowerhalf)
## Make vector of colors for values above threshold
rc2 = colorRampPalette(colors = c(col.50, col.01), space="Lab")(upperhalf)
rampcols = c(rc1, rc2)
## In your example, this line sets the color for values between 49 and 51.
rampcols[c(upperhalf, lowerhalf+1)] = rgb(t(col2rgb(col.50)), maxColorValue=256)

rb1 = seq(min_absolute_value, Thresh, length.out=lowerhalf+1)
rb2 = seq(Thresh, max_absolute_value, length.out=upperhalf+1)[-1]
rampbreaks = c(rb1, rb2)

# Make custom legend
nhalf = length(predict_risk_reclass@data@values)/2
rclegend = colorRampPalette(colors = c(col.99,col.50, col.01), space="Lab")(nhalf)
rclegend[c(nhalf,nhalf+1)] = rgb(t(col2rgb(col.50)), maxColorValue=256)
legend_image <- as.raster(matrix(rev(rclegend)), ncol=1)

layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(3, 1))
par(oma=c(0,0,0,0),mar=c(0.1,0.1,0.1,0.1)/5, pty="s", mai = c(0,0,0,0), family = "LM Roman 10")
image(predict_risk_reclass, col = rampcols, breaks=rampbreaks,
      #xlab = "Longitude", ylab="Latitude", main = "Predicted log relative risk of cases and controls in geographic space",
      xlab = "", ylab = "",
      xlim = c(predict_risk_raster@extent@xmin-0.01*(abs(predict_risk_raster@extent@xmin)),predict_risk_raster@extent@xmax+0.01*(abs(predict_risk_raster@extent@xmin))), ylim = c(predict_risk_raster@extent@ymin-0.01*(abs(predict_risk_raster@extent@ymin)),predict_risk_raster@extent@ymax+0.01*(abs(predict_risk_raster@extent@ymin))),
      axes = F
)
#image(naband_reclass, col=col.na, add = T)
#plot(wus, add = T)
par(mai=c(0,0,0,0), mar=c(0.1,0.1,0.1,2.1)/3, pty = "m", family = "LM Roman 10")
# par(xpd=T)
plot(c(-10,20)-2,c(-0.1,5)-2, type = 'n', axes = F,xlab = '', ylab = '')
text(x=-1.5, y = c(0,0.5,1,1.2), labels = c(paste("<",round(min_absolute_value,digits=0), sep= " "),"0",round(max_absolute_value,digits=0),"log relative risk"))
graphics::rasterImage(image = legend_image, xleft = -7, ybottom = 0, xright = -5, ytop = 1)
#graphics::rasterImage(image = legend_image, xleft = 0, ybottom = 0, xright = 1, ytop = 0.57)
legend(
  -14,0,
  legend = "Indeterminate",
  fill = col.na,
  ncol = 1,
  bty="n"
)









# Create separate layer for NAs (if any)
naband$rr <- ifelse(is.na(naband$rr),9999, naband$rr)
coordinates(naband) <- ~ x + y # coordinates
gridded(naband) <- TRUE # gridded
NA_risk_raster <- raster(naband)
crs(NA_risk_raster) <- cref0
NA_risk_raster <- projectRaster(NA_risk_raster, crs = cref1, method = "ngb")
naband_reclass <- reclassify(NA_risk_raster, c(-Inf,9998,NA,
                                               9998,Inf,1))




