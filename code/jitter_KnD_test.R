## TEST OF jitterKnD

# packages
library(spatstat)
library(prism)
library(maps)
library(raster)

source("code/R_functions/jitter_KnD.R")
source("code/R_functions/jitter_lrren.R")

# Choice of coordinates reference system
crs_wgs84 <- "+proj=longlat +datum=WGS84"

# Random Data
data_df <- as.data.frame(dplyr::data_frame(
  id = seq(from = 1, to = 100, by = 1),
  lon = rnorm(100)*2 - 100,
  lat = rnorm(100)*2 + 35,
  mark = rbinom(100, 1, 0.5),
  level = rbinom(100,2, 0.33)
))

#data_df_original <- data_df_na
data_df_na <- data_df
data_df_na[101,] <- c(101, -101, 50, 0, 0)
data_df_na[102,] <- c(102, -100, 50, 0, 1)
data_df_na[103,] <- c(103, -99, 50, 0, 2)

# Get PRISM Raster Data
library(prism)
options(prism.path = "~/prismtmp")
get_prism_normals(type= "tmean"
                  ,resolution="4km"
                  ,annual = TRUE
                  ,keepZip=FALSE)
get_prism_normals(type= "ppt"
                  ,resolution="4km"
                  ,annual = TRUE
                  ,keepZip=FALSE)

# Convert to rasters
ppt <- ls_prism_data(absPath=T)[1,2]
ppt_rast <- raster(ppt)
proj4string(ppt_rast) <- crs(crs_wgs84)
tmean <- ls_prism_data(absPath=T)[4,2]
tmean_rast <- raster(tmean)
proj4string(tmean_rast) <- crs(crs_wgs84)
# Standardize rasters (for comparison and KnD test must have non-negative values)
ppt_rast_scaled <- (ppt_rast - abs(ppt_rast@data@min))/(ppt_rast@data@max - abs(ppt_rast@data@min))
tmean_rast_scaled <- (tmean_rast - abs(tmean_rast@data@min))/(tmean_rast@data@max - abs(tmean_rast@data@min))

# Mask rasters
# Removes NA values outside of mask (if any)
mask_poly <- Polygon(cbind(c(min(data_df$lon)-1,max(data_df$lon)+1,max(data_df$lon)+1,min(data_df$lon)-1), c(min(data_df$lat)-1,min(data_df$lat)-1,max(data_df$lat)+1,max(data_df$lat)+1)))
mask_poly <- SpatialPolygons(list(Polygons(list(mask_poly), "s1")))
ppt_rast_mask <- mask(ppt_rast_scaled, mask_poly)
tmean_rast_mask <- mask(tmean_rast_scaled, mask_poly)

# Stack rasters
rasterstack <- stack(ppt_rast_mask, tmean_rast_mask)

# matrix of distances for each level (levels x 2 matrix)
distances_mat <- matrix(data = c(500,0,2000,0,6000,0), nrow = 3, ncol = 2, byrow = T)

# Number of simulations
sim_num <- 10

## Function Run
# No NAs

test_jitterKnD <- jitterKnD(data = data_df, rasters = rasterstack, sim = sim_num, distances = distances_mat, crs = crs_wgs84)

# Yes NAs
#(slower because finding closest raster cell centroid to point and taking its value)
test_jitterKnD_na <- jitterKnD(data = data_df_na, rasters = rasterstack, sim = sim_num, distances = distances_mat, crs = crs_wgs84)

# Jittered Datasets
jittered <- test_jitterKnD_na[[2]]

## Results
plot(as.vector(rapply(lapply(test_jitterKnD,function(x) x[2]),c)), as.vector(rapply(lapply(test_jitterKnD,function(x) x[3]),c)), type = "n", xlab = "Longitude", ylab = "Latitude", main = "Random points with random jittering by set distance categories")
points(data_df$lon, data_df$lat, pch = 16)
points(as.vector(rapply(lapply(test_jitterKnD,function(x) x[2]),c)), as.vector(rapply(lapply(test_jitterKnD,function(x) x[3]),c)), col = "red")
maps::map("state", add = T)

plot(as.vector(rapply(lapply(jittered,function(x) x[2]),c)), as.vector(rapply(lapply(jittered,function(x) x[3]),c)), type = "n", xlab = "Longitude", ylab = "Latitude", main = "Random points with random jittering by set distance categories\nIncludes forced points outside of covariate extent")
points(data_df_na$lon, data_df_na$lat, pch = 16)
points(as.vector(rapply(lapply(jittered,function(x) x[2]),c)), as.vector(rapply(lapply(jittered,function(x) x[3]),c)), col = "red")
maps::map("state", add = T)


# Extract raster values for original data
data_df <- cbind(data_df, raster::extract(rasterstack,data_df[,2:3]))
data_df_na <- cbind(data_df_na, raster::extract(rasterstack,data_df_na[,2:3]))

plot(as.vector(rapply(lapply(test_jitterKnD,function(x) x[7]),c)), as.vector(rapply(lapply(test_jitterKnD,function(x) x[8]),c)), type = "n",  ylab = "tmean", xlab = "ppt", main = "Environmental Space of random points\nwith random jittering by set distance categories")
points(data_df$PRISM_ppt_30yr_normal_4kmM2_annual_bil, data_df$PRISM_tmean_30yr_normal_4kmM2_annual_bil,pch = 16)
points(as.vector(rapply(lapply(test_jitterKnD,function(x) x[7]),c)), as.vector(rapply(lapply(test_jitterKnD,function(x) x[8]),c)), col = "red")


plot(as.vector(rapply(lapply(jittered,function(x) x[6]),c)), as.vector(rapply(lapply(jittered,function(x) x[7]),c)), type = "n", ylab = "tmean", xlab = "ppt",main = "Environmental Space of random points with random jittering\nby set distance categories, Includes forced points outside of covariate extent")
points(data_df_na[,7:8], pch = 16)
points(as.vector(rapply(lapply(jittered,function(x) x[6]),c)), as.vector(rapply(lapply(jittered,function(x) x[7]),c)), col = "red")

# Save Data
save(test_jitterKnD, ppt_rast_mask, tmean_rast_mask, file = "jittered_data.RData")



# BUG IN data.frame()
# arguments imply differing number of rows:







## BUG IN NA RETRIEVAL

data_df_na

data_df_na$zone = (floor((data_df_na[,2] + 180)/6) %% 60) + 1
names(data_df_na) <- c("id", "lon", "lat", "mark", "levels", "zone")
# Split data by zone
df_list = split(data_df_na, as.factor(data_df_na$zone))
#crs = crs
datum = gsub('^.*datum=\\s*|\\s* .*$', '', crs_wgs84)

i <- NULL
coords_list <- NULL
for (i in 1:length(df_list)){
  zone = df_list[[i]]$zone[1]
  crs_utm = paste("+proj=utm +zone=",zone," +datum=",datum, sep = "")
  coordinates(df_list[[i]]) = ~lon+lat
  proj4string(df_list[[i]]) = CRS(crs_wgs84)
  sp_df = spTransform(df_list[[i]], CRS(crs_utm))
  coords_list[[i]] = as.data.frame(cbind(sp_df@coords, sp_df@data))
}

# Compile UTM coordinates
coords_utm = do.call(rbind, coords_list)

## Jitter points by multiple level categories
# Split data by level
df_level = split(coords_utm, as.factor(coords_utm$level))

jitter_list <- NULL
df_coords <- NULL
jitter_out <- NULL

for (j in 1:sim_num){
  for (k in 1:length(df_level)){
    jitter_max = distances_mat[,1] # extract maximum distance value
    jitter_min = distances_mat[,2] # extract minimum distance value

    df_jitter = jitter2d(df_level[[k]][,1:2], max = jitter_max[k], min = jitter_min[k])
    jitter_list[[k]] = cbind(df_jitter, df_level[[k]][,-c(1,2)]) # recompile data with new coordinates
  }
  jitter_df = do.call(rbind, jitter_list) # compile list into one data.frame per simulation

  # Convert from UTM to decimal degree
  jitter_zone = split(jitter_df, as.factor(jitter_df$zone)) # split list by zone

  for (i in 1:length(jitter_zone)){
    zone_utm = jitter_zone[[i]]$zone[1]
    crs_utm2 = paste("+proj=utm +zone=",zone_utm," +datum=",datum, sep = "")
    coordinates(jitter_zone[[i]]) = ~lon+lat
    proj4string(jitter_zone[[i]]) = CRS(crs_utm2)
    spdf2 = spTransform(jitter_zone[[i]], CRS(crs_wgs84))
    df_coords[[i]] = as.data.frame(cbind(spdf2@coords, spdf2@data))
  }

  coords_latlon = do.call(rbind, df_coords)
  #jitter_mat[[j]] = coords_latlon #store data.frames of each simulation into list

  ## Extract Covariate Values from Rasters
  extract_coords <- coords_latlon[,1:2]
  extract_raster <- raster::extract(rasterstack,extract_coords)

  df_extract <- cbind(coords_latlon[,3],coords_latlon[,-3], extract_raster) # recompile with data
  colnames(df_extract)[1] <- "id"
  df_extract = df_extract[order(df_extract$id),]

  ######THE FOLLOWING WORKS#####
  # coords_latlon = do.call(rbind, df_coords)
  # jitter_mat[[j]] = coords_latlon #store data.frames of each simulation into list
  #
  # ## Extract Covariate Values from Rasters
  # extract_coords <- jitter_mat[[j]][,1:2]
  # extract_raster <- raster::extract(rasters,extract_coords)
  #
  # df_extract <- cbind(jitter_mat[[j]][,3],jitter_mat[[j]][,-3], extract_raster) # recompile with data
  # colnames(df_extract)[1] <- "id"
  ######THE ABOVE WORKS#####

  #
  # # #### ADDED TO ENSURE POINTS OUTSIDE OF RASTER GET CLOSEST CELL VALUE ####
  # if(any(is.na(extract_raster))){
  #   #if(class(rasters) == "RasterStack"){
  #
  #   extract_coords_na_df <- df_extract[!complete.cases(df_extract),-c(1,2,3)]
  #   xy <- extract_coords_na_df[,2:3]
  #   sampled_raster_list <- purrr::map(.x = rasters@layers, .f = sample_raster_NA, xy=xy)
  #
  #   sampled_raster <- data.frame(id = as.numeric(unlist(Map(names, sampled_raster_list))), value = unlist(sampled_raster_list), layer =
  #                                  rep(unlist(lapply(rasters@layers, names)), lengths(sampled_raster_list)))
  #
  #   sampled_raster <- tidyr::spread(sampled_raster, layer, value)
  #
  #   ### REPLACE NA values in df_extract with values from sampled_raster
  #   # Match by column and id
  #
  #   for(j in 1:ncol(df_extract)){
  #     rows <- as.vector(which(is.na(df_extract[,j])))
  #   }
  #   for(i in 1:nrow(df_extract)){
  #     columns <- as.vector(which(apply(df_extract[i,],2,is.na)))
  #   }
  #   df_extract[rows,columns] = sampled_raster[,-1]
  # }

  # else #{if(class(rasters) != "RasterStack")
  #   {
  #
  #   # Which coordinates are missing values
  #   extract_coords_na_df <- df_extract[!complete.cases(df_extract),1:3]
  #   xy <- extract_coords_na_df[,2:3]
  #
  #   # Extract raster value at closet raster cell centroid
  #   sampled_raster <- apply(X = xy , MARGIN = 1, FUN = function(xy) rasters@data@values[which.min(replace(distanceFromPoints(rasters, xy), is.na(rasters), NA))])
  #   # Convert to data.frame with ids
  #   sampled_raster <- as.data.frame(dplyr::data_frame(id = extract_coords_na_df[,1], r = as.vector(sampled_raster)))
  #   colnames(sampled_raster)[2] <- rasters@data@names #rename with raster name
  #   # Find common columns between dataframes
  #   col_index <- which(colnames(df_extract)== colnames(sampled_raster)[2])
  #
  #   # Match ids and replace NAs with closest raster cell centroid
  #   df_extract[,col_index][is.na(df_extract[,col_index])] <- sampled_raster[,2][match(df_extract$id[is.na(df_extract[,col_index])],sampled_raster$id)]
  # }

  # }

  if(any(is.na(extract_raster))){

    extract_coords_na_df <- df_extract[!complete.cases(df_extract),2:3]
    #xy <- extract_coords_na_df[,2:3]
    sampled_raster_list <- purrr::map(.x = rasterstack@layers, .f = sample_raster_NA, xy=extract_coords_na_df)

    sampled_raster <- data.frame(id = as.numeric(unlist(Map(names, sampled_raster_list))), value = unlist(sampled_raster_list), layer = rep(unlist(lapply(rasterstack@layers, names)), lengths(sampled_raster_list)))

    sampled_raster <- tidyr::spread(sampled_raster, layer, value)

    # Replace NA values in original dataset with nearest raster cell value
    merge_df <- df_extract %>%
      dplyr::mutate(id = as.character(id)) %>%
      gather(key = "col", value = "val", -id) %>%
      left_join(sampled_raster %>%
                  mutate(id = as.character(id)) %>%
                  gather(key = "col", value = "val", -id),
                by =c("id", "col")) %>%
      transmute(id, col, val = ifelse(is.na(val.x), val.y, val.x)) %>%
      spread(col, val) %>% arrange(as.numeric(id))

    old_names <- colnames(df_extract) #column names of original
    merge_df <- merge_df[old_names] #sort columns by original
    merge_df$zone <- NULL

    jitter_out[[j]] <- merge_df # Output
  }
  else {
    df_extract$zone <- NULL
    jitter_out[[j]] <- df_extract} #If no NA values (much faster)
}







### MISCELLANEOUS



plot(data_df$PRISM_ppt_30yr_normal_4kmM2_annual_bil, data_df$PRISM_tmean_30yr_normal_4kmM2_annual_bil, ylab = "tmean", xlab = "ppt",pch = 16)
points(test_jitterKnD[[1]]$PRISM_ppt_30yr_normal_4kmM2_annual_bil, test_jitterKnD[[1]]$PRISM_tmean_30yr_normal_4kmM2_annual_bil, col = "black")
points(test_jitterKnD[[2]]$PRISM_ppt_30yr_normal_4kmM2_annual_bil, test_jitterKnD[[2]]$PRISM_tmean_30yr_normal_4kmM2_annual_bil, col = "red")
points(test_jitterKnD[[3]]$PRISM_ppt_30yr_normal_4kmM2_annual_bil, test_jitterKnD[[3]]$PRISM_tmean_30yr_normal_4kmM2_annual_bil, col = "blue")



## PC

test_jitterKnD <- NULL
test_jitterKnD <- jitterKnD(data = data_df, rasters = pc2[[1:2]], sim = 100, distances = distances_mat, crs = crs_wgs84)

head(test_jitterKnD[[1]])

data_df_raw <- cbind(data_df, raster::extract(pc2[[1:2]],data_df[,2:3]))

plot(data_df_raw$PC1, data_df_raw$PC2, ylab = "PC2", xlab = "PC1",pch = 16)
points(as.vector(rapply(lapply(test_jitterKnD1,function(x) x[7]),c)), as.vector(rapply(lapply(test_jitterKnD,function(x) x[8]),c)), col = "red")











test1_jitterKnD <- jitterKnD(data = data_df, rasters = pc2[[1:2]], sim = 10, distances = distances_mat, crs = crs_wgs84)

subset(test1_jitterKnD[[1]], is.na(PC1))

## TEST OF NA values in points outside raster

# Add a 101th value outside of raster


extract_coords_test[101,1] <- -100
extract_coords_test[101,2] <- 50

extract_coords_test[102,1] <- -99
extract_coords_test[102,2] <- 50

extract_coords_test[103,1] <- -101
extract_coords_test[103,2] <- 50

extract_coords_test <- cbind(data_df$lon, data_df$lat)

extract_raster <- raster::extract(rasters,extract_coords_test)
extract_df <- cbind(data_df[,1:3], extract_raster)


# Subset out these coordinates

extract_coords_na_df <- extract_df[!complete.cases(extract_df),1:3]
xy <- extract_coords_na_df[,2:3]

sampled_raster <- apply(X = xy , MARGIN = 1, FUN = function(xy) rasters[[1]]@data@values[which.min(replace(distanceFromPoints(rasters[[1]], xy), is.na(rasters[[1]]), NA))])

sampled_raster <- as.data.frame(dplyr::data_frame(id = extract_coords_na_df[,1], r = as.vector(sampled_raster)))
colnames(sampled_raster)[2] <- rasters[[1]]@data@names

col_index <- which(colnames(extract_df)== colnames(sampled_raster)[2])

extract_df[,col_index][is.na(extract_df[,col_index])] <- sampled_raster[,2][match(extract_df$id[is.na(extract_df[,col_index])],sampled_raster$id)]




as.vector(sampled_raster) == extract_raster


str(rasters@layers@data@values)

rasters@layers

sample_raster_NA <- function(r, xy){
  apply(X = xy, MARGIN = 1,
        FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
}





## TEST NA REMOVAL
head(test_jitterKnD_na[[1]])
extract_test <- test_jitterKnD_na[[1]]

extract_coords_na_df <- extract_test[!complete.cases(extract_test),2:3]
#xy <- extract_coords_na_df[,1:2]
sampled_raster_list <- purrr::map(.x = rasterstack@layers, .f = sample_raster_NA, xy=extract_coords_na_df)


sampled_raster <- data.frame(id = as.numeric(unlist(Map(names, sampled_raster_list))), value = unlist(sampled_raster_list), layer =
                               rep(unlist(lapply(rasterstack@layers, names)), times = lengths(sampled_raster_list)))

sampled_raster <- tidyr::spread(sampled_raster, layer, value)

### REPLACE NA values in df_extract with values from sampled_raster
# Match by column and id


extract_test
sampled_raster

merge_test <- extract_test %>%
  mutate(id = as.character(id)) %>%
  gather(key = "col", value = "val", -id) %>%
  left_join(sampled_raster %>%
              mutate(id = as.character(id)) %>%
              gather(key = "col", value = "val", -id),
            by =c("id", "col")) %>%
  transmute(id, col, val = ifelse(is.na(val.x), val.y, val.x)) %>%
  spread(col, val) %>% arrange(as.numeric(id))

old_names <- colnames(extract_test)
merge_test <- merge_test[old_names]











merge_test <- dplyr::left_join(extract_test, sampled_raster, by = "id", all.y = T)
head(merge_test)

pat <- c("PC1", "PC2")
cbind(merge_test[rep(1:nrow(merge_test), 2), 1:6], do.call(cbind, lapply(pat,
                                                         function(nm) melt(merge_test[grep(nm, names(merge_test))]))))

grep(pat, names(merge_test))



  merge_test <- plyr::rbind.fill(extract_test,sampled_raster)



  sub('\\..*', '', colnames(merge_test))


 %>% # this will generate age.x and age.y
  mutate(age = ifelse(is.na(age.x), age.y, age.x)) %>% # we generate a joint 'age' variable
  select(-age.y, -age.x) # drop the superfluous columns




extract_test %>% dplyr::left_join(sampled_raster, by = "id")
#%>% dplyr::select(id)


extract_test %

for(l in 1:ncol(extract_test)){
  rows <- as.vector(which(is.na(extract_test[,l])))
}


which(is.na(extract_test), arr.ind = T)

m <- 100
for(m in 1:nrow(extract_test)){
  columns <- which(is.na(extract_test[m,]))
}

columns = c(7,8)

extract_test[rows,c(1,columns)] = sampled_raster[,-1]
jitter_out[[j]] <- df_extract
}





##########  DEBUG PURRR ISSUE

dat <- obs_dat
rasters <- rasterstack
sim <- sim_num
distances <- distances_mat
cref <- cref0



require(raster)
require(sp)
require(geoR)
require(rgdal)
require(purrr)
require(dplyr)
require(tidyr)
coords_list <- NULL
jitter_list <- NULL
df_coords <- NULL
jitter_out <- NULL

# if(unit = "degree"){
# coords <- sp::coordinates(cbind(dat$lon, dat$lat))
# }
if(!is.matrix(distances)){stop("Distances must be a matrix")}
#if(unit !="km"){stop("Units must be in kilometers (km)")}
## Convert lon/lat coordinates to UTM
# Get the UTM zone for a given longitude
dat$zone = (floor((dat[,2] + 180)/6) %% 60) + 1
names(dat) <- c("id", "lon", "lat", "mark", "levels", "zone")
# Split data by zone
df_list = split(dat, as.factor(dat$zone))
cref = cref
datum = gsub('^.*datum=\\s*|\\s* .*$', '', cref)

# Convert to correct UTM if multiple zones
for (i in 1:length(df_list)){
  zone = df_list[[i]]$zone[1]
  crs_utm = paste("+proj=utm +zone=",zone," +datum=",datum, sep = "")
  coordinates(df_list[[i]]) = ~lon+lat
  proj4string(df_list[[i]]) = CRS(cref)
  sp_df = spTransform(df_list[[i]], CRS(crs_utm))
  coords_list[[i]] = as.data.frame(cbind(sp_df@coords, sp_df@data))
}
# Compile UTM coordinates
coords_utm = do.call(rbind, coords_list)

## Jitter points by multiple level categories
# Split data by level
df_level = split(coords_utm, as.factor(coords_utm$level))

# Jitter randomly and iteratively
for (j in 1:sim){
  for (k in 1:length(df_level)){
    jitter_max = distances[,1] # extract maximum distance value
    jitter_min = distances[,2] # extract minimum distance value

    df_jitter = jitter2d(df_level[[k]][,1:2], max = jitter_max[k], min = jitter_min[k])
    jitter_list[[k]] = cbind(df_jitter, df_level[[k]][,-c(1,2)]) # recompile data with new coordinates
  }
  jitter_df = do.call(rbind, jitter_list) # compile list into one data.frame per simulation

  # Convert from UTM to decimal degree
  jitter_zone = split(jitter_df, as.factor(jitter_df$zone)) # split list by zone

  for (i in 1:length(jitter_zone)){
    zone_utm = jitter_zone[[i]]$zone[1]
    crs_utm2 = paste("+proj=utm +zone=",zone_utm," +datum=",datum, sep = "")
    coordinates(jitter_zone[[i]]) = ~lon+lat
    proj4string(jitter_zone[[i]]) = CRS(crs_utm2)
    spdf2 = spTransform(jitter_zone[[i]], CRS(cref))
    df_coords[[i]] = as.data.frame(cbind(spdf2@coords, spdf2@data))
  }

  coords_latlon = do.call(rbind, df_coords)
  #jitter_mat[[j]] = coords_latlon #store data.frames of each simulation into list

  ## Extract Covariate Values from Rasters
  extract_coords <- coords_latlon[,1:2]
  extract_raster <- raster::extract(rasters,extract_coords)

  df_extract <- cbind(coords_latlon[,3],coords_latlon[,-3], extract_raster) # recompile with data
  colnames(df_extract)[1] <- "id"
  df_extract = df_extract[order(df_extract$id),]


  df_extract$zone <- NULL
  jitter_out[[j]] <- df_extract
} #If no NA values (much faster)

table(is.na(jitter_out[[1]]))

jitter_test <- jitter_out[[1]]

sample_raster_NA <- function(r, xy){
  apply(X = xy, MARGIN = 1,
        FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
}

extract_coords_na_df <- jitter_test[!complete.cases(jitter_test),2:3]
rownames(extract_coords_na_df) <- jitter_test[!complete.cases(jitter_test),1]
#xy <- extract_coords_na_df[,2:3]
sampled_raster_list <- purrr::map(.x = rasters@layers, .f = sample_raster_NA, xy=extract_coords_na_df)


plot(ca)
plot(rasters)
points(extract_coords_na_df)


str(sampled_raster_list)



str(rasters@layers)


sampled_raster <- data.frame(id = as.numeric(unlist(Map(names, sampled_raster_list))), value = as.numeric(unlist(sampled_raster_list)), layer = rep(unlist(lapply(rasters@layers, names)), lengths(sampled_raster_list)))

sampled_raster <- tidyr::spread(sampled_raster, layer, value)

# Replace NA values in original dataset with nearest raster cell value
merge_df <- df_extract %>%
  dplyr::mutate(id = as.character(id)) %>%
  gather(key = "col", value = "val", -id) %>%
  left_join(sampled_raster %>%
              mutate(id = as.character(id)) %>%
              gather(key = "col", value = "val", -id),
            by =c("id", "col")) %>%
  transmute(id, col, val = ifelse(is.na(val.x), val.y, val.x)) %>%
  spread(col, val) %>% arrange(as.numeric(id))

old_names <- colnames(df_extract) #column names of original
merge_df <- merge_df[old_names] #sort columns by original
merge_df$zone <- NULL

jitter_out[[j]] <- merge_df # Output



if(any(is.na(extract_raster))){

  extract_coords_na_df <- df_extract[!complete.cases(df_extract),2:3]
  rownames(extract_coords_na_df) <- df_extract[!complete.cases(df_extract),1]
  #xy <- extract_coords_na_df[,2:3]
  sampled_raster_list <- purrr::map(.x = rasters@layers, .f = sample_raster_NA, xy=extract_coords_na_df)

  sampled_raster <- data.frame(id = as.numeric(unlist(Map(names, sampled_raster_list))), value = as.numeric(unlist(sampled_raster_list)), layer = rep(unlist(lapply(rasters@layers, names)), lengths(sampled_raster_list)))

  sampled_raster <- tidyr::spread(sampled_raster, layer, value)

  # Replace NA values in original dataset with nearest raster cell value
  merge_df <- df_extract %>%
    dplyr::mutate(id = as.character(id)) %>%
    gather(key = "col", value = "val", -id) %>%
    left_join(sampled_raster %>%
                mutate(id = as.character(id)) %>%
                gather(key = "col", value = "val", -id),
              by =c("id", "col")) %>%
    transmute(id, col, val = ifelse(is.na(val.x), val.y, val.x)) %>%
    spread(col, val) %>% arrange(as.numeric(id))

  old_names <- colnames(df_extract) #column names of original
  merge_df <- merge_df[old_names] #sort columns by original
  merge_df$zone <- NULL

  jitter_out[[j]] <- merge_df # Output
}
else {

}

# Original Dataset (Observed)

## Extract Covariate Values from Rasters
extract_coords <- dat[,2:3]
df_extract <-  data.frame(dat, raster::extract(rasters,extract_coords))
df_extract <- df_extract[order(df_extract$id),]
df_extract$id <- as.character(df_extract$id)
df_extract$zone <- NULL

## Output
# jitterKnD_output <- jitter_out
jitterKnD_output <- list("obs_dat" = df_extract,
                         "sim_dat" = jitter_out

)


####################
