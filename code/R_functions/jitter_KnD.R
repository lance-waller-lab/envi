# Relative Risk Ratio Function
#
# Authors: Lance Waller & Ian Buller
# Created: June 2018
#
# Jitters locations based on set distances and categories for Monte-Carlo Simulation
#
#

# Data structure
# data = data.frame of 5 columns (id, lon, lat, mark, level)
# rasters = raster or rasterstack of covariates
# distances = vector of maximum distances per level

jitterKnD <- function(data, rasters, distances, crs, sim){

  sample_raster_NA <- function(r, xy){
    apply(X = xy, MARGIN = 1,
          FUN = function(xy) r@data@values[which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))])
  }

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
# coords <- sp::coordinates(cbind(data$lon, data$lat))
# }
if(!is.matrix(distances)){stop("Distances must be a matrix")}
#if(unit !="km"){stop("Units must be in kilometers (km)")}
  ## Convert lon/lat coordinates to UTM
  # Get the UTM zone for a given longitude
  data$zone = (floor((data[,2] + 180)/6) %% 60) + 1
  names(data) <- c("id", "lon", "lat", "mark", "levels", "zone")
  # Split data by zone
  df_list = split(data, as.factor(data$zone))
  crs = crs
  datum = gsub('^.*datum=\\s*|\\s* .*$', '', crs)

  # Convert to correct UTM if multiple zones
  for (i in 1:length(df_list)){
    zone = df_list[[i]]$zone[1]
    crs_utm = paste("+proj=utm +zone=",zone," +datum=",datum, sep = "")
    coordinates(df_list[[i]]) = ~lon+lat
    proj4string(df_list[[i]]) = CRS(crs)
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
      spdf2 = spTransform(jitter_zone[[i]], CRS(crs))
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
      sampled_raster_list <- purrr::map(.x = rasters@layers, .f = sample_raster_NA, xy=extract_coords_na_df)

      sampled_raster <- data.frame(id = as.numeric(unlist(Map(names, sampled_raster_list))), value = unlist(sampled_raster_list), layer = rep(unlist(lapply(rasters@layers, names)), lengths(sampled_raster_list)))

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
    else {jitter_out[[j]] <- df_extract} #If no NA values (much faster)
  }

  # Original Dataset (Observed)

  ## Extract Covariate Values from Rasters
  extract_coords <- data[,2:3]
  df_extract <-  data.frame(data, raster::extract(rasters,extract_coords))
  df_extract <- df_extract[order(df_extract$id),]
  df_extract$id <- as.character(df_extract$id)
  df_extract$zone <- NULL

  ## Output
  # jitterKnD_output <- jitter_out
  jitterKnD_output <- list("obs_dat" = df_extract,
                           "sim_dat" = jitter_out

  )

}


