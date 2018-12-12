# Relative Risk Ratio Function
# Jitted datasets
# Assess mean and standard deviation
#
# Authors: Lance Waller & Ian Buller
# Created: August 2018


# debug(utils::unpackPkgZip)
# install.packages("sparr")
# install.packages("concaveman")
# install.packages("sp")
# install.packages("rgeos")
# install.packages("dplyr")
# install.packages("spatstat")
# install.packages("raster")
# install.packages("foreach")

require("sparr")
require("concaveman")
require("sp")
require("rgeos")
require("dplyr")
require("spatstat")
require("raster")
require("foreach")
require("geoR")

# Data structure
# case_locs = data.frame of 4 variables (lon, lat, pc1, pc2)
# control_locs = data.frame of 4 variables (lon, lat, pc1, pc2)
# extract_locs = data.frame of 4 variables (lon, lat, pc1, pc2)

jitter_llren <- function(sim_locs, predict_locs,
                         #bandw=NULL, sim=NULL,quant=NULL,grid_number=NULL,
                         #cv=FALSE,
                         predict=TRUE,
                         #nfold=NULL,
                         poly_buffer=NULL,
                         #parallel=FALSE, balance = FALSE,
                         #conserve=TRUE,
                         plot=FALSE,
                         plot.cols=NULL,...) {

  require("sparr")
  require("concaveman")
  require("sp")
  require("rgeos")
  require("spatstat")
  require("raster")

  ## Set colors for plotting
  if(is.null(plot.cols)){
    plot.cols <- c("yellow","blue3", "cornflowerblue", "grey80", "indianred1", "indianred4")
  } else {plot.cols <- plot.cols
  }
  col.na <- plot.cols[1]
  col.99 <- plot.cols[2]
  col.95 <- plot.cols[3]
  col.50 <- plot.cols[4]
  col.05 <- plot.cols[5]
  col.01 <- plot.cols[6]

  ## Set function used in foreach
  `%fun%` <- `%do%`
  # Combine function used in foreach
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }

  ## Calculate boundary polygons
  # Calculate outer boundary polygon (full extent of geographical extent in environmental space)
  outer_chull <- concaveman::concaveman(cbind(predict_locs[,3], predict_locs[,4]))
  outer_chull_pts <- sp::coordinates(outer_chull)
  outer_chull_pts <- rbind(outer_chull_pts, outer_chull_pts[1,]) #repeats the first coordinate to complete the polygon
  outer_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(outer_chull_pts)),1))) # convert points to polygon
  if(is.null(poly_buffer)){
    poly_buffer <- max(outer_chull_pts)/100
  } else {poly_buffer <- poly_buffer
  }
  outer_chull_poly_buffer <- rgeos::gBuffer(outer_chull_poly, width=poly_buffer, byid=TRUE) #add small buffer around polygon to include boundary points
  outer_poly <- outer_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon

  out_par <- foreach::foreach(k=1:length(sim_locs), .combine = comb, .multicombine = TRUE, .packages = c("sparr", "spatstat", "dplyr", "raster")
                        , .init = list(list(), list(), list(), list())
     ) %fun% {

    #cat("Estimating jittered relative risk",k,"of",length(sim_locs),"simulations\n")
    case_locs = subset(sim_locs[[k]], sim_locs[[k]]$mark == 1)
    control_locs = subset(sim_locs[[k]], sim_locs[[k]]$mark == 0)

    ## Calculate Bandwidth or user specified value
      ## Input Preparation
      # case and control point pattern datasets
      ppp_case <- spatstat::ppp(case_locs[,6], case_locs[,7], window = owin(poly = list(x=rev(outer_poly[,1]),y=rev(outer_poly[,2]))))
      ppp_control <- spatstat::ppp(control_locs[,6], control_locs[,7], window = owin(poly = list(x=rev(outer_poly[,1]),y=rev(outer_poly[,2]))))


  ## Calculate observed kernel density ratio
  jitter_lrr <- sparr::risk(f=ppp_case, g=ppp_control,...)
  #cat("Case Bandwidth =", jitter_lrr$f$h0, "\n")
  #cat("Control Bandwidth =", jitter_lrr$g$h0, "\n")

  rx <- rep(jitter_lrr$rr$xcol, length(jitter_lrr$rr$yrow))
  for(i in 1:length(jitter_lrr$rr$yrow)){
    if (i == 1){
      ry <- rep(jitter_lrr$rr$yrow[i], length(jitter_lrr$rr$xcol))
    }
    if (i != 1){
      ry <- c(ry, rep(jitter_lrr$rr$yrow[i], length(jitter_lrr$rr$xcol)))
    }
  }

 if(k == 1){
        sim_risk = as.vector(t(jitter_lrr$rr$v))
        sim_pval = as.vector(t(jitter_lrr$P$v))
 } else{
        rx <- NULL
        ry <- NULL
        sim_risk = as.vector(t(jitter_lrr$rr$v))
        sim_pval = as.vector(t(jitter_lrr$P$v))
  }

  # Output for each n-fold
  par_results <- list("sim_risk" = sim_risk,
                      "sim_pval" = sim_pval,
                      "rx" = rx,
                      "ry" = ry
  )
  par_results <- list(sim_risk,
                      sim_pval,
                      rx,
                      ry
  )
  return(par_results)
}
  #cat("Calculating summary statistics in predictor space\n")
  jitter_rr <- out_par[[1]]
  jitter_pval <- out_par[[2]]
  rx <- out_par[[3]][[1]]
  ry <- out_par[[4]][[1]]

  # jitter_results <- list("jitter_rr" = jitter_rr,
  #                        "jitter_pval" = jitter_pval
  # )

  rr_mean <- rowMeans(do.call(cbind, jitter_rr), na.rm = TRUE)
  pval_mean <-  rowMeans(do.call(cbind, jitter_pval), na.rm = TRUE)
  rr_sd <- apply(do.call(cbind,jitter_rr),1,sd, na.rm = TRUE)
  pval_sd <- apply(do.call(cbind,jitter_pval),1,sd, na.rm = TRUE)

  # Convert to mean relative risk raster
  rr <- as.data.frame(dplyr::data_frame(
    x = rx,
    y = ry,
    rr = rr_mean
  ))
  lrr_narm <- na.omit(rr) # remove NAs
  coordinates(lrr_narm) <- ~ x + y # coordinates
  gridded(lrr_narm) <- TRUE # gridded
  rr_raster <- raster(lrr_narm)

  # Convert to mean p-value raster
  pval <- as.data.frame(dplyr::data_frame(
    x = rx,
    y = ry,
    tol = pval_mean
  ))
  lrr_narm <- na.omit(pval) # remove NAs
  coordinates(lrr_narm) <- ~ x + y # coordinates
  gridded(lrr_narm) <- TRUE # gridded
  pval_raster <- raster(lrr_narm)

  # Convert to standard deviation relative risk raster
  rrsd <- as.data.frame(dplyr::data_frame(
    x = rx,
    y = ry,
    sd = rr_sd
  ))
  lrr_narm <- na.omit(rrsd) # remove NAs
  coordinates(lrr_narm) <- ~ x + y # coordinates
  gridded(lrr_narm) <- TRUE # gridded
  rrsd_raster <- raster(lrr_narm)

  # Convert to standard deviation relative risk raster
  pvalsd <- as.data.frame(dplyr::data_frame(
    x = rx,
    y = ry,
    sd = pval_sd
  ))
  lrr_narm <- na.omit(pvalsd) # remove NAs
  coordinates(lrr_narm) <- ~ x + y # coordinates
  gridded(lrr_narm) <- TRUE # gridded
  pvalsd_raster <- raster(lrr_narm)

  out_sim <- list("rr_mean"=rr_mean,
                  "pval_mean" = pval_mean,
                  "rr_sd" = rr_sd,
                  "pval_sd" = pval_sd,
                  "rx" = rx,
                  "ry" = ry,
                  "outer_poly" = outer_poly
                  )

  if(plot == FALSE){}else{

    # Conservative for non finite values
    rr_raster@data@values <- ifelse(rr_raster@data@values == -Inf, 0, rr_raster@data@values)
    rr_raster@data@values <- ifelse(rr_raster@data@values == Inf, 0, rr_raster@data@values)

    pval_raster@data@values <- ifelse(pval_raster@data@values == -Inf, 0.5, pval_raster@data@values)
    pval_raster@data@values <- ifelse(pval_raster@data@values == Inf, 0.5, pval_raster@data@values)

    # Reclassify for alpha levels
    reclass_p <- raster::reclassify(pval_raster, c(-Inf,0.005,5,
                                                   0.005,0.025,4,
                                                   0.025,0.975,3,
                                                   0.975,0.995,2,
                                                   0.995,Inf,1
    ))

    reclass_pval_sd <- pvalsd_raster
    reclass_pval_sd@data@values <- ifelse(reclass_pval_sd@data@values < 0.01, NA, reclass_pval_sd@data@values)

    # Mean relative risk
    upperhalf = length(rr_raster@data@values[rr_raster@data@values>0 & !is.na(rr_raster@data@values)])
    lowerhalf = length(rr_raster@data@values[rr_raster@data@values<0 & !is.na(rr_raster@data@values)])

    max_absolute_value=max(rr_raster@data@values[is.finite(rr_raster@data@values)], na.rm = T) #what is the maximum absolute value of raster?
    min_absolute_value=min(rr_raster@data@values[is.finite(rr_raster@data@values)], na.rm = T) #what is the maximum absolute value of raster?
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
    nhalf = length(rr_raster@data@values)/2
    rclegend = colorRampPalette(colors = c(col.99,col.50, col.01), space="Lab")(nhalf)
    rclegend[c(nhalf,nhalf+1)] = rgb(t(col2rgb("white")), maxColorValue=256)

    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(rr_raster, col = rampcols, breaks=rampbreaks,
          main = "Mean relative risk of cases versus controls\nin predictor space",
          xlab = "Predictor A", ylab = "Predictor B", xlim = c(rr_raster@extent@xmin-0.01*(abs(rr_raster@extent@xmin)),rr_raster@extent@xmax+0.01*(abs(rr_raster@extent@xmin))), ylim = c(rr_raster@extent@ymin-0.01*(abs(rr_raster@extent@ymin)),rr_raster@extent@ymax+0.01*(abs(rr_raster@extent@ymin))))
    polygon(outer_poly)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
    legend_image <- as.raster(matrix(rev(rclegend)), ncol=1)
    plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=3, y = c(0,0.5,1,1.2), labels = c("< 0","0","> 0","Relative Risk"))
    rasterImage(legend_image, 0, 0, 1,1, main = "Relative Risk")
    legend(
      "bottom",
      legend = "Extent of prediction area",
      col = "black",
      lty = 1,
      ncol = 1,
      bty="n"
    )

    # SD of RR
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(rrsd_raster,  main = "Standard deviation of predicted relative risk of cases and controls\nin predictor space", col=rev(terrain.colors(256)),
          xlab = "Predictor A", ylab = "Predictor B"
    )
    polygon(outer_poly)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
    legend_image <- as.raster(matrix(terrain.colors(256)), ncol=1)
    plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=3.5, y = c(0,1,1.2), labels = c(paste(round(min(rrsd_raster@data@values, na.rm=T), digits = 2), sep=""),paste(round(max(rrsd_raster@data@values, na.rm=T), digits = 2), sep=""), "Standard Deviation"))
    rasterImage(legend_image, 0, 0, 1,1, main = "")
    legend(
      "bottom",
      legend = "Extent of prediction area",
      col = "black",
      lty = 1,
      ncol = 1,
      bty="n"
    )

    # Mean tolerance
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(reclass_p, col = c(col.99, col.95,col.50, col.05, col.01),
          xlab = "Predictor A", ylab = "Predictor B", main = "Mean predicted significant p-values\nin predictor space",
          xlim = c(reclass_p@extent@xmin-0.01*(abs(reclass_p@extent@xmin)),reclass_p@extent@xmax+0.01*(abs(reclass_p@extent@xmin))), ylim = c(reclass_p@extent@ymin-0.01*(abs(reclass_p@extent@ymin)),reclass_p@extent@ymax+0.01*(abs(reclass_p@extent@ymin))))
    polygon(outer_poly)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1), pty = "m")
    plot.new()
    legend(
      "bottomleft", title = "Legend", bty = "n",
      legend = c("p-value < 0.01", "p-value < 0.05", "Insignficant", "p-value > 0.95", "p-value < 0.99"),
      fill = c(
        col.01,
        col.05,
        col.50,
        col.95,
        col.99
      ),
      ncol = 1
    )

    # SD of tolerance
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(reclass_pval_sd,  main = "Standard deviation of predicted significant p-values\nin predictor space", col=rev(terrain.colors(256)),
          xlab = "Predictor A", ylab = "Predictor B"
    )
    polygon(outer_poly)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
    legend_image <- as.raster(matrix(terrain.colors(256)), ncol=1)
    plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=3.5, y = c(0,1,1.2), labels = c(paste(round(min(reclass_pval_sd@data@values, na.rm=T), digits = 2), sep=""),paste(round(max(reclass_pval_sd@data@values, na.rm=T), digits = 2), sep=""), "Standard Deviation"))
    rasterImage(legend_image, 0, 0, 1,1, main = "")
    legend(
      "bottom",
      legend = "Extent of prediction area",
      col = "black",
      lty = 1,
      ncol = 1,
      bty="n"
    )
  } # end of plot = T

  if(predict == FALSE){
    out_pred <- NULL
  }
  else{
  #cat("Predicting summary statistics in geographic space\n")

  ## Predict testing dataset
  extract_predict <- predict_locs[,3:4]

  # Extract to prediction locations
  mean_rr <- raster::extract(rr_raster, extract_predict)
  mean_pval <- raster::extract(pval_raster, extract_predict)
  sd_rr <- raster::extract(rrsd_raster, extract_predict)
  sd_pval <- raster::extract(pvalsd_raster, extract_predict)


  out_pred <- list("mean_rr" = mean_rr,
                   "mean_pval" = mean_pval,
                   "sd_rr" = sd_rr,
                   "sd_pval" = sd_pval,
                   "x" = predict_locs[,1],
                   "y" = predict_locs[,2]
   )

  if(plot == FALSE){}else{
    #cat("Creating visualizations of results\n")

    ## Mean Relative Risk
    avg_rr <- as.data.frame(dplyr::data_frame(
      x = predict_locs[,1],
      y =  predict_locs[,2],
      rr = mean_rr
    ))
    avg_rr$rr <- ifelse(avg_rr$rr == -Inf, 0, avg_rr$rr)
    avg_rr$rr <- ifelse(avg_rr$rr == Inf, 0, avg_rr$rr)
    naband <- avg_rr # save for next step
    lrr_narm <- na.omit(avg_rr) # remove NAs
    coordinates(lrr_narm) <- ~ x + y # coordinates
    gridded(lrr_narm) <- TRUE # gridded
    mean_rr_raster <- raster(lrr_narm)

    # Create separate layer for NAs (if any)
    naband$rr <- ifelse(is.na(naband$rr),9999, naband$rr)
    coordinates(naband) <- ~ x + y # coordinates
    gridded(naband) <- TRUE # gridded
    NA_risk_raster <- raster(naband)
    naband_reclass <- reclassify(NA_risk_raster, c(-Inf,9998,NA,
                                                   9998,Inf,1))

    ## Mean tolerance
    avg_pval <- as.data.frame(dplyr::data_frame(
      x = predict_locs[,1],
      y =  predict_locs[,2],
      tol = mean_pval
    ))
    naband_p <- avg_pval # save for next step
    lrr_narm <- na.omit(avg_pval) # remove NAs
    coordinates(lrr_narm) <- ~ x + y # coordinates
    gridded(lrr_narm) <- TRUE # gridded
    mean_pval_raster <- raster(lrr_narm)

    # Create separate layer for NAs (if any)
    naband_p$tol <- ifelse(is.na(naband_p$tol),9999, naband_p$tol)
    coordinates(naband_p) <- ~ x + y # coordinates
    gridded(naband_p) <- TRUE # gridded
    NA_p_raster <- raster(naband_p)
    naband_p_reclass <- reclassify(NA_p_raster, c(-Inf,9998,NA,
                                                    9998,Inf,1))

    # Reclassify for alpha ranges
    reclass_tol <- raster::reclassify(mean_pval_raster, c(-Inf,0.005,5,
                                                          0.005,0.025,4,
                                                          0.025,0.975,3,
                                                          0.975,0.995,2,
                                                          0.995,Inf,1
    ))

    # Standard deviation fo RR
    dev_rr <- as.data.frame(dplyr::data_frame(
      x = predict_locs[,1],
      y =  predict_locs[,2],
      sd = sd_rr
    ))
    naband_sd <- dev_rr
    lrr_narm <- na.omit(dev_rr) # remove NAs
    coordinates(lrr_narm) <- ~ x + y # coordinates
    gridded(lrr_narm) <- TRUE # gridded
    sd_rr_raster <- raster(lrr_narm)

    # Create separate layer for NAs (if any)
    naband_sd$sd <- ifelse(is.na(naband_sd$sd),9999, naband_sd$sd)
    coordinates(naband_sd) <- ~ x + y # coordinates
    gridded(naband_sd) <- TRUE # gridded
    NA_sd_raster <- raster(naband_sd)
    naband_sd_reclass <- reclassify(NA_sd_raster, c(-Inf,9998,NA,
                                                    9998,Inf,1))

    # Standard deviation of pvalues
    dev_pval <- as.data.frame(dplyr::data_frame(
      x = predict_locs[,1],
      y = predict_locs[,2],
      sd = sd_pval
    ))
    naband_pval <- dev_pval
    lrr_narm <- na.omit(dev_pval) # remove NAs
    coordinates(lrr_narm) <- ~ x + y # coordinates
    gridded(lrr_narm) <- TRUE # gridded
    sd_pval_raster <- raster(lrr_narm)

    # Reclassify for alpha ranges
    reclass_sd_pval <- sd_pval_raster
    reclass_sd_pval@data@values <- ifelse(reclass_sd_pval@data@values < 0.01, NA, reclass_sd_pval@data@values)

    # Create separate layer for NAs (if any)
    naband_pval$sd <- ifelse(is.na(naband_pval$sd),9999, naband_pval$sd)
    coordinates(naband_pval) <- ~ x + y # coordinates
    gridded(naband_pval) <- TRUE # gridded
    NA_sd_raster <- raster(naband_pval)
    naband_pval_reclass <- reclassify(NA_sd_raster, c(-Inf,9998,NA,
                                                      9998,Inf,1))

    # Mean Relative Risk
    upperhalf = length(mean_rr_raster@data@values[mean_rr_raster@data@values>0 & !is.na(mean_rr_raster@data@values)])
    lowerhalf = length(mean_rr_raster@data@values[mean_rr_raster@data@values<0 & !is.na(mean_rr_raster@data@values)])

    max_absolute_value=max(mean_rr_raster@data@values[is.finite(mean_rr_raster@data@values)], na.rm = T) #what is the maximum absolute value of raster?
    min_absolute_value=min(mean_rr_raster@data@values[is.finite(mean_rr_raster@data@values)], na.rm = T) #what is the maximum absolute value of raster?
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
    nhalf = length(mean_rr_raster@data@values)/2
    rclegend = colorRampPalette(colors = c(col.99,col.50, col.01), space="Lab")(nhalf)
    rclegend[c(nhalf,nhalf+1)] = rgb(t(col2rgb(col.50)), maxColorValue=256)

    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(mean_rr_raster, col = rampcols, breaks=rampbreaks, xlab = "Longitude", ylab="Latitude", main = "Mean predicted relative risk of cases and controls in geographic space",
          xlim = c(mean_rr_raster@extent@xmin-0.01*(abs(mean_rr_raster@extent@xmin)),mean_rr_raster@extent@xmax+0.01*(abs(mean_rr_raster@extent@xmin))), ylim = c(mean_rr_raster@extent@ymin-0.01*(abs(mean_rr_raster@extent@ymin)),mean_rr_raster@extent@ymax+0.01*(abs(mean_rr_raster@extent@ymin))))
    image(naband_reclass, col=col.na, add = T)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
    legend_image <- as.raster(matrix(rev(rclegend)), ncol=1)
    plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=3, y = c(0,0.5,1,1.2), labels = c("< 0","0","> 0","Relative Risk"))
    rasterImage(legend_image, 0, 0, 1,1, main = "Relative Risk")
    legend(
      "bottom",
      legend = "Indeterminate",
      fill = col.na,
      ncol = 1,
      bty="n"
    )

    # SD of RR
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(sd_rr_raster,  main = "Standard deviation of predicted relative risk of cases and controls in geographic space", col=rev(terrain.colors(256)),
          xlab = "Longitude", ylab = "Latitude",
          xlim = c(sd_rr_raster@extent@xmin-0.01*(abs(sd_rr_raster@extent@xmin)),reclass_tol@extent@xmax+0.01*(abs(sd_rr_raster@extent@xmin))), ylim = c(sd_rr_raster@extent@ymin-0.01*(abs(sd_rr_raster@extent@ymin)),sd_rr_raster@extent@ymax+0.01*(abs(sd_rr_raster@extent@ymin))))
    image(naband_sd_reclass, col = "black", add = T)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
    legend_image <- as.raster(matrix(terrain.colors(256)), ncol=1)
    plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=3.5, y = c(0,1,1.2), labels = c(paste(round(min(sd_rr_raster@data@values, na.rm=T), digits = 2), sep=""),paste(round(max(sd_rr_raster@data@values, na.rm=T), digits = 2), sep=""), "Standard Deviation"))
    rasterImage(legend_image, 0, 0, 1,1, main = "")
    legend(
      "bottom",
      legend = "Indeterminate",
      fill = "black",
      ncol = 1,
      bty="n"
    )

    # Mean toleance
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(reclass_tol, col = c(col.99, col.95,col.50, col.05, col.01),
          xlab = "Longitude", ylab = "Latitude", main = "Mean predicted significant p-values in geographic space",
          xlim = c(reclass_tol@extent@xmin-0.01*(abs(reclass_tol@extent@xmin)),reclass_tol@extent@xmax+0.01*(abs(reclass_tol@extent@xmin))), ylim = c(reclass_tol@extent@ymin-0.01*(abs(reclass_tol@extent@ymin)),reclass_tol@extent@ymax+0.01*(abs(reclass_tol@extent@ymin))))
    image(naband_p_reclass, col=col.na, add = T)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1), pty = "m")
    plot.new()
    legend(
      "bottomleft", title = "Legend", bty = "n",
      legend = c("p-value < 0.01", "p-value < 0.05", "Insignficant", "p-value > 0.95", "p-value < 0.99", "Indeterminate"),
      fill = c(
        col.01,
        col.05,
        col.50,
        col.95,
        col.99,
        col.na
      ),
      ncol = 1
    )

    # SD of tolerance
    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
    par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    image(reclass_sd_pval,  main = "Standard deviation of predicted p-values in geographic space", col=rev(terrain.colors(256)),
          xlab = "Longitude", ylab = "Latitude",
          xlim = c(reclass_sd_pval@extent@xmin-0.01*(abs(reclass_sd_pval@extent@xmin)),reclass_tol@extent@xmax+0.01*(abs(reclass_sd_pval@extent@xmin))), ylim = c(reclass_sd_pval@extent@ymin-0.01*(abs(reclass_sd_pval@extent@ymin)),reclass_sd_pval@extent@ymax+0.01*(abs(reclass_sd_pval@extent@ymin))))
    image(naband_pval_reclass, col = "black", add = T)
    par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
    legend_image <- as.raster(matrix(terrain.colors(256)), ncol=1)
    plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=3.5, y = c(0,1,1.2), labels = c("0",paste(round(max(reclass_sd_pval@data@values, na.rm=T), digits = 2), sep=""), "Standard Deviation"))
    rasterImage(legend_image, 0, 0, 1,1, main = "")
    legend(
      "bottom",
      legend = "Indeterminate",
      fill = "black",
      ncol = 1,
      bty="n"
    )

  } # end of plot = T
  } # end of predict = T

par(mfrow=c(1,1), par(oma=c(0,0,0,0),mar=c(5.1,4.1,4.1,2.1), pty="m")) #reset

  jitter_llren_output <- list("out_sim" = out_sim ,
                              "out_pred" = out_pred
                                )

} # end of jitter_llren function

