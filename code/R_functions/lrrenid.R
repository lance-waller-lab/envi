# Log Relative Risk Function with Edge Correction and K-Fold Cross-Validation
# Optimized with foreach do loops
# Option for parallelization of cross-validation
# Option for balanced cross-validation via under-sampling controls
# Converted to sparr notation for added quality
# Allowed for id as a column for replicability
# Similar input to jittered simulated data
#
# Authors: Lance Waller & Ian Buller
# Created: August 2018
#
# Calculates Null model of 2-D log relative risk ratio using the Kelsall & Diggle Method
# Calculates rank of observed model to null model (semi-continuous surface)
# Calculates 95% and 99% tolerance intervals (categorical surface)
# Edge correction using the sparr package (Kelsall-Diggle Correction, could be expanded to others)
# K-Fold Cross Validation
# with do loops to speed up
# option for parallelization of cross-validation

# debug(utils::unpackPkgZip)
# install.packages("sparr")
# install.packages("concaveman")
# install.packages("sp")
# install.packages("rgeos")
# install.packages("raster")
# install.packages("pls")
# install.packages("cvAUC")
# install.packages("ROCR")
# install.packages("parallel")
# install.packages("foreach")

# Data structure
# obs_locs = data.frame of 6 variables (id, lon, lat, mark, v1, v2)
# predict_locs = data.frame of 4 variables (lon, lat, v1, v2)

lrren <- function(obs_locs, predict_locs,
                    #bandw=NULL, sim=NULL,
                    nfold=NULL,
                    #quant=NULL,
                    poly_buffer=NULL,
                    #grid_number=NULL,
                    predict=TRUE, cv=FALSE, parallel=FALSE, balance = FALSE, conserve=TRUE, plot=FALSE, plot.cols=NULL, cref0=NULL, cref1=NULL,
                  ...) {

  suppressMessages(suppressWarnings(require("sparr")))
  suppressMessages(suppressWarnings(require("concaveman")))
  suppressMessages(suppressWarnings(require("sp")))
  suppressMessages(suppressWarnings(require("rgeos")))
  suppressMessages(suppressWarnings(require("dplyr")))
  suppressMessages(suppressWarnings(require("plyr")))
  suppressMessages(suppressWarnings(require("spatstat")))
  suppressMessages(suppressWarnings(require("raster")))
  suppressMessages(suppressWarnings(require("foreach")))

  cat("Estimating observed log relative risk\n")

  ## Set the number of simulations
  # if(is.null(sim)){
  #   sim <- 100
  # } else {sim <- sim
  # }

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

  ## Set CRS
  if(is.null(cref0)){cref0 <- "+proj=longlat +datum=WGS84"}

  # Column names
  names_obs <- names(obs_locs)

  ## Set function used in foreach
  `%fun%` <- `%do%`
  # Combine function used in foreach
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }

  ## Calculate boundary polygons
  # Calculate inner boundary polygon (extent of case and control locations in environmental space)
  
  inner_chull <- concaveman::concaveman(as.matrix(obs_locs[,5:6]))
  inner_chull_pts <- sp::coordinates(inner_chull)
  inner_chull_pts <- rbind(inner_chull_pts, inner_chull_pts[1,]) # repeats the first coordinate to complete the polygon
  inner_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(inner_chull_pts)),1))) # convert points to polygon

  if(is.null(poly_buffer)){
    poly_buffer <- max(inner_chull_pts)/100
  } else {poly_buffer <- poly_buffer
  }

  inner_chull_poly_buffer <- rgeos::gBuffer(inner_chull_poly, width=poly_buffer, byid=TRUE) #add small buffer around polygon to include boundary points
  inner_poly <- inner_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon

  if(is.null(predict_locs)){
    outer_poly <- inner_poly
    outer_chull_poly <- inner_chull_poly
  } else {
  # Calculate outer boundary polygon (full extent of geographical extent in environmental space)
    if(nrow(predict_locs) > 5000000){
      outer_chull <- chull(x=predict_locs[,3], y=predict_locs[,4])
      outer_chull_pts <- predict_locs[c(outer_chull, outer_chull[1]), 3:4]
    }else{
      outer_chull <- concaveman::concaveman(as.matrix(predict_locs[,3:4]))
      outer_chull_pts <- sp::coordinates(outer_chull)
    }

  outer_chull_pts <- rbind(outer_chull_pts, outer_chull_pts[1,]) #repeats the first coordinate to complete the polygon
  outer_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(outer_chull_pts)),1))) # convert points to polygon

  # if(is.null(poly_buffer)){
  #   poly_buffer <- max(outer_chull_pts)/100
  # } else {poly_buffer <- poly_buffer
  # }

  outer_chull_poly_buffer <- rgeos::gBuffer(outer_chull_poly, width=poly_buffer, byid=TRUE) #add small buffer around polygon to include boundary points
  outer_poly <- outer_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon
  }

  # Calculate inner boundary polygon (extent of case and control locations in environmental space)
  # inner_chull <- concaveman::concaveman(cbind(obs_locs[,5], obs_locs[,6]))
  # inner_chull_pts <- sp::coordinates(inner_chull)
  # inner_chull_pts <- rbind(inner_chull_pts, inner_chull_pts[1,]) # repeats the first coordinate to complete the polygon
  # inner_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(inner_chull_pts)),1))) # convert points to polygon
  # inner_chull_poly_buffer <- rgeos::gBuffer(inner_chull_poly, width=poly_buffer, byid=TRUE) #add small buffer around polygon to include boundary points
  # inner_poly <- inner_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon

  if(conserve==TRUE){window_poly <- inner_poly
  } else {
    window_poly <- outer_poly
  }

  ## Input Preparation
  # case and control point pattern datasets
  case_locs <- subset(obs_locs, obs_locs$mark == 1)
  control_locs <- subset(obs_locs, obs_locs$mark == 0)

  ppp_case <- spatstat::ppp(case_locs[,5], case_locs[,6], window = owin(poly = list(x=rev(window_poly[,1]),y=rev(window_poly[,2]))))
  ppp_control <- spatstat::ppp(control_locs[,5], control_locs[,6], window = owin(poly = list(x=rev(window_poly[,1]),y=rev(window_poly[,2]))))

  ## Calculate Bandwidth or user specified value
  # if(h0 == "diggle"){
  #   h0 <- sparr::LSCV.risk(f = ppp_case, g = ppp_control,...)
  # } else {h0 <- h0
  # }

  ## Calculate observed kernel density ratio
  obs_lrr <- sparr::risk(f=ppp_case, g=ppp_control,...)
  cat("Case Bandwidth =", obs_lrr$f$h0, "\n")
  cat("Control Bandwidth =", obs_lrr$g$h0, "\n")
  bandw <- obs_lrr$f$h0

  # ## Calculate test statistic
  # test.stat.obs <- sum((t(obs_lrr$rr$v)[is.finite(t(obs_lrr$rr$v)) & !is.na(t(obs_lrr$rr$v))]/(diff(obs_lrr$rr$xcol)[1]*diff(obs_lrr$rr$yrow)[1]))^2) #include is.finite() for calculation?

  if(plot == FALSE){
  } else {
    ## Visualizing observed kernel densities and density ratio
    x_con <- control_locs[,5]
    y_con <- control_locs[,6]
    x_cas <- case_locs[,5]
    y_cas <- case_locs[,6]
    p_all <- cbind(c(x_cas,x_con), c(y_cas,y_con))

    # Densities of Cases and Controls
    layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4, 1))
    par(oma=c(0,1,0,0),mar=c(0.1,4.1,4.1,2.1), pty="s")
    plot(x_con, y_con,
         xlab=names_obs[5],ylab=names_obs[6],
         xlim=c(min(bbox(outer_chull_poly)[1,]), max(bbox(outer_chull_poly)[1,])),
         ylim=c(min(bbox(outer_chull_poly)[2,]), max(bbox(outer_chull_poly)[2,])),
         type="n")
    graphics::polygon(inner_poly, border = "grey")
    graphics::polygon(outer_poly, lty = 2, border = "grey")
    graphics::points(x_con,y_con, pch=1, col="black", cex=0.5)
    title("Control Locations",cex.main=1.1)
    graphics::contour(obs_lrr$g$z$xcol,obs_lrr$g$z$yrow,t(obs_lrr$g$z$v),add=T,
                      vfont=c("sans serif","bold"), nlevels=10,
                      drawlabels=F, col = "black")

    plot(x_cas, y_cas,
         xlab=names_obs[5],ylab=names_obs[6],
         xlim=c(min(bbox(outer_chull_poly)[1,]), max(bbox(outer_chull_poly)[1,])),
         ylim=c(min(bbox(outer_chull_poly)[2,]), max(bbox(outer_chull_poly)[2,])),
         type="n")
    graphics::polygon(inner_poly, border = "grey")
    graphics::polygon(outer_poly, lty = 2, border = "grey")
    graphics::points(x_cas,y_cas,pch=16, col="black", cex=0.5)
    title("Case Locations",cex.main=1.1)
    graphics::contour(obs_lrr$f$z$xcol,obs_lrr$f$z$yrow,t(obs_lrr$f$z$v),add=T,
                      vfont=c("sans serif","bold"), nlevels=10,
                      drawlabels=F,col = "black")

    par(mai=c(0,0,0,0), mar=c(5.1,4.1,0.1,2.1)/5, pty = "m")
    plot.new()
    legend(x="top", inset = 0, ncol=2, title = "Legend",
           legend=c("One bandwidth", "Extent of prediction data","Extent of observed data","Control locations", "Case locations"), col=c("black", "grey", "grey", "black", "black"), lwd=2, lty=c(1,2,1,NA,NA), pch=c(NA,NA,NA,1,16))
    mtext(paste("Estimated intensity functions in Predictor Space\nKelsall and Diggle method with bandwidth of", round(bandw, digits = 3), sep = " "), side = 3, line = -4, outer = TRUE, cex=1.25)

    ## 3-D plot of log relative risk
    par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1),oma=c(0,0,0,0),mai = c(1.02, 0.82, 0.82, 0.42),xpd=FALSE, pty="m")
    graphics::persp(obs_lrr$rr$xcol,obs_lrr$rr$yrow,t(obs_lrr$rr$v),theta=-30,phi=45, xlab=names_obs[5],ylab=names_obs[6],zlab="log relative risk",zlim=c(min(obs_lrr$rr$v[is.finite(obs_lrr$rr$v)], na.rm = T),max(obs_lrr$rr$v[is.finite(obs_lrr$rr$v)], na.rm = T)), main=paste("log relative risk surface for bandwidth of ",round(bandw, digits = 3),"\ncases versus control", sep = "")
                    ,lwd=0.5, ticktype='detailed',
                    axes = T,
                    box = T
    )

    ## 2-D Plot of log relative risk
    layout(matrix(c(1,2), ncol=1, byrow=TRUE), heights=c(4, 1))
    par(oma=c(0,1,0,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    plot(x_cas, y_cas,
         xlab=names_obs[5],ylab=names_obs[6],
         xlim=c(min(bbox(outer_chull_poly)[1,]), max(bbox(outer_chull_poly)[1,])),
         ylim=c(min(bbox(outer_chull_poly)[2,]), max(bbox(outer_chull_poly)[2,])),
         # pch=16,
         # col="black",
         # cex=0.5
         type = "n"
    )
    graphics::contour(obs_lrr$rr$xcol,obs_lrr$rr$yrow,t(obs_lrr$rr$v),
                      levels=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),lwd=1.5,
                      lty=   c(3,3,3,3,1,2,2,2,2),
                      # vfont=c("sans serif","bold"),
                      # labcex=1.0,
                      add=T, col = "black")
    graphics::polygon(inner_poly, border = "grey")
    graphics::polygon(outer_poly, lty = 2, border = "grey")

    par(mai=c(0,0,0,0), mar=c(5.1,4.1,0.1,2.1)/5, pty = "m")
    plot.new()
    legend("top",ncol=2, legend=c(#"Case locations",
      "log relative risk values above null 0", "log relative risk values at null 0", "log relative risk values below null 0","Extent of prediction data","Extent of observed data"),
      col=c(#"black",
        "black", "black", "black","grey", "grey"),
      lwd=2,
      lty=c(#0,
        2,1,3,2,1),
      pch=c(#16,
        NA,NA,NA,NA,NA), bty = "o", title = "Legend")
    mtext(paste("log relative risk for bandwidth of ", round(bandw, digits=3), "\ncases versus controls",sep = ""), side = 3, line = -3, outer = TRUE, cex = 1.1)

    ## Plot of p-value surface for tolerance contours
    layout(matrix(c(1,2), ncol=1, byrow=TRUE), heights=c(4, 1))
    par(oma=c(0,1,0,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    plot(x_cas, y_cas,
         xlab=names_obs[5],ylab=names_obs[6],
         xlim=c(min(bbox(outer_chull_poly)[1,]), max(bbox(outer_chull_poly)[1,])),
         ylim=c(min(bbox(outer_chull_poly)[2,]), max(bbox(outer_chull_poly)[2,])),
         # pch=16,
         # col="black",
         # cex=0.5
         type = "n"
    )
    graphics::contour(obs_lrr$P$xcol,obs_lrr$P$yrow,t(obs_lrr$P$v), add=T,
                      levels = c(0.005,0.025,0.5,0.975,0.995),
                      drawlabels=F, col = "black",
                      lwd = c(1,2,3,2,1), lty = c(2,2,1,3,3))
    graphics::polygon(inner_poly, border = "grey")
    graphics::polygon(outer_poly, lty = 2, border = "grey")

    par(mai=c(0,0,0,0), mar=c(5.1,4.1,0.1,2.1)/5, pty = "m")
    plot.new()
    legend("top",ncol=3, legend=c(#"Case locations",
      "p-value < 0.01",
      "p-value < 0.05",
      "p-value = 0.5",
      "p-value > 0.95",
      "p-value > 0.99", "",
      "Extent of prediction data",
      "Extent of observed data"
    ),
    col=c(#"black",
      "black","black", "black", "black", "black","transparent","grey", "grey"), lwd=c(1,2,3,2,1,NA,1,1), lty=c(#0,
        2,2,1,3,3,NA,2,1), pch=c(#16,
          NA,NA,                                                                            NA,NA,NA,NA,NA,NA), bty = "o", title = "Legend")
    mtext(paste("Asymptomic tolerance contours for bandwidth of ", round(bandw, digits=3), "\ncases versus controls",sep = ""), side = 3, line = -3, outer = TRUE, cex = 1.1)

    # Plot of log relative risks (with color)
    rho_hat_values <- as.vector(t(obs_lrr$rr$v))

    upperhalf = length(rho_hat_values[rho_hat_values>0 & !is.na(rho_hat_values)])
    lowerhalf = length(rho_hat_values[rho_hat_values<0 & !is.na(rho_hat_values)])
    nhalf = length(rho_hat_values[!is.na(rho_hat_values)])/2

    max_absolute_value=max(rho_hat_values[is.finite(rho_hat_values)], na.rm = T) #what is the maximum absolute value of raster?
    min_absolute_value=min(rho_hat_values[is.finite(rho_hat_values)], na.rm = T) #what is the maximum absolute value of raster?
    Thresh = 0
    ## Make vector of colors for values below threshold
    rc1 = colorRampPalette(colors = c(col.99, col.50), space="Lab")(lowerhalf)
    ## Make vector of colors for values above threshold
    rc2 = colorRampPalette(colors = c(col.50, col.01), space="Lab")(upperhalf)
    rampcols = c(rc1, rc2)
    # Add threshold color
    rampcols[c(upperhalf, lowerhalf+1)] = rgb(t(col2rgb(col.50)), maxColorValue=256)

    rb1 = seq(min_absolute_value, Thresh, length.out=lowerhalf+1)
    rb2 = seq(Thresh, max_absolute_value, length.out=upperhalf+1)[-1]
    rampbreaks = c(rb1, rb2)

    layout(matrix(c(1,2), ncol=1, byrow=TRUE), heights=c(4, 1))
    par(oma=c(0,1,0,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
    plot(obs_lrr, col = rampcols, breaks=rampbreaks,
         main = "log relative risk of cases versus controls\nwith tolerance intervals (alpha = 0.05 & alpha = 0.01)",
         xlab=names_obs[5],ylab=names_obs[6],
         clipwin= owin(poly = list(x=rev(outer_poly[,1]),y=rev(outer_poly[,2]))),
         #xlim=c(min(bbox(outer_chull_poly)[1,]), max(bbox(outer_chull_poly)[1,])),
         # ylim=c(min(bbox(outer_chull_poly)[2,]), max(bbox(outer_chull_poly)[2,])),
         tol.show = F, useRaster = T
    )
    graphics::contour(obs_lrr$P$xcol,obs_lrr$P$yrow,t(obs_lrr$P$v), add=T,
                      levels = c(0.005,0.025,0.5,0.975,0.995),
                      drawlabels=F, col = "black", lwd = c(1,2,3,2,1), lty = c(2,2,1,3,3))
    polygon(outer_poly, lty = 1, lwd = 1, border = "black")
    par(mai=c(0,0,0,0), mar=c(5.1,4.1,0.1,2.1)/5, pty = "m")
    plot.new()
    legend("top",ncol=2, legend=c(#"Case locations",
      "p-value < 0.01",
      "p-value < 0.05",
      "p-value = 0.5",
      "p-value > 0.95",
      "p-value > 0.99",
      "Extent of prediction data"
    ),
    col=c(#"black",
      "black","black", "black", "black", "black","black"), lwd=c(1,2,3,2,1), lty=c(#0,
        2,2,1,3,3,1), pch=c(#16,
          NA,NA,                                                                            NA,NA,NA,NA), bty = "o", title = "Legend")
  }

  ## Project Kelsall and Diggle results to geographic space
  if(predict == FALSE){
    output <- list(
      #"test_stat" = test.stat.obs,
      #"sim_stats" = test.stat.sim,
      #"p_val" = pval,
      "obs_lrr" = obs_lrr,
      "outer_poly" = outer_poly,
      "inner_poly" = inner_poly
      #"rx" = rx,
      #"ry" = ry,
      #"obs_rank" = as.vector(obs_rank),
      #"obs_quant" = lrr_quant$obs_quant,
    )
  }
  else{
    cat("Predicting area of interest\n")

    ## Create index coordinates
    rx <- rep(obs_lrr$rr$xcol, length(obs_lrr$rr$yrow))
    for(i in 1:length(obs_lrr$rr$yrow)){
      if (i == 1){
        ry <- rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol))
      }
      if (i != 1){
        ry <- c(ry, rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol)))
      }
    }

    # Convert to semi-continuous raster
    rr <- as.data.frame(dplyr::data_frame(
      x = rx,
      y = ry,
      obs_risk = as.vector(t(obs_lrr$rr$v))
    ))
    lrr_narm <- na.omit(rr) # remove NAs
    coordinates(lrr_narm) <- ~ x + y # coordinates
    gridded(lrr_narm) <- TRUE # gridded
    rr_raster <- raster(lrr_narm)

    # Convert to categorical raster
    pval <- as.data.frame(dplyr::data_frame(
      x = rx,
      y = ry,
      obs_tol = as.vector(t(obs_lrr$P$v))
    ))
    lrr_narm <- na.omit(pval) # remove NAs
    coordinates(lrr_narm) <- ~ x + y # coordinates
    gridded(lrr_narm) <- TRUE # gridded
    pval_raster <- raster(lrr_narm)

    # Prediction locations
    extract_points <- cbind(predict_locs[,3], predict_locs[,4])
    extract_predict <- data.frame(predict_locs, raster::extract(rr_raster, extract_points), raster::extract(pval_raster, extract_points))
    extract_predict <- plyr::rename(extract_predict, c("raster..extract.rr_raster..extract_points." = "rr", "raster..extract.pval_raster..extract_points." = "pval"))

    # Convert to geospatial raster
    predict_risk <- as.data.frame(dplyr::data_frame(
      x = extract_predict[,1],
      y = extract_predict[,2],
      rr = extract_predict$rr
    ))
    naband <- predict_risk # save for next step
    coordinates(predict_risk) <- ~ x + y # coordinates
    gridded(predict_risk) <- TRUE # gridded
    predict_risk_raster <- raster(predict_risk)
    crs(predict_risk_raster) <- cref0
    if(!is.null(cref1)){
    predict_risk_raster <- projectRaster(predict_risk_raster, crs = cref1, method = "ngb")
    }

    # Create separate layer for NAs (if any)
    naband$rr <- ifelse(is.na(naband$rr),9999, naband$rr)
    coordinates(naband) <- ~ x + y # coordinates
    gridded(naband) <- TRUE # gridded
    NA_risk_raster <- raster(naband)
    crs(NA_risk_raster) <- cref0
    if(!is.null(cref1)){
    NA_risk_raster <- projectRaster(NA_risk_raster, crs = cref1, method = "ngb")
    }
    naband_reclass <- reclassify(NA_risk_raster, c(-Inf,9998,NA,
                                                   9998,Inf,1))

    # Convert to geospatial raster
    predict_tol <- as.data.frame(dplyr::data_frame(
      x = extract_predict[,1],
      y = extract_predict[,2],
      pval = extract_predict$pval
    ))
    #predict_tol$pval <- ifelse(is.na(predict_tol$pval),1.01, predict_tol$pval)
    coordinates(predict_tol) <- ~ x + y # coordinates
    gridded(predict_tol) <- TRUE # gridded
    predict_tol_raster <- raster(predict_tol)
    crs(predict_tol_raster) <- cref0
    if(!is.null(cref1)){
    predict_tol_raster <- projectRaster(predict_tol_raster, crs = cref1)
    }

    reclass_tol <- raster::reclassify(predict_tol_raster, c(-Inf,0.005,5,
                                                            0.005,0.025,4,
                                                            0.025,0.975,3,
                                                            0.975,0.995,2,
                                                            0.995,Inf,1
                                                            #0.99,1.0,1
                                                            #,1.0,Inf,0
    ))

    if(plot == T){

      # Plot of log relative risk
      upperhalf = length(predict_risk_raster@data@values[predict_risk_raster@data@values>0 & !is.na(predict_risk_raster@data@values)])
      lowerhalf = length(predict_risk_raster@data@values[predict_risk_raster@data@values<0 & !is.na(predict_risk_raster@data@values)])

      max_absolute_value=max(predict_risk_raster@data@values[is.finite(predict_risk_raster@data@values)]) #what is the maximum absolute value of raster?
      min_absolute_value=min(predict_risk_raster@data@values[is.finite(predict_risk_raster@data@values)]) #what is the maximum absolute value of raster?
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
      nhalf = length(predict_risk_raster@data@values)/2
      rclegend = colorRampPalette(colors = c(col.99,col.50, col.01), space="Lab")(nhalf)
      rclegend[c(nhalf,nhalf+1)] = rgb(t(col2rgb(col.50)), maxColorValue=256)

      # par(mfrow=c(1,1), par(oma=c(0,0,0,0),mar=c(5.1,4.1,4.1,2.1), pty="m")) #reset
      # plot(naband_reclass, col="yellow", legend = F)
      # plot(predict_risk_raster, col = rampcols, breaks=rampbreaks, legend = F, add =T)

      # Plot of log relative risk
      layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
      par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
      image(predict_risk_raster, col = rampcols, breaks=rampbreaks, xlab = "Longitude", ylab="Latitude", main = "Predicted log relative risk of cases and controls in geographic space",
            xlim = c(predict_risk_raster@extent@xmin-0.01*(abs(predict_risk_raster@extent@xmin)),predict_risk_raster@extent@xmax+0.01*(abs(predict_risk_raster@extent@xmin))), ylim = c(predict_risk_raster@extent@ymin-0.01*(abs(predict_risk_raster@extent@ymin)),predict_risk_raster@extent@ymax+0.01*(abs(predict_risk_raster@extent@ymin))))
      image(naband_reclass, col=col.na, add = T)
      #plot(wus, add = T)
      par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1)/4, pty = "m")
      # par(xpd=T)
      legend_image <- as.raster(matrix(rev(rclegend)), ncol=1)
      plot(c(-10,20),c(-0.1,5),type = 'n', axes = F,xlab = '', ylab = '')
      text(x=3.2, y = c(0,0.5,1,1.2), labels = c(round(min_absolute_value,digits=0),"0",round(max_absolute_value,digits=0),"log relative risk"))
      rasterImage(legend_image, 0, 0, 1,1, main = "log relative risk")
      legend(
        "bottom",
        legend = "Indeterminate",
        fill = col.na,
        ncol = 1,
        bty="n"
      )

      # Plot of p-values
      layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(4, 1))
      par(oma=c(0,0,1,0),mar=c(5.1,4.1,4.1,2.1), pty="s")
      image(reclass_tol, col = #c("yellow","blue3", "cornflowerblue","gray80", "indianred1", "indianred4"),
              c(col.99, col.95,col.50, col.05, col.01),
            xlab = "Longitude", ylab = "Latitude", main = "Predicted significant p-values in geographic space",
            xlim = c(reclass_tol@extent@xmin-0.01*(abs(reclass_tol@extent@xmin)),reclass_tol@extent@xmax+0.01*(abs(reclass_tol@extent@xmin))), ylim = c(reclass_tol@extent@ymin-0.01*(abs(reclass_tol@extent@ymin)),reclass_tol@extent@ymax+0.01*(abs(reclass_tol@extent@ymin))))
      image(naband_reclass, col=col.na, add = T)
      par(mai=c(0,0,0,0), mar=c(5.1,0.1,4.1,2.1), pty = "m")
      plot.new()
      legend(
        "bottomleft", title = "Legend", bty = "n",
        legend = c("p-value < 0.01", "p-value < 0.05", "Insignficant", "p-value > 0.95", "p-value > 0.99", "Indeterminate"),
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
    } # end of plot = T

    output <- list(
      # "test_stat" = test.stat.obs,
      # "p_val" = pval,
      "obs_lrr" = obs_lrr,
      "outer_poly" = outer_poly,
      "inner_poly" = inner_poly,
      # "obs_rank" = as.vector(obs_rank),
      # "obs_quant" = lrr_quant$obs_quant,
      # "rx" = rx,
      # "ry" = ry,
      "predict" = extract_predict
    )

  }
  par(mfrow=c(1,1), par(oma=c(0,0,0,0),mar=c(5.1,4.1,4.1,2.1), pty="m")) #reset

  # ## Output
  # lrren_output <- output

  #### K-Fold Cross Validation
  if(cv == FALSE){
    cv_results <- NULL
  } else {
    require("pls")
    require("cvAUC")
    require("ROCR")

    cv_predictions_rank <- list()
    cv_predictions_quant <- list()
    cv_labels <- list()
    cv_pvals <- list()

    ## Set function used in foreach
    `%fun%` <- `%do%`
    # Combine function used in foreach
    comb <- function(x, ...) {
      lapply(seq_along(x),
             function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
    }

    # Partition n-folds
    ####### IF RANDOM CROSS-VALIDATION #####
    if(is.null(nfold)){
      nfold <- 10
    } else {nfold <- nfold
    }

    # Randomly sample data into n-folds
    if(balance == FALSE)  {
      # Set-up presence-absence data
      # PA <- as.data.frame(cbind(c(case_locs[,2], control_locs[,2]), c(case_locs[,3], control_locs[,3]), c(case_locs[,3], control_locs[,3]), c(case_locs[,4], control_locs[,4]), c(rep(1, nrow(case_locs)), rep(0,nrow(control_locs)))))
      # names(PA) <- c("lon", "lat", "v1", "v2", "mark")
      cv_segments <- pls::cvsegments(nrow(obs_locs), nfold)
      cv_seg_cas <- NULL
      cv_seg_con <- NULL
    } else {
      cv_seg_cas <-  pls::cvsegments(nrow(case_locs), nfold)
      cv_seg_con <-  pls::cvsegments(nrow(control_locs), nfold)
      #PA <- NULL
      cv_segments <- NULL
    }

    # Train model with training dataset and predict testing dataset
    # if (parallel == TRUE){
    #   require(parallel)
    #   no_cores <- detectCores() - 1 # Set the number of cores for parallelization
    #   cl <- makeCluster(no_cores)
    #   clusterExport(cl, c("PA", "cv_segments", "cv_seg_cas", "cv_seg_con","nfold", "grid_number", "outer_poly", "inner_poly", "case_locs", "control_locs", "window_poly", "obs_lrr", "adapt"))
    #   `%fun%` <- `%dopar%`
    # }
    out_par <- foreach(k=1:nfold , .combine = comb, .multicombine = TRUE, .packages = c("sparr", "spatstat", "dplyr", "raster")
                       , .init = list(list(), list(), list())
    ) %fun% {

      # h0 <- obs_lrr$f$h0
      # hp <- hp
      # adapt <- adapt

      cat("Cross-validation in progress. Fold", k, "of", nfold, "\n")
      #if (k == nfold+1) cat("Done! Cross-validation complete\n")

      if(balance == FALSE) {
        testing <- obs_locs[cv_segments[k]$V,]
        training <- obs_locs[-(cv_segments[k]$V),]
      }else{
        ind <- 1:length(cv_seg_con[k]$V)
        randind <- sample(ind, length(cv_seg_cas[k]$V), replace = F)

        testing_cas <- case_locs[cv_seg_cas[k]$V,]
        #testing_cas[,5] <- 1
        testing_con <- control_locs[cv_seg_con[k]$V,]
        #testing_con[,5] <- 0
        testing_con <- testing_con[randind,] # under-sample the controls for testing
        testing <- rbind(testing_cas,testing_con)
        training_cas <- case_locs[-(cv_seg_cas[k]$V),]
        #training_cas[,5] <- 1
        training_con <- control_locs[-(cv_seg_con[k]$V),]
        #training_con[,5] <- 0
        training <- rbind(training_cas,training_con)
      }

      # training data
      # case and control point pattern datasets
      ppp_case_training <- spatstat::ppp(training[,5][training[,4] == 1], training[,6][training[,4] == 1], window = owin(poly = list(x=rev(window_poly[,1]),y=rev(window_poly[,2]))))
      ppp_control_training <- spatstat::ppp(training[,5][training[,4] == 0], training[,6][training[,4] == 0], window = owin(poly = list(x=rev(window_poly[,1]),y=rev(window_poly[,2]))))
      # marked point pattern dataset
      # ppp_cascon_training <- spatstat::superimpose("case"=ppp_case_training, "control"=ppp_control_training, W = owin(poly = list(x=rev(window_poly[,1]),y=rev(window_poly[,2]))))

      ## Calculate observed kernel density ratio
      rand_lrr <- sparr::risk(f=ppp_case_training, g=ppp_control_training,...)

      ## Create index coordinates
      rx <- rep(rand_lrr$rr$xcol, length(rand_lrr$rr$yrow))
      for(i in 1:length(rand_lrr$rr$yrow)){
        if (i == 1){
          ry <- rep(rand_lrr$rr$yrow[i], length(rand_lrr$rr$xcol))
        }
        if (i != 1){
          ry <- c(ry, rep(rand_lrr$rr$yrow[i], length(rand_lrr$rr$xcol)))
        }
      }

      # Convert to semi-continuous raster
      rr <- as.data.frame(dplyr::data_frame(
        x = rx,
        y = ry,
        obs_risk = as.vector(t(rand_lrr$rr$v))
      ))
      lrr_narm <- na.omit(rr) # remove NAs
      coordinates(lrr_narm) <- ~ x + y # coordinates
      gridded(lrr_narm) <- TRUE # gridded
      rr_raster <- raster(lrr_narm)

      # Convert to categorical raster
      pval <- as.data.frame(dplyr::data_frame(
        x = rx,
        y = ry,
        obs_tol = as.vector(t(rand_lrr$P$v))
      ))
      lrr_narm <- na.omit(pval) # remove NAs
      coordinates(lrr_narm) <- ~ x + y # coordinates
      gridded(lrr_narm) <- TRUE # gridded
      pval_raster <- raster(lrr_narm)

      ## Predict testing dataset
      extract_testing <- testing[,5:6]

      # Output for each n-fold
      cv_predictions_rr <- raster::extract(rr_raster, extract_testing) # Record category (semi-continuous) of testing data
      cv_predictions_pval <- raster::extract(pval_raster, extract_testing) # Record category (categorical) of testing data
      cv_labels <- testing[,4] # Record labels (marks) of testing data

      par_results <- list("cv_predictions_rr" = cv_predictions_rr,
                          "cv_predictions_pval" = cv_predictions_pval,
                          "cv_labels"= cv_labels
      )
      par_results <- list(cv_predictions_rr,
                          cv_predictions_pval,
                          cv_labels
      )
      return(par_results)
    }

    # if(parallel == T){
    #   stopCluster(cl)
    # }
    cat("Calculating Cross-Validation Statistics\n")
    cv_predictions_rr <- out_par[[1]]
    cv_predictions_pval <- out_par[[2]]
    cv_labels <- out_par[[3]]

    cv_results <- list("cv_predictions_rr" = cv_predictions_rr,
                       "cv_predictions_pval" = cv_predictions_pval,
                       "cv_labels" = cv_labels
    )

    ## Receiver Operating Characteristic
    if(plot == FALSE){
    } else {
      out_cv_rr <- cvAUC::cvAUC(cv_results$cv_predictions_rr, cv_results$cv_labels)
      out_cv_pval <- cvAUC::cvAUC(cv_results$cv_predictions_pval, cv_results$cv_labels)
      out_ci_rr <- cvAUC::ci.cvAUC(cv_results$cv_predictions_rr, cv_results$cv_labels, confidence = 0.95)
      out_ci_pval <- cvAUC::ci.cvAUC(cv_results$cv_predictions_pval, cv_results$cv_labels, confidence = 0.95)

      layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4, 1))
      par(oma=c(0,1,0,0),mar=c(0.1,4.1,4.1,2.1), pty="s")
      plot(out_cv_rr$perf, col="black", lty=3,
           xlab = "False Positive Rate (FPR)",
           ylab = "True Positive Rate (TPR)") #Plot fold AUCs
      abline(0,1, col = "black", lty = 2)
      plot(out_cv_rr$perf, col="red", avg="vertical", add=TRUE, lwd = 2) #Plot CV AUC
      title(paste("log relative risk prediction\nArea Under Curve (AUC) = ", round(out_cv_rr$cvAUC, digits = 3), " (95% CI: ", round(out_ci_rr$ci[1], digits = 3)," - ", round(out_ci_rr$ci[2], digits=3),")",sep=""),cex.main=1.1)

      plot(out_cv_pval$perf, col="black", lty=3,
           xlab = "False Positive Rate (FPR)",
           ylab = "True Positive Rate (TPR)") #Plot fold AUCs
      abline(0,1, col = "black", lty = 2)
      plot(out_cv_pval$perf, col="red", avg="vertical", add=TRUE, lwd = 2) #Plot CV AUC
      title(paste("Tolerance threshold prediction\nArea Under Curve (AUC) = ", round(out_cv_pval$cvAUC, digits = 3), " (95% CI: ", round(out_ci_pval$ci[1], digits = 3)," - ", round(out_ci_pval$ci[2], digits=3),")",sep=""),cex.main=1.1)

      par(mai=c(0,0,0,0), mar=c(5.1,4.1,0.1,2.1)/5, pty = "m")
      plot.new()
      legend(x="top", inset = 0, title = "Legend",
             legend = c("Individual n-fold",
                        "Average",
                        "Luck (Reference)"),
             lty = c(3,1,2),
             col = c("black", "red", "black"))
      mtext(paste("Area Under the Receiver Operating Characteristic Curve\n",nfold,"-fold cross-validation", sep=""), side = 3, line = -4, outer = TRUE, cex=1.25)

      ## Precision Recall

      pred_rr <- ROCR::prediction(cv_results$cv_predictions_rr,cv_results$cv_labels)
      perf_rr <- ROCR::performance(pred_rr, "prec", "rec") # PRREC same as "ppv", "tpr"
      pred_pval <- ROCR::prediction(cv_results$cv_predictions_pval,cv_results$cv_labels)
      perf_pval <- ROCR::performance(pred_pval, "prec", "rec") # PRREC same as "ppv", "tpr"

      layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4, 1))
      par(oma=c(0,1,0,0),mar=c(0.1,4.1,4.1,2.1), pty="s")
      plot(perf_rr, ylim = c(0,1), xlim = c(0,1), lty = 3,
           xlab = "True Positive Rate (Sensitivity or Recall)",
           ylab = "Positive Predictive Value (Precision)")
      abline((nrow(case_locs)/nfold)/length(cv_results$cv_labels[[1]])

             ,0, lty = 2, col = "black")
      lines(colMeans(do.call(rbind,perf_rr@x.values)),colMeans(do.call(rbind,perf_rr@y.values)), col = "red", lty = 1, lwd = 2) # mean PRREC
      title("Semi-continuous prediction",cex.main=1.1)

      plot(perf_pval, ylim = c(0,1), xlim = c(0,1), lty = 3,
           xlab = "True Positive Rate (Sensitivity or Recall)",
           ylab = "Positive Predictive Value (Precision)")
      abline((nrow(case_locs)/nfold)/length(cv_results$cv_labels[[1]])

             ,0, lty = 2, col = "black")
      lines(colMeans(do.call(rbind,perf_pval@x.values)),colMeans(do.call(rbind,perf_pval@y.values)), col = "red", lty = 1, lwd = 2) # mean PRREC
      title("Categorical prediction",cex.main=1.1)

      par(mai=c(0,0,0,0), mar=c(5.1,4.1,0.1,2.1)/5, pty = "m")
      plot.new()
      legend(x="top", inset = 0, title = "Legend",
             legend = c("Individual n-fold",
                        "Average",
                        "Luck (Reference)"),
             lty = c(3,1,2),
             col = c("black", "red", "black"))
      mtext(paste("Precision-Recall Curve\n",nfold,"-fold cross-validation", sep=""), side = 3, line = -4, outer = TRUE, cex=1.25)
    }

  }
  par(mfrow=c(1,1), par(oma=c(0,0,0,0),mar=c(5.1,4.1,4.1,2.1), pty="s")) #reset

  ## Output
  lrren_output <- list("out" = output,
                          "cv" = cv_results
  )

} # End of function
