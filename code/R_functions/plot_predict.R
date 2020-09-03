plot_predict <- function(input,
                         plot_cols = c("#0000cd", "#cccccc", "#8b3a3a", "#ffff00"),
                         alpha = 0.05,
                         cref0 = "+init=epsg:4326",
                         cref1 = NULL,
                         ...) {

  op <- par(no.readonly = TRUE)
  # Convert to geospatial rasters
  predict_risk <-  dplyr::data_frame(x = input$out$predict$predict_locs[ , 1],
                                     y = input$out$predict$predict_locs[ , 2],
                                     v = input$out$predict$rr)
  naband <- predict_risk # save for next step
  sp::coordinates(predict_risk) <- ~ x + y # coordinates
  sp::gridded(predict_risk) <- TRUE # gridded
  predict_risk_raster <- raster::raster(predict_risk)
  raster::crs(predict_risk_raster) <- cref0
  if(!is.null(cref1)){
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
  if(!is.null(cref1)){
    NA_risk_raster <- raster::projectRaster(NA_risk_raster,
                                            crs = cref1,
                                            method = "ngb")
  }
  naband_reclass <- raster::reclassify(NA_risk_raster, c(-Inf, 9998, NA,
                                                         9998, Inf, 1))

  # Convert to geospatial raster
  predict_tol <- dplyr::data_frame(x = input$out$predict$predict_locs[ , 1],
                                   y = input$out$predict$predict_locs[ , 2],
                                   v = input$out$predict$pval)
  sp::coordinates(predict_tol) <- ~ x + y # coordinates
  sp::gridded(predict_tol) <- TRUE # gridded
  predict_tol_raster <- raster::raster(predict_tol)
  raster::crs(predict_tol_raster) <- cref0
  if(!is.null(cref1)){
    predict_tol_raster <- raster::projectRaster(predict_tol_raster,
                                                crs = cref1,
                                                method = "ngb")
  }

  reclass_tol <- raster::cut(predict_tol_raster,
                             breaks = c(-Inf, alpha/2, 1-alpha/2, Inf),
                             right = FALSE)

  # Plot 1: log relative risk
  rrp <- lrr_raster(input = predict_risk_raster,
                  cols = plot_cols[c(3,2,1)],
                  midpoint = 0)

  par(pty = "s")
  p1 <- fields::image.plot(rrp$v,
                           breaks = rrp$breaks,
                           col = rrp$cols,
                           axes = TRUE,
                           cex.lab = 1,
                           main = "log relative risk",
                           xlab = "Longitude",
                           ylab = "Latitude",
                           cex = 1,
                           axis.args = list(at = rrp$at,
                                            labels = rrp$labels,
                                            cex.axis = 0.67))
  raster::image(naband_reclass, col = plot_cols[4], add = T)

  # Plot 2: Significant p-values
  if(all(raster::values(reclass_tol)[!is.na(raster::values(reclass_tol))] == 2)){
    pcols <- plot_cols[2]
    brp <- c(1, 3)
    atp <- 2
    labp <- "Insignificant"
  } else {
    pcols <- plot_cols[c(3,2,1)]
    brp <- c(1, 1.67, 2.33, 3)
    atp <- c(1.33, 2, 2.67)
    labp <- c("Presence", "Insignificant", "Absence")
  }

  p2 <- fields::image.plot(reclass_tol,
                           breaks = brp,
                           col = pcols,
                           cex = 1,
                           axes = TRUE,
                           main = paste("Significant p-values\nalpha =", alpha, sep = " "),
                           xlab = "Longitude",
                           ylab = "Latitude",
                           axis.args = list(at = atp,
                                            labels = labp,
                                            las = 0,
                                            cex.axis = 0.67))
  raster::image(naband_reclass, col = plot_cols[4], add = T)

  suppressMessages(suppressWarnings(par(op)))
}
