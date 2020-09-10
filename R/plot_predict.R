#' Visualizations for a predicted ecological niche in geographic space
#' 
#' Create multiple plots of output from the \code{\link{lrren}} function, specifically for the predicted values of the ecological niche at geographic coordinates. 
#' 
#' @param input An object of class "list" from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length four (4) specifying the colors for plotting: 1) presense, 2) neither, 3) absence, and 4) NA values. The default colors in hex are \code{c("#8b3a3a", "#cccccc", "#0000cd", "#ffff00")} or \code{c("indianred4", "grey80", "blue3", "yellow")}.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default is 0.05).
#' @param cref0 Character. The Coordinate Reference System (CRS) for the x- and y-coordinates in geographic space. The default is WGS84 \code{"+init=epsg:4326"}.
#' @param cref1 Optional, character. The Coordinate Reference System (CRS) to spatially project the x- and y-coordinates in geographic space. 
#' @param ... Arguments passed to \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces two plots in a two-dimensional space where the axes are geographic coordinates (e.g., longitude and latitude): 1) predicted log relative risk, and 2) significant p-values. 
#' 
#' @importFrom fields image.plot
#' @importFrom graphics par
#' @importFrom raster crs cut image projectRaster raster reclassify values 
#' @importFrom sp coordinates gridded
#' @import maptools
#' @export
#'
#' @examples
#' \donttest{
#' plot_predict(input = test_lrren)
#' }
#' 
plot_predict <- function(input,
                         plot_cols = c("#8b3a3a", "#cccccc", "#0000cd", "#ffff00"),
                         alpha = 0.05,
                         cref0 = "+init=epsg:4326",
                         cref1 = NULL,
                         ...) {

  op <- graphics::par(no.readonly = TRUE)
  # Convert to geospatial rasters
  predict_risk <-  data.frame("x" = input$out$predict$predict_locs.x,
                              "y" = input$out$predict$predict_locs.y,
                              "v" = input$out$predict$rr)
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
  
  naband_reclass <- raster::reclassify(NA_risk_raster,
                                       c(-Inf, 9998, NA,
                                         9998, Inf, 1))
  if (all(is.na(raster::values(naband_reclass)))) { naband_reclass <- NULL }
  

  # Convert to geospatial raster
  predict_tol <- data.frame("x" = input$out$predict$predict_locs.x,
                            "y" = input$out$predict$predict_locs.y,
                            "v" = input$out$predict$pval)
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
  rrp <- lrr_raster(input = predict_risk_raster,
                    cols = plot_cols[1:3],
                    midpoint = 0)

  graphics::par(pty = "s")
  p1 <- fields::image.plot(rrp$v,
                           breaks = rrp$breaks,
                           col = rrp$cols,
                           axes = TRUE,
                           main = "log relative risk",
                           xlab = "Longitude",
                           ylab = "Latitude",
                           legend.mar = 3.1,
                           axis.args = list(at = rrp$at,
                                            las = 0,
                                            labels = rrp$labels,
                                            cex.axis = 0.67))
  if (!is.null(naband_reclass)) {
  raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
  }

  # Plot 2: Significant p-values
  if (all(raster::values(reclass_tol)[!is.na(raster::values(reclass_tol))] == 2)) {
    pcols <- plot_cols[2]
    brp <- c(1, 3)
    atp <- 2
    labp <- "Insignificant"
  } else {
    pcols <- plot_cols[1:3]
    brp <- c(1, 1.67, 2.33, 3)
    atp <- c(1.33, 2, 2.67)
    labp <- c("Presence", "Insignificant", "Absence")
  }

  p2 <- fields::image.plot(reclass_tol,
                           breaks = brp,
                           col = pcols,
                           axes = TRUE,
                           main = paste("Significant p-values\nalpha =", alpha, sep = " "),
                           xlab = "Longitude",
                           ylab = "Latitude",
                           legend.mar = 3.1,
                           axis.args = list(at = atp,
                                            labels = labp,
                                            las = 0,
                                            cex.axis = 0.67))
  if (!is.null(naband_reclass)) {
  raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
  }

  on.exit(graphics::par(op))
}
