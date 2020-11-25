#' Visualizations for a predicted ecological niche in geographic space
#' 
#' Create multiple plots of output from the \code{\link{lrren}} function, specifically for the predicted values of the ecological niche at geographic coordinates.
#' 
#' @param input An object of class 'list' from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length four (4) specifying the colors for plotting: 1) presence, 2) neither, 3) absence, and 4) NA values. The default colors in hex are \code{c("#8B3A3A", "#CCCCCC", "#0000CD" "#FFFF00")} or \code{c("indianred4", "grey80", "blue3", "yellow")}.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default is 0.05).
#' @param cref0 Character. The Coordinate Reference System (CRS) for the x- and y-coordinates in geographic space. The default is WGS84 \code{"+init=epsg:4326"}.
#' @param cref1 Optional, character. The Coordinate Reference System (CRS) to spatially project the x- and y-coordinates in geographic space. 
#' @param lower_lrr Optional, numeric. Lower cut-off value for the log relative risk value in the color key (typically a negative value). The default is no limit and the color key will include the minimum value of the log relative risk surface. 
#' @param upper_lrr Optional, numeric. Upper cut-off value for the log relative risk value in the color key (typically a positive value). The default is no limit and the color key will include the maximum value of the log relative risk surface.
#' @param digits Optional, integer. The number of significant digits for the color key labels using the \code{\link[base]{round}} function (default is 1).
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
#' if (interactive()) {
#'   set.seed(1234) # for reproducibility
#'
#' # Using the 'bei' and 'bei.extra' data within {spatstat.data}
#' 
#' # Covariate data (centered and scaled)
#'   elev <- spatstat.data::bei.extra[[1]]
#'   grad <- spatstat.data::bei.extra[[2]]
#'   elev$v <- scale(elev)
#'   grad$v <- scale(grad)
#'   elev_raster <- raster::raster(elev)
#'   grad_raster <- raster::raster(grad)
#' 
#' # Presence data
#'   presence <- spatstat.data::bei
#'   spatstat::marks(presence) <- data.frame("presence" = rep(1, presence$n),
#'                                           "lon" = presence$x,
#'                                           "lat" = presence$y)
#'   spatstat::marks(presence)$elev <- elev[presence]
#'   spatstat::marks(presence)$grad <- grad[presence]
#' 
#' # (Pseudo-)Absence data
#'   absence <- spatstat::rpoispp(0.008, win = elev)
#'   spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
#'                                               "lon" = absence$x,
#'                                               "lat" = absence$y)
#'   spatstat::marks(absence)$elev <- elev[absence]
#'   spatstat::marks(absence)$grad <- grad[absence]
#' 
#' # Combine into readable format
#'   obs_locs <- spatstat::superimpose(presence, absence, check = FALSE)
#'   obs_locs <- spatstat::marks(obs_locs)
#'   obs_locs$id <- seq(1, nrow(obs_locs), 1)
#'   obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]
#'   
#' # Prediction Data
#'   predict_locs <- data.frame(raster::rasterToPoints(elev_raster))
#'   predict_locs$layer2 <- raster::extract(grad_raster, predict_locs[, 1:2])
#' 
#' # Run lrren
#'   test_lrren <- lrren(obs_locs = obs_locs,
#'                       predict_locs = predict_locs,
#'                       predict = TRUE,
#'                       cv = TRUE)
#'                       
#' # Run plot_predict
#'   plot_predict(input = test_lrren, cref0 = "+init=epsg:5472")
#' }
#' 
plot_predict <- function(input,
                         plot_cols = c("#8B3A3A", "#CCCCCC", "#0000CD", "#FFFF00"),
                         alpha = 0.05,
                         cref0 = "+init=epsg:4326",
                         cref1 = NULL,
                         lower_lrr = NULL,
                         upper_lrr = NULL,
                         digits = 1,
                         ...) {
  
  if (alpha >= 1 | alpha <= 0) {
    stop("The argument 'alpha' must be a numeric value between 0 and 1")
  }
  
  if (length(plot_cols) != 4) { 
    stop("The argument 'plot_cols' must have 4 colors")
    }

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
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
  rrp <- div_plot(input = predict_risk_raster,
                  cols = plot_cols[1:3],
                  midpoint = 0,
                  thresh_low = lower_lrr,
                  thresh_up = upper_lrr,
                  digits = digits)

  graphics::par(pty = "s")
  p1 <- fields::image.plot(rrp$v,
                           breaks = rrp$breaks,
                           col = rrp$cols,
                           axes = TRUE,
                           main = "log relative risk",
                           xlab = "longitude",
                           ylab = "latitude",
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
    labp <- "insignificant"
  } else {
    pcols <- plot_cols[1:3]
    brp <- c(1, 1.67, 2.33, 3)
    atp <- c(1.33, 2, 2.67)
    labp <- c("presence", "insignificant", "absence")
  }

  p2 <- fields::image.plot(reclass_tol,
                           breaks = brp,
                           col = pcols,
                           axes = TRUE,
                           main = paste("significant p-values\nalpha =", alpha, sep = " "),
                           xlab = "longitude",
                           ylab = "latitude",
                           legend.mar = 3.1,
                           axis.args = list(at = atp,
                                            labels = labp,
                                            las = 0,
                                            cex.axis = 0.67))
  if (!is.null(naband_reclass)) {
  raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
  }
}
