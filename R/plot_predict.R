#' Visualizations for a predicted ecological niche in geographic space
#' 
#' Create multiple plots of output from the \code{\link{lrren}} function, specifically for the predicted values of the ecological niche at geographic coordinates.
#' 
#' @param input An object of class 'list' from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length four (4) specifying the colors for plotting: 1) presence, 2) neither, 3) absence, and 4) NA values. The default colors in hex are \code{c("#8B3A3A", "#CCCCCC", "#0000CD" "#FFFF00")} or \code{c("indianred4", "grey80", "blue3", "yellow")}.
#' @param alpha Optional, numeric. The two-tailed alpha level for significance threshold (default is the \code{p_critical} value imported from \code{input}).
#' @param cref0 Character. The Coordinate Reference System (CRS) for the x- and y-coordinates in geographic space. The default is WGS84 \code{"EPSG:4326"}.
#' @param cref1 Optional, character. The Coordinate Reference System (CRS) to spatially project the x- and y-coordinates in geographic space. 
#' @param lower_lrr Optional, numeric. Lower cut-off value for the log relative risk value in the color key (typically a negative value). The default is no limit, and the color key will include the minimum value of the log relative risk surface. 
#' @param upper_lrr Optional, numeric. Upper cut-off value for the log relative risk value in the color key (typically a positive value). The default is no limit, and the color key will include the maximum value of the log relative risk surface.
#' @param digits Optional, integer. The number of significant digits for the color key labels using the \code{\link[base]{round}} function (default is 1).
#' @param ... Arguments passed to \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces two plots in a two-dimensional space where the axes are geographic coordinates (e.g., longitude and latitude): 1) predicted log relative risk, and 2) significant p-values. 
#' 
#' @importFrom fields image.plot
#' @importFrom graphics par
#' @importFrom terra crs image project rast classify values
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
#'   elev_raster <- terra::rast(elev)
#'   grad_raster <- terra::rast(grad)
#' 
#' # Presence data
#'   presence <- spatstat.data::bei
#'   spatstat.geom::marks(presence) <- data.frame("presence" = rep(1, presence$n),
#'                                           "lon" = presence$x,
#'                                           "lat" = presence$y)
#'   spatstat.geom::marks(presence)$elev <- elev[presence]
#'   spatstat.geom::marks(presence)$grad <- grad[presence]
#' 
#' # (Pseudo-)Absence data
#'   absence <- spatstat.random::rpoispp(0.008, win = elev)
#'   spatstat.geom::marks(absence) <- data.frame("presence" = rep(0, absence$n),
#'                                               "lon" = absence$x,
#'                                               "lat" = absence$y)
#'   spatstat.geom::marks(absence)$elev <- elev[absence]
#'   spatstat.geom::marks(absence)$grad <- grad[absence]
#' 
#' # Combine into readable format
#'   obs_locs <- spatstat.geom::superimpose(presence, absence, check = FALSE)
#'   obs_locs <- spatstat.geom::marks(obs_locs)
#'   obs_locs$id <- seq(1, nrow(obs_locs), 1)
#'   obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]
#'   
#' # Prediction Data
#'   predict_xy <- terra::crds(elev_raster)
#'   predict_locs <- as.data.frame(predict_xy)
#'   predict_locs$elev <- terra::extract(elev_raster, predict_xy)[ , 1]
#'   predict_locs$grad <- terra::extract(grad_raster, predict_xy)[ , 1]
#' 
#' # Run lrren
#'   test_lrren <- lrren(obs_locs = obs_locs,
#'                       predict_locs = predict_locs,
#'                       predict = TRUE)
#'                       
#' # Run plot_predict
#'   plot_predict(input = test_lrren, cref0 = "EPSG:5472")
#' }
#' 
plot_predict <- function(input,
                         plot_cols = c("#8B3A3A", "#CCCCCC", "#0000CD", "#FFFF00"),
                         alpha = input$p_critical,
                         cref0 = "EPSG:4326",
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
  ## log relative risk
  predict_risk <- data.frame("x" = input$out$predict[ , 1],
                             "y" = input$out$predict[ , 2],
                             "v" = input$out$predict$rr)
  predict_risk_raster <- terra::rast(predict_risk)
  terra::crs(predict_risk_raster) <- cref0
  if (!is.null(cref1)) {
    predict_risk_raster <- terra::project(predict_risk_raster,
                                          y = cref1,
                                          method = "bilinear")
    
  }
  
  ## p-value
  predict_tol <- data.frame("x" = input$out$predict[ , 1],
                            "y" = input$out$predict[ , 2],
                            "v" = input$out$predict$pval)
  predict_tol_raster <- terra::rast(predict_tol)
  terra::crs(predict_tol_raster) <- cref0
  if (!is.null(cref1)) {
    predict_tol_raster <- terra::project(predict_tol_raster,
                                         y = cref1,
                                         method = "bilinear")
  }
  
  terra::values(predict_tol_raster) <- cut(terra::values(predict_tol_raster),
                                           breaks = c(-Inf, alpha / 2, 1 - alpha / 2, Inf),
                                           right = FALSE)
  
  ## Separate layer for NAs (if any)
  naband <- predict_risk
  naband$v <-  ifelse(is.na(naband$v), 9999, NA)
  naband_raster <- terra::rast(naband)
  terra::crs(naband_raster) <- cref0
  if (!is.null(cref1)) {
    naband_raster <- terra::project(naband_raster,
                                         y = cref1,
                                         method = "near")
  }
  
  if (all(is.na(terra::values(naband_raster)))) { naband_raster <- NULL }
  
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
  if (!is.null(naband_raster)) {
  terra::image(naband_raster, y = 1, col = plot_cols[4], add = TRUE)
  }

  # Plot 2: Significant p-values
  if (all(terra::values(predict_tol_raster)[!is.na(terra::values(predict_tol_raster))] == 2)) {
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

  p2 <- fields::image.plot(predict_tol_raster,
                           breaks = brp,
                           col = pcols,
                           axes = TRUE,
                           main = paste("significant p-values\nalpha =", formatC(alpha, format = "e", digits = 2), sep = " "),
                           xlab = "longitude",
                           ylab = "latitude",
                           legend.mar = 3.1,
                           axis.args = list(at = atp,
                                            labels = labp,
                                            las = 0,
                                            cex.axis = 0.67))
  if (!is.null(naband_raster)) {
  terra::image(naband_raster, y = 1, col = plot_cols[4], add = TRUE)
  }
}
