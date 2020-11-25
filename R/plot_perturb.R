#' Visualizations for a simulated ecological niche after iteratively perturbing the observation coordinates
#' 
#' Create multiple plots of output from the \code{\link{perlrren}} function, specifically for the four summary statistics in covariate space and geographic space.
#' 
#' @param input An object of class 'list' from the \code{\link{perlrren}} function.
#' @param predict Logical. If TRUE (the default), will visualize the four summary statistics in geographic space. If FALSE, will not.  
#' @param mean_cols Character string of length three (3) specifying the colors for plots with a divergent color palette: 1) presence, 2) neither, and 3) absence. The default colors in hex are \code{c("#8B3A3A", "#CCCCCC", "#0000CD")} or \code{c("indianred4", "grey80", "blue3")}.
#' @param var_cols Character string of length two (2) specifying the colors for plots with a sequential color palette from low to high values. The default colors in hex are \code{c("#E5E5E5", "#1A1A1A")} or \code{c("grey90", "grey10")}.
#' @param cov_labs Character string of length two (2) specifying the x- and y-axis labels in plots of the ecological niche in covariate space. The default values are generic \code{c("V1", "V2")}. 
#' @param cref0 Character. The Coordinate Reference System (CRS) for the x- and y-coordinates in geographic space. The default is WGS84 \code{"+init=epsg:4326"}.
#' @param cref1 Optional, character. The Coordinate Reference System (CRS) to spatially project the x- and y-coordinates in geographic space.
#' @param lower_lrr Optional, numeric. Lower cut-off value for the log relative risk value in the color key (typically a negative value). The default is no limit and the color key will include the minimum value of the log relative risk surface. 
#' @param upper_lrr Optional, numeric. Upper cut-off value for the log relative risk value in the color key (typically a positive value). The default is no limit and the color key will include the maximum value of the log relative risk surface.
#' @param upper_sd Optional, numeric. Upper cut-off value for the standard deviation of log relative risk value in the color key. The default is no limit and the color key will include the maximum value of the standard deviation surface.
#' @param digits Optional, integer. The number of significant digits for the color key labels using the \code{\link[base]{round}} function (default is 1).
#' @param ... Arguments passed to \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces four plots in a two-dimensional space where the axes are the two specified covariates: 1) mean of the log relative risk, 2) standard deviation of the log relative risk, 3) mean of the asymptotically normal p-value, and 4) proportion of iterations were statistically significant based on a two-tailed alpha-level threshold. If \code{predict = TRUE}, this function produces an additional four plots of the summary statistics above in a two-dimensional geographic space where the axes are longitude and latitude.
#' 
#' @importFrom fields image.plot
#' @importFrom graphics par
#' @importFrom raster crs raster projectRaster
#' @importFrom spatstat pixellate
#' 
#' @export
#' 
#' @examples 
#' if (interactive()) {
#'   set.seed(1234) # for reproducibility
#' 
#' # Using the 'bei' and 'bei.extra' data within {spatstat.data}
#' 
#' # Covariate data (centered and scaled)
#'   ims <- spatstat.data::bei.extra
#'   ims[[1]]$v <- scale(ims[[1]]$v)
#'   ims[[2]]$v <- scale(ims[[2]]$v)
#'   
#' # Presence data
#'   presence <- spatstat.data::bei
#'   spatstat::marks(presence) <- data.frame("presence" = rep(1, presence$n),
#'                                               "lon" = presence$x,
#'                                               "lat" = presence$y)
#'                                           
#' # (Pseudo-)Absence data
#'   absence <- spatstat::rpoispp(0.008, win = ims[[1]])
#'   spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
#'                                               "lon" = absence$x,
#'                                               "lat" = absence$y)
#' # Combine into readable format
#'   obs_locs <- spatstat::superimpose(presence, absence, check = FALSE)
#'   spatstat::marks(obs_locs)$id <- seq(1, obs_locs$n, 1)
#'   spatstat::marks(obs_locs) <- spatstat::marks(obs_locs)[ , c(4, 2, 3, 1)]
#'  
#' # Specify categories for varying degrees of spatial uncertainty
#' ## Creates three groups
#'   spatstat::marks(obs_locs)$levels <- as.factor(stats::rpois(obs_locs$n,
#'                                                                   lambda = 0.05))
#'                                                                   
#' # Run perlrren
#'   test_perlrren <- perlrren(obs_ppp = obs_locs,
#'                             covariates = ims,
#'                             radii = c(10, 100, 500),
#'                             n_sim = 10)
#'                             
#' # Run plot_perturb                             
#'   plot_perturb(input = test_perlrren)
#' }
#' 
plot_perturb <- function(input,
                         predict = TRUE,
                         mean_cols = c("#8B3A3A", "#CCCCCC", "#0000CD"),
                         var_cols = c("#E5E5E5", "#1A1A1A"),
                         cov_labs = c("V1", "V2"),
                         cref0 = "+init=epsg:4326",
                         cref1 = NULL,
                         lower_lrr = NULL,
                         upper_lrr = NULL,
                         upper_sd = NULL,
                         digits = 1,
                         ...) {
  
  if (is.null(input)) { 
    stop("The argument 'input' must be the output from perlrren function")
  }
  
  if (length(mean_cols) != 3) { 
    stop("The argument 'mean_cols' must have 3 colors")
  }
  
  if (length(var_cols) != 2) { 
    stop("The argument 'var_cols' must have 2 colors")
  }
  
  if (length(cov_labs) != 2) { 
    stop("The argument 'cov_labs' must have 2 labels")
  }
  
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::par(pty = "s")
  
  # Plot 1: mean log relative risk
  mlrr <- div_plot(input = input$sim$lrr_mean,
                   cols = mean_cols,
                   midpoint = 0,
                   thresh_low = lower_lrr,
                   thresh_up = upper_lrr,
                   digits = digits)
  p1 <- fields::image.plot(mlrr$v,
                           breaks = mlrr$breaks,
                           col = mlrr$cols,
                           axes = TRUE,
                           main = "mean of\nlog relative risk",
                           xlab = cov_labs[1],
                           ylab = cov_labs[2],
                           legend.mar = 3.1,
                           axis.args = list(at = mlrr$at,
                                            las = 0,
                                            labels = mlrr$labels,
                                            cex.axis = 0.67),
                           ...)
  
  # Plot 2: standard deviation of log relative risk
  sdlrr <- seq_plot(input = input$sim$lrr_sd,
                    cols = var_cols,
                    thresh_up = upper_sd,
                    digits = digits)
  p2 <- fields::image.plot(sdlrr$v,
                           breaks = sdlrr$breaks,
                           col = sdlrr$cols,
                           axes = TRUE,
                           main = "standard deviation of\nlog relative risk",
                           xlab = cov_labs[1],
                           ylab = cov_labs[2],
                           legend.mar = 3.1,
                           axis.args = list(at = sdlrr$at,
                                            las = 0,
                                            labels = sdlrr$labels,
                                            cex.axis = 0.67),
                           ...)
  
  # Plot 3: mean p-value
  mpval <- div_plot(input = input$sim$pval_mean,
                    cols = rev(mean_cols),
                    midpoint = 0.5,
                    digits = digits)
  p3 <- fields::image.plot(mpval$v,
                           breaks = mpval$breaks,
                           col = mpval$cols,
                           axes = TRUE,
                           main = "mean of\nasymptotic normal p-value",
                           xlab = cov_labs[1],
                           ylab = cov_labs[2],
                           legend.mar = 3.1,
                           axis.args = list(at = mpval$at,
                                            las = 0,
                                            labels = mpval$labels,
                                            cex.axis = 0.67),
                           ...)
  
  # Plot 4: proportion of simulations significant p-value
  ppval <- seq_plot(input = input$sim$pval_prop,
                    cols = var_cols,
                    digits = digits)
  p4 <- fields::image.plot(ppval$v,
                           breaks = ppval$breaks,
                           col = ppval$cols,
                           axes = TRUE,
                           main = "proportion of iterations\nwith significant p-value",
                           xlab = cov_labs[1],
                           ylab = cov_labs[2],
                           legend.mar = 3.1,
                           axis.args = list(at = ppval$at,
                                            las = 0,
                                            labels = ppval$labels,
                                            cex.axis = 0.67),
                           ...)
  
  if (predict == TRUE) {
    
    # Convert 'im' objects to spatially projected 'RasterLayer' objects
    lrr_mean <- spatstat::pixellate(input$predict,
                                         weights = marks(input$predict)$lrr_mean)
    lrr_mean <- raster::raster(lrr_mean)
    raster::crs(lrr_mean) <- cref0
    if (!is.null(cref1)) {
      lrr_mean <- raster::projectRaster(lrr_mean,
                                        crs = cref1,
                                        method = "ngb")
    }
    
    lrr_sd <- spatstat::pixellate(input$predict,
                                       weights = marks(input$predict)$lrr_sd)
    lrr_sd <- raster::raster(lrr_sd)
    raster::crs(lrr_sd) <- cref0
    if (!is.null(cref1)) {
      lrr_sd <- raster::projectRaster(lrr_sd,
                                      crs = cref1,
                                      method = "ngb")
    }
    
    pval_mean <- spatstat::pixellate(input$predict,
                                          weights = marks(input$predict)$pval_mean)
    pval_mean <- raster::raster(pval_mean)
    raster::crs(pval_mean) <- cref0
    if (!is.null(cref1)) {
      pval_mean <- raster::projectRaster(pval_mean,
                                         crs = cref1,
                                         method = "ngb")
    }
    
    pval_prop <- spatstat::pixellate(input$predict,
                                          weights = marks(input$predict)$pval_prop)
    pval_prop <- raster::raster(pval_prop)
    raster::crs(pval_prop) <- cref0
    if (!is.null(cref1)) {
      pval_prop <- raster::projectRaster(pval_prop,
                                         crs = cref1,
                                         method = "ngb")
    }
    
    # Plot 5: mean log relative risk
    mlrr <- div_plot(input = lrr_mean,
                     cols = mean_cols,
                     midpoint = 0,
                     thresh_low = lower_lrr,
                     thresh_up = upper_lrr,
                     digits = digits)
    p5 <- fields::image.plot(mlrr$v,
                             breaks = mlrr$breaks,
                             col = mlrr$cols,
                             axes = TRUE,
                             main = "mean of\nlog relative risk",
                             xlab = "longitude",
                             ylab = "latitude",
                             legend.mar = 3.1,
                             axis.args = list(at = mlrr$at,
                                              las = 0,
                                              labels = mlrr$labels,
                                              cex.axis = 0.67),
                             ...)
    
    # Plot 6: standard deviation of log relative risk
    sdlrr <- seq_plot(input = lrr_sd,
                      cols = var_cols,
                      thresh_up = upper_sd,
                      digits = digits)
    p6 <- fields::image.plot(sdlrr$v,
                             breaks = sdlrr$breaks,
                             col = sdlrr$cols,
                             axes = TRUE,
                             main = "standard deviation of\nlog relative risk",
                             xlab = "longitude",
                             ylab = "latitude",
                             legend.mar = 3.1,
                             axis.args = list(at = sdlrr$at,
                                              las = 0,
                                              labels = sdlrr$labels,
                                              cex.axis = 0.67),
                             ...)
    
    # Plot 7: mean p-value
    mpval <- div_plot(input = pval_mean,
                      cols = rev(mean_cols),
                      midpoint = 0.5,
                      digits = digits)
    p7 <- fields::image.plot(mpval$v,
                             breaks = mpval$breaks,
                             col = mpval$cols,
                             axes = TRUE,
                             main = "mean of\nasymptotic normal p-value",
                             xlab = "longitude",
                             ylab = "latitude",
                             legend.mar = 3.1,
                             axis.args = list(at = mpval$at,
                                              las = 0,
                                              labels = mpval$labels,
                                              cex.axis = 0.67),
                             ...)
    
    # Plot 8: proportion of simulations significant p-value
    ppval <- seq_plot(input = pval_prop,
                      cols = var_cols,
                      digits = digits)
    p8 <- fields::image.plot(ppval$v,
                             breaks = ppval$breaks,
                             col = ppval$cols,
                             axes = TRUE,
                             main = "proportion of iterations with\nsignificant p-value",
                             xlab = "longitude",
                             ylab = "latitude",
                             legend.mar = 3.1,
                             axis.args = list(at = ppval$at,
                                              las = 0,
                                              labels = ppval$labels,
                                              cex.axis = 0.67),
                             ...)
  }
}
