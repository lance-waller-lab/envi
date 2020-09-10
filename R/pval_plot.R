#' Prepare significant p-values for plotting
#' 
#' Internal function to convert \code{im} object to values readable by \code{\link[fields]{image.plot}} function within the \code{\link{plot_obs}} function. 
#' 
#' @param input An object of class "rrs" from the \code{\link{lrren}} function.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default in \code{\link{plot_obs}} is 0.05).
#'
#' @return An object of class "raster" with categorical values
#' 
#' @importFrom raster cut raster
#' @importFrom sp coordinates gridded
#'
#' @keywords internal

pval_plot <- function(input, alpha) {

  # Inputs
  if (class(input) != "im") {
    stop("The 'input' argument must be of class 'im' from the 'sparr' package")
  }

  # Coordinates of grid points within input 'im'
  rx <- rep(input$xcol, length(input$yrow))
  for(i in 1:length(input$yrow)) {
    if (i == 1) { ry <- rep(input$yrow[i], length(input$xcol)) }
    if (i != 1) { ry <- c(ry, rep(input$yrow[i], length(input$xcol))) }
  }

  out <- data.frame("x" = rx,
                    "y" = ry,
                    "v" = as.vector(t(input$v)))
  out$v <- ifelse(is.infinite(out$v), NA, out$v)
  out <- na.omit(out) # remove NAs
  sp::coordinates(out) <- ~ x + y # convert to spatialpixelsdataframe
  sp::gridded(out) <- TRUE # gridded
  out <- raster::raster(out)  # create raster
  out <- raster::cut(out,
                     breaks = c(-Inf, alpha / 2, 1 - alpha / 2, Inf),
                     right = FALSE)
  return(out)
}
