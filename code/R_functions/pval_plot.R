pval_plot <- function(input, alpha) {

  # Inputs
  if (class(input) != "im") {
    stop("The 'input' argument must be of class 'im' from the 'sparr' package")
  }

  # Coordinates of grid points within input 'im'
  rx <- rep(input$xcol, length(input$yrow))
  for(i in 1:length(input$yrow)) {
    if (i == 1) {ry <- rep(input$yrow[i], length(input$xcol))}
    if (i != 1) {ry <- c(ry, rep(input$yrow[i], length(input$xcol)))}
  }

  out <- dplyr::data_frame(x = rx, y = ry, v = as.vector(t(input$v)))
  out$v <- ifelse(is.infinite(out$v), NA, out$v)
  out <- na.omit(out) # remove NAs
  sp::coordinates(out) <- ~ x + y # convert to spatialpixelsdataframe
  sp::gridded(out) <- TRUE # gridded
  out <- raster::raster(out)  # create raster
  out <- raster::cut(out,
                     breaks = c(-Inf, alpha/2, 1-alpha/2, Inf),
                     right = FALSE)
  return(out)
}
