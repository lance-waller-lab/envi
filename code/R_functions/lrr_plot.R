lrr_plot <- function(input, cols, midpoint, thresh_up = NULL, thresh_low = NULL) {

  # Inputs
  if (class(input) != "im") {
    stop("The 'input' argument must be of class 'im' from an 'rrs' object from the 'sparr' package")
  }

  if (length(cols) != 3) {
    stop("The 'cols' argument must be a vector of length 3")
  }

  # Coordinates of grid points within input 'im'
  rx <- rep(input$xcol, length(input$yrow))
  for(i in 1:length(input$yrow)){
    if (i == 1){ry <- rep(input$yrow[i], length(input$xcol))}
    if (i != 1){ry <- c(ry, rep(input$yrow[i], length(input$xcol)))}
  }

  out <- dplyr::data_frame(x = rx, y = ry, v = as.vector(t(input$v)))
  out$v <- ifelse(is.infinite(out$v), NA, out$v)
  out <- na.omit(out) # remove NAs
  sp::coordinates(out) <- ~ x + y # convert to spatialpixelsdataframe
  sp::gridded(out) <- TRUE # gridded
  out <- raster::raster(out)  # create raster

  # Restrict spurious log relative risk values
  if (!is.null(thresh_low)) {
    out[out <= thresh_low] <- thresh_low
  }
  if (!is.null(thresh_up)){
    out[out >= thresh_up] <- thresh_up
  }

  # Identify ramp above and below midpoint
  lowerhalf <- length(out[out < midpoint & !is.na(out)]) # values below 0
  upperhalf <- length(out[out > midpoint & !is.na(out)]) # values above 0
  nhalf <- length(out[!is.na(out)])/2 # number of values at half
  min_absolute_value <- min(out[is.finite(out)], na.rm = T) # minimum absolute value of raster
  max_absolute_value <- max(out[is.finite(out)], na.rm = T) # maximum absolute value of raster

  # Color ramp parameters
  ## Colors
  ### vector of colors for values below midpoint
  rc1 <- grDevices::colorRampPalette(colors = c(cols[3], cols[2]), space="Lab")(lowerhalf)
  ### vector of colors for values above midpoint
  rc2 <- grDevices::colorRampPalette(colors = c(cols[2], cols[1]), space="Lab")(upperhalf)
  ### compile colors
  rampcols <- c(rc1, rc2)
  ### add midpoint color
  #rampcols[c(lowerhalf+1,upperhalf)] <- grDevices::rgb(t(col2rgb(cols[2])), maxColorValue = 256)
  ## Breaks
  ### vector of breaks for values below midpoint
  rb1 <- seq(min_absolute_value, midpoint, length.out = lowerhalf+1)
  ### vector of breaks for values above midpoint
  rb2 <- seq(midpoint, max_absolute_value, length.out = upperhalf+1)[-1]
  ### compile breaks
  rampbreaks <- c(rb1, rb2)

  # At for colorkey lables
  rbr <- max_absolute_value - min_absolute_value
  rbt <- rbr/4
  rbs <- seq(min_absolute_value, max_absolute_value, rbt)
  rbm <- which.min(abs(rbs - midpoint))
  rbs[rbm] <- midpoint

  # Text for colorkey labels
  rbl <- round(rbs, digits = 1)

  # Output
  out <- list("v" = out,
              "cols" = rampcols,
              "breaks" = rampbreaks,
              "at" = rbs,
              "labels" = rbl
              )
}
