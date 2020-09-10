#' Prepare log relative risk values for plotting with a diverging color palette
#' 
#' Internal function to convert \code{im} object to values readable by \code{\link[fields]{image.plot}} function within the \code{\link{plot_obs}} function. 
#' 
#' @param input An object of class "rrs" from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length three (3) specifying the colors for plotting: 1) presense, 2) neither, and 3) absence from the \code{\link{plot_obs}} function. 
#' @param midpoint Numeric. The value to center the diverging color palette. 
#' @param thresh_up Numeric. The upper value to concatonate the color key. The default (NULL) uses the maximum value from \code{input}.
#' @param thresh_low Numeric. The lower value to concatonate the color key. The default (NULL) uses the minimum value from \code{input}.
#' @param digits Integer. The number of significant digits for the labels using the \code{round} function (default is 1).
#'
#' @return An object of class "list". This is a named list with the following components:
#' 
#' \describe{
#' \item{\code{v}}{An object of class 'vector' for the estimated ecological niche values.}
#' \item{\code{cols}}{An object of class 'vector', returns diverging color palette values.}
#' \item{\code{breaks}}{An object of class 'vector', returns diverging color palette breaks.}
#' \item{\code{at}}{An object of class 'vector', returns legend breaks.}
#' \item{\code{labels}}{An object of class 'vector', returns legend labels.}
#' }
#' 
#' @importFrom grDevices colorRampPalette
#' @importFrom raster raster
#' @importFrom sp coordinates gridded
#' @export
#' @keywords internal

lrr_plot <- function(input, 
                     cols, 
                     midpoint = 0, 
                     thresh_up = NULL,
                     thresh_low = NULL,
                     digits = 1) {

  # Inputs
  if (class(input) != "im") {
    stop("The 'input' argument must be of class 'im' from an 'rrs' object from the 'sparr' package")
  }

  if (length(cols) != 3) {
    stop("The 'cols' argument must be a vector of length 3")
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

  # Restrict spurious log relative risk values
  if (!is.null(thresh_low)) {
    out[out <= thresh_low] <- thresh_low
  }
  if (!is.null(thresh_up)) {
    out[out >= thresh_up] <- thresh_up
  }

  # Identify ramp above and below midpoint
  lowerhalf <- length(out[out < midpoint & !is.na(out)]) # values below 0
  upperhalf <- length(out[out > midpoint & !is.na(out)]) # values above 0
  nhalf <- length(out[!is.na(out)]) / 2 # number of values at half
  min_absolute_value <- min(out[is.finite(out)], na.rm = TRUE) # minimum absolute value of raster
  max_absolute_value <- max(out[is.finite(out)], na.rm = TRUE) # maximum absolute value of raster

  # Color ramp parameters
  ## Colors
  ### vector of colors for values below midpoint
  rc1 <- grDevices::colorRampPalette(colors = c(cols[3], cols[2]), space = "Lab")(lowerhalf)
  ### vector of colors for values above midpoint
  rc2 <- grDevices::colorRampPalette(colors = c(cols[2], cols[1]), space = "Lab")(upperhalf)
  ### compile colors
  rampcols <- c(rc1, rc2)
  ## Breaks
  ### vector of breaks for values below midpoint
  rb1 <- seq(min_absolute_value, midpoint, length.out = lowerhalf + 1)
  ### vector of breaks for values above midpoint
  rb2 <- seq(midpoint, max_absolute_value, length.out = upperhalf + 1)[-1]
  ### compile breaks
  rampbreaks <- c(rb1, rb2)

  # At for colorkey lables
  rbr <- max_absolute_value - min_absolute_value
  rbt <- rbr / 4
  rbs <- seq(min_absolute_value, max_absolute_value, rbt)
  rbm <- which.min(abs(rbs - midpoint))
  rbs[rbm] <- midpoint

  # Text for colorkey labels
  rbl <- round(rbs, digits = digits)

  # Output
  out <- list("v" = out$v,
              "cols" = rampcols,
              "breaks" = rampbreaks,
              "at" = rbs,
              "labels" = rbl)
}
