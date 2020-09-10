#' Prepare raster for plotting with a diverging color palette
#' 
#' Internal function to convert \code{raster} object to values readable by \code{\link[fields]{image.plot}} function within the \code{\link{plot_predict}} function. 
#' 
#' @param input An object of class "rrs" from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length three (3) specifying the colors for plotting: 1) presense, 2) neither, and 3) absence from the \code{\link{plot_predict}} function. 
#' @param midpoint Numeric. The value to center the diverging color palette. 
#' @param thresh_up Numeric. The upper value to concatonate the color key. The default (NULL) uses the maximum value from \code{input}.
#' @param thresh_low Numeric. The lower value to concatonate the color key. The default (NULL) uses the minimum value from \code{input}.
#' @param digits Integer. The number of significant digits for the labels using the \code{round} function (default is 1).
#'
#' @return An object of class "list". This is a named list with the following components:
#' 
#' \describe{
#' \item{\code{v}}{An object of class 'vector' for the predicted ecological niche values.}
#' \item{\code{cols}}{An object of class 'vector', returns diverging color palette values.}
#' \item{\code{breaks}}{An object of class 'vector', returns diverging color palette breaks.}
#' \item{\code{at}}{An object of class 'vector', returns legend breaks.}
#' \item{\code{labels}}{An object of class 'vector', returns legend labels.}
#' }
#' 
#' @importFrom grDevices colorRampPalette
#' @importFrom raster raster
#' @importFrom sp coordinates gridded
#' @import maptools
#'
#' @keywords internal

lrr_raster <- function(input,
                       cols,
                       midpoint = 0,
                       thresh_up = NULL,
                       thresh_low = NULL,
                       digits = 1) {

  # Inputs
  if (class(input) != "RasterLayer") {
    stop("The 'input' argument must be of class 'RasterLayer'")
  }

  if (length(cols) != 3) {
    stop("The 'cols' argument must be a vector of length 3")
  }

  # Identify ramp above and below midpoint
  lowerhalf <- length(input@data@values[input@data@values < midpoint & !is.na(input@data@values)]) # values below 0
  upperhalf <- length(input@data@values[input@data@values > midpoint & !is.na(input@data@values)]) # values above 0
  nhalf <- length(input@data@values[!is.na(input@data@values)]) / 2 # number of values at half
  min_absolute_value <- min(input@data@values[is.finite(input@data@values)], na.rm = TRUE) # minimum absolute value of raster
  max_absolute_value <- max(input@data@values[is.finite(input@data@values)], na.rm = TRUE) # maximum absolute value of raster

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
  out <- list("v" = input$v,
              "cols" = rampcols,
              "breaks" = rampbreaks,
              "at" = rbs,
              "labels" = rbl
  )
}
