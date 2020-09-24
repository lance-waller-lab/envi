#' Prepare an 'im' or 'raster' object for plotting with diverging color palette
#' 
#' Internal function to convert 'im' object or 'RasterLayer' object to values readable by \code{\link[fields]{image.plot}} function within the \code{\link{plot_obs}}, \code{\link{plot_predict}}, and \code{\link{plot_perturb}} functions. 
#' 
#' @param input An object of class 'im' or 'RasterLayer' from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length three (3) specifying the colors for plotting: 1) presence, 2) neither, and 3) absence from the \code{\link{plot_obs}} function. 
#' @param midpoint Numeric. The value to center the diverging color palette. 
#' @param thresh_up Numeric. The upper value to concatenate the color key. The default (NULL) uses the maximum value from \code{input}.
#' @param thresh_low Numeric. The lower value to concatenate the color key. The default (NULL) uses the minimum value from \code{input}.
#' @param digits Integer. The number of significant digits for the labels using the \code{\link[base]{round}} function (default is 1).
#'
#' @return An object of class 'list'. This is a named list with the following components:
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
#'
#' @keywords internal

div_plot <- function(input, 
                     cols, 
                     midpoint = 0, 
                     thresh_up = NULL,
                     thresh_low = NULL,
                     digits = 1) {

  # Inputs
  if (class(input) == "im") {
    out <- raster::raster(input)
  } else { out <- input }

  if (length(cols) != 3) {
    stop("The 'cols' argument must be a vector of length 3")
  }
  
  min_raw_value <- min(out[is.finite(out)], na.rm = TRUE) # minimum absolute value of raster
  max_raw_value <- max(out[is.finite(out)], na.rm = TRUE) # maximum absolute value of raster
  
  # Restrict spurious log relative risk values
  if (!is.null(thresh_low)) {
    if (thresh_low >= 0) {
      stop("The 'thresh_low' argument must be a numeric value less than zero")
    }
    out[out <= thresh_low] <- thresh_low
  }
  if (!is.null(thresh_up)) {
    if (thresh_up <= 0) {
      stop("The 'thresh_up' argument must be a numeric value greater than zero")
    }
    out[out >= thresh_up] <- thresh_up
  }

  # Identify ramp above and below midpoint
  lowerhalf <- length(out[out < midpoint & !is.na(out)]) # values below 0
  upperhalf <- length(out[out > midpoint & !is.na(out)]) # values above 0
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
  
  if (min_raw_value < min_absolute_value) { rbl[1] <- paste("<", rbl[1], sep = "") }
  
  if (max_raw_value > max_absolute_value) { rbl[5] <- paste(">", rbl[5], sep = "") }

  # Output
  out <- list("v" = out,
              "cols" = rampcols,
              "breaks" = rampbreaks,
              "at" = rbs,
              "labels" = rbl)
}
