#' Prepare an 'im' or 'raster' object for plotting with sequential color palette
#' 
#' Internal function to convert 'im' object or 'RasterLayer' object to values readable by \code{\link[fields]{image.plot}} function within the \code{\link{plot_perturb}} function. 
#' 
#' @param input An object of class 'im' or 'RasterLayer' from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length three (3) specifying the colors for plotting: 1) presence, 2) neither, and 3) absence from the \code{\link{plot_obs}} function. 
#' @param thresh_up Numeric. The upper value to concatenate the color key. The default (NULL) uses the maximum value from \code{input}.
#' @param digits Integer. The number of significant digits for the labels using the \code{round} function (default is 1).
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
#' 
#' @keywords internal
#' 
seq_plot <- function(input,
                     cols,
                     thresh_up = NULL,
                     digits = 1) {
  
  # Inputs
  if (class(input) == "im") {
    out <- raster::raster(input)
  } else { out <- input }
  
  # Restrict spurious standard deviation values
  if (!is.null(thresh_up)) {
    out[out >= thresh_up] <- thresh_up
  }
  
  max_absolute_value <- max(out[is.finite(out)], na.rm = TRUE) # maximum absolute value of raster
  ncols <- length(out[!is.na(out)]) # number of values 
  
  ## Colors
  rampcols <- grDevices::colorRampPalette(colors = c(cols[2], cols[1]), space = "Lab")(ncols)
  
  ## Breaks
  rampbreaks <- seq(0, max_absolute_value, length.out = length(rampcols) + 1)
  
  # At for colorkey lables
  rbr <- max_absolute_value
  rbt <- rbr / 4
  rbs <- seq(0, max_absolute_value, rbt)
  
  # Text for colorkey labels
  rbl <- round(rbs, digits = digits)
  
  # Output
  out <- list("v" = out,
              "cols" = rampcols,
              "breaks" = rampbreaks,
              "at" = rbs,
              "labels" = rbl)
}
