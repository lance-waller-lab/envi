# The combine function used in foreach
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

# Convert an object of class 'imlist' to an object of class 'data.frame'
ims2df <- function(ims) {
  rx <- rep(ims[[1]]$xcol, length(ims[[1]]$yrow))
  for(i in 1:length(ims[[1]]$yrow)) {
    if (i == 1) { ry <- rep(ims[[1]]$yrow[i], length(ims[[1]]$xcol)) }
    if (i != 1) { ry <- c(ry, rep(ims[[1]]$yrow[i], length(ims[[1]]$xcol))) }
  }
  out <- data.frame("x" = rx,
                    "y" = ry,
                    "v" = as.vector(t(ims[[1]]$v)))
  out$v <- ifelse(is.infinite(out$v), NA, out$v)
  out$z <- as.vector(t(ims[[2]]$v))
  out$z <- ifelse(is.infinite(out$z), NA, out$z)
  return(out)
}

# Progress bar used in foreach
progBar <- function(kk, N, per = 1) {
  if (kk %in% seq(1, N, per)) {
    x <- round(kk * 100 / N)
    message("[ ", 
            paste(rep("=", x), collapse = ""),
            paste(rep("-", 100 - x), collapse = ""), 
            " ] ", x, "%", "\r",
            appendLF = FALSE)
    if (kk == N) cat("\r")
  }
}

# Temporary stopgap for converting 'SpatRaster' to 'im'
## Modification of `as.im.RasterLayer` from `maptools` because `maptools` is retiring in 2023
as.im.SpatRaster <- function(from, factor.col.name = NULL) {
  if (!requireNamespace("spatstat.geom", quietly = TRUE)) {
    stop(paste("package spatstat.geom required; please install it (or the full spatstat package) first"))
  }
  else {
    spst_ver <- try(utils::packageVersion("spatstat"), silent = TRUE)
    if (!inherits(spst_ver, "try-error") && spst_ver < 2 - 
        0) {
      stop(paste("You have an old version of spatstat installed which is", 
                 " incompatible with spatstat.geom. Please update spatstat (or uninstall it).", 
                 sep = ""))
    }
  }
  if (!requireNamespace("sf", quietly = TRUE)) 
    stop("package sf required for coercion")
  if (!requireNamespace("terra", quietly = TRUE)) 
    stop("package terra required for coercion")
  if (!terra::hasValues(from)) 
    stop("values required in SpatRaster object")
  rs <- terra::res(from)
  orig <- sf::st_bbox(from)[1:2] + 0.5 * rs
  dm <- dim(from)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2] - 1))))
  val <- terra::values(from)
  if (is.factor(from)) {
    lev <- levels(from)[[1]]
    if (!is.null(factor.col.name)) {
      if (factor.col.name %in% colnames(lev)) {
        factor.col <- which(colnames(lev) == factor.col.name)
      }
      else {
        stop("'factor.col.name' is not a column name of the SpatRaster 'from'")
      }
    }
    else {
      factor.col <- length(lev)
    }
    val <- factor(val, levels = lev$ID, labels = lev[[factor.col]])
  }
  dim(val) <- dm
  val <- spatstat.geom::transmat(val, from = list(x = "-i", 
                                                  y = "j"), to = "spatstat")
  im <- spatstat.geom::im(val, xcol = xx, yrow = yy)
  return(im)
}

# False Discovery Rate (Benjamini & Hochberg)
fdr <- function(pvals, alpha) {
  pcrit <- NULL
  m <- length(pvals)
  for (i in 1:m) {
    if (pvals[i] <= (i/m) * alpha) { 
      pcrit <- pvals[i]
    }
  }
  return(max(pcrit, pvals[1]))
}
