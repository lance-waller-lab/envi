#' The envi Package: Environmental interpolation using spatial kernel density estimation
#'
#' Estimates an ecological niche using occurrence data, covariates, and kernel density-based estimation methods.
#'
#' @details
#' 
#' For a single species with presence and absence data, the {envi} package uses the spatial relative risk function that is estimated using the {sparr} package. Details about the {sparr} package methods can be found in the tutorial: Davies et al. (2018) <doi:10.1002/sim.7577>. Details about kernel density estimation can be found in J. F. Bithell (1990) <doi:10.1002/sim.4780090616>.  More information about relative risk functions using kernel density estimation (KDE) can be found in J. F. Bithell (1991) <doi:10.1002/sim.4780101112>.
#' 
#' This package provides a function to estimate the ecological niche for a single species with presence and absence data. The {envi} package also provides some visualization tools for the estimated ecological niche, its predicted spatial distribution, and prediction diagnostics.
#' 
#' Key content of the {envi} package include:\cr
#' 
#' \bold{Ecological Niche Model}
#' 
#' \code{\link{lrren}} Estimates an ecological niche for a single species with presence/absence data, two covariates, and the spatial relative risk function. Provide functionality to predict the spatial distribution of the estimated ecological niche in geographic space and prepare internal k-fold cross-validation data.
#' 
#' \bold{Data Visualization}
#' 
#' \code{\link{plot_obs}} Visualizes the \code{\link{lrren}} output, specifically the estimated ecological niche in a space with dimensions as the two specified covariates in the model.
#' 
#' \code{\link{plot_predict}} Visualizes the \code{\link{lrren}} output, specifically the predicted spatial distribution of the ecological niche.
#' 
#' \code{\link{plot_cv}} Visualizes the \code{\link{lrren}} output, specifically two prediction diagnostics (area under the receiver operating characteristic curve and precision-recall curve).
#' 
#' @name envi-package
#' @aliases envi-package envi 
#' @docType package
#' 
#' @section Dependencies: The {envi} package depends upon \code{\link{sparr}}, \code{\link{spatstat.core}}, and \code{\link{raster}}. For a single species, the spatial relative risk function uses the \code{\link[sparr]{risk}} function. The cross-validation can be performed using parallelization using \code{\link[doParallel]{doParallel}}, \code{\link[parallel]{parallel}}, and \code{\link[foreach]{foreach}}.
#' 
#' @author Ian D. Buller\cr \emph{Environmental Health Sciences, Emory University, Atlanta, Georgia, USA.}
#' 
#' Maintainer: I.D.B. \email{ian.buller@@alumni.emory.edu}
#'
#' @keywords package
NULL

#' @importFrom concaveman concaveman
#' @importFrom cvAUC ci.cvAUC cvAUC
#' @importFrom doParallel registerDoParallel
#' @importFrom fields image.plot
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom graphics abline layout legend lines mtext par plot plot.new title
#' @importFrom grDevices chull colorRampPalette
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pls cvsegments
#' @importFrom raster crs cut extract image projectRaster raster reclassify values
#' @importFrom rgeos gBuffer
#' @importFrom ROCR performance prediction
#' @importFrom sp bbox coordinates gridded Polygon Polygons SpatialPolygons
#' @importFrom spatstat.core owin ppp setmarks superimpose
#' @importFrom stats na.omit
#' @importFrom utils packageDescription setTxtProgressBar txtProgressBar 
#' @import maptools
NULL
