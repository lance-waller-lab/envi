#' An ecological niche model using a log relative risk surface
#' 
#' Estimate the ecological niche of a single species with presence/absence data and two covariates. Predict the ecological niche in geographic space.
#'
#' @param obs_locs Input data frame of presence and absence observations with 5 features (columns): 1) ID, 2) longitude, 3) latitude, 4) presence/absence binary variable, 5) covariate 1 as x-coordinate, 6) covariate 2 as y-coordinate
#' @param predict Logical. If TRUE, will predict the ecological niche in geographic space. If FALSE (the default), will not predict. 
#' @param predict_locs Input data frame of prediction locations with 4 features (columns): 1) longitude, 2) latitude, 3) covariate 1 as x-coordinate, 4) covariate 2 as y-coordinate. The covariates must be the same as those included in \code{obs_locs}.
#' @param conserve Logical. If TRUE (the default), the ecological niche will be estimated within a concave hull around the locations in \code{obs_locs}. If FALSE, the ecological niche will be estimated within a concave hull around the locations in \code{predict_locs}.
#' @param cv Logical. If TRUE, will calculate prediction diagnostics using internal n-fold cross-validation. If FALSE (the default), will not. 
#' @param nfold Integer. Specify the number of folds using in the internal cross-validation. Default is 10.
#' @param balance Logical. If TRUE, the prevalence within each n-fold will be 0.50 by undersampling absence locations (assumes absence data are more frequent). If FALSE (the default), the prevalnce within each n-fold will match the prevalence in \code{obs_locs}.
#' @param parallel Logical. If TRUE, will execute the function in parallel. If FALSE (the default), will not execute the function in parallel.
#' @param n_core Optional. Integer specifying the number of CPU cores on current host to use for parallelization (the default is 2 cores).
#' @param poly_buffer Optional. Specify a custom distance (in same units as covariates) to add to window within which the ecological niche is estimated. The default is 1/100th of the smallest range among the two covariates.
#' @param obs_window Optional. Specify a custom window of class \code{owin} within which to estimate the ecological niche. The default computes a concave hull around the data specified in \code{conserve}.
#' @param verbose Logical. If TRUE (the default), will print function progress during execution. If FALSE, will not print.
#' @param ... Arguments passed to \code{\link[sparr]{risk}} to select bandwidth, edge correction, and resolution.
#'
#' @details This function estimates the ecological niche of a single species (presence/absence, or the presence of one species relative to another) using two covariates, will predict the ecological niche into a geographic area, and prepare n-fold cross-validation data sets for prediction diagnostics. 
#' 
#' The function uses the \code{\link[sparr]{risk}} function to estimate the spatial relative risk function and forces the \code{tolerate} argument to be TRUE in order to calculate asymptotic p-values. The estiamted ecologial niche can be visualized using the \code{\link{plot_obs}} function.
#' 
#' If \code{predict = TRUE} this funciton will predict ecological niche at every location specified with \code{predict_locs} with best performance if \code{predict_locs} are gridded locations in the same study area as the observations in \code{obs_locs} - a version of environmental interpolation. The predicted spatial distribution of the estimated ecological niche can be visualized using the \code{\link{plot_prediction}} function.
#' 
#' If \code{cv = TRUE} this function will prepare n-fold cross-validation data sets for prediction diagnostics. The sample size of each fold depends on the number of folds set with \code{nfold}. If \code{balance = TRUE}, the sample size of each fold will be frequency of precence locations divided by number of folds times two. If \code{balance = FALSE}, the sample size of each fold will be frequency of all observed locations divided by number of folds. Two diagnostics (area under the receiver operating characteristic curve and precision-recall curve) can be visualized using the \code{plot_cv}} function.
#' 
#' The \code{obs_window} argument may be useful to specify a 'known' window for the ecological niche (e.g., a convex hull around observed locations).
#' 
#' @return An object of class "list". This is a named list with the following components:
#' 
#' \describe{
#' \item{\code{out}}{An object of class 'list' for the estimated ecological niche.}
#' \item{\code{dat}}{An object of class 'data.frame', returns \code{obs_locs} that are used in the accompanying plotting functions.}
#' }
#' 
#' The returned \code{out} is a named list with the following components:
#' 
#' \describe{
#' \item{\code{obs}}{An object of class 'rrs' for the spatial relative risk.}
#' \item{\code{presence}}{An object of class 'ppp' for the presence locations.}
#' \item{\code{absence}}{An object of class 'ppp' for the absence locations.}
#' \item{\code{outer_poly}}{An object of class 'matrix' for the coordinates of the concave hull around the observation locations.}
#' \item{\code{inner_poly}}{An object of class 'matrix' for the coordinates of the concave hull around the observation locations. Same as \code{outer_poly}.}
#' }
#' 
#' If \code{predict = TRUE} the returned \code{out} has additional components:
#' 
#' \describe{
#' \item{\code{outer_poly}}{An object of class 'matrix' for the coordinates of the concave hull around the prediction locations.}
#' \item{\code{prediction}}{An object of class 'matrix' for the coordinates of the concave hull around the prediction locations.}
#' }
#' 
#' If \code{cv = TRUE} the returned object of class "list" has an additional named list \code{cv} with the following components:
#' 
#' \describe{
#' \item{\code{cv_predictions_rr}}{A list of length \code{nfold} with values of the log relative risk surface at each point randomly selected in a cross-validation fold.}
#' \item{\code{cv_predictions_pval}}{A list of length \code{nfold} with values of the asymptotic tolerance (p-value) surface at each point randomly selected in a cross-validation fold.}
#' \item{\code{cv_labels}}{A list of length \code{nfold} with a binary value of presence (1) or absence (0) for each point randomly selected in a cross-validation fold.}
#' }
#' 
#' @importFrom concaveman concaveman
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom grDevices chull
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pls cvsegments
#' @importFrom raster extract raster
#' @importFrom rgeos gBuffer
#' @importFrom sp bbox coordinates gridded Polygon Polygons SpatialPolygons
#' @importFrom sparr risk
#' @importFrom spatstat.core owin ppp
#' @importFrom stats na.omit
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#'
#' @examples
#' # Using the \code{\link[spatstat.data]{bei}} and \code{\link[bei.extra]{bei.extra}} datasets
#' data(bei)
#' data(bei.extra)
#' 
#' # Environmental Covariates
#' elev <- spatstat.data::bei.extra$elev
#' grad <- spatstat.data::bei.extra$grad
#' elev$v <- scale(elev)
#' grad$v <- scale(grad)
#' elev_raster <- raster::raster(elev)
#' grad_raster <- raster::raster(grad)
#' 
#' # Presence
#' bei <- spatstat.data::bei
#' spatstat::marks(bei) <- data.frame("presence" = rep(1, bei$n),
#'                                    "lon" = bei$x,
#'                                    "lat" = bei$y)
#' spatstat::marks(bei)$elev <- elev[bei]
#' spatstat::marks(bei)$grad <- grad[bei]
#' 
#' # Absence
#' set.seed(1234)
#' absence <- spatstat::rpoispp(0.008, win = elev)
#' spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
#'                                        "lon" = absence$x,
#'                                        "lat" = absence$y)
#' spatstat::marks(absence)$elev <- elev[absence]
#' spatstat::marks(absence)$grad <- grad[absence]
#' 
#' # Combine
#' obs_locs <- spatstat::superimpose(bei, absence, check = FALSE)
#' plot(obs_locs, which.marks = "presence")
#' obs_locs <- spatstat::marks(obs_locs)
#' obs_locs$id <- seq(1, nrow(obs_locs), 1)
#' obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]
#' 
#' # Run lrren
#' test_lrren <- lrren(obs_locs = obs_locs)
#' 
lrren <- function(obs_locs,
                  predict = FALSE,
                  predict_locs = NULL,
                  conserve = TRUE,
                  cv = FALSE,
                  nfold = 10,
                  balance = FALSE,
                  parallel = FALSE,
                  n_core = NULL,
                  poly_buffer = NULL,
                  obs_window = NULL,
                  verbose = FALSE,
                  ...) {

  if (verbose == TRUE) { message("Estimating relative risk surfaces\n") }

  # Compute spatial windows
  ## Calculate inner boundary polygon (extent of presence and absence locations in environmental space)
  inner_chull <- concaveman::concaveman(as.matrix(obs_locs[ , 5:6]))
  inner_chull_pts <- sp::coordinates(inner_chull)
  inner_chull_pts <- rbind(inner_chull_pts, inner_chull_pts[1, ])
  inner_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(inner_chull_pts)), 1)))

  if (is.null(poly_buffer)) {
    poly_buffer <- abs(min(diff(sp::bbox(inner_chull_poly)[1, ]), diff(sp::bbox(inner_chull_poly)[2, ])) / 100)
  }

  # add small buffer around polygon to include boundary points
  inner_chull_poly_buffer <- rgeos::gBuffer(inner_chull_poly, width = poly_buffer, byid = TRUE)
  inner_poly <- inner_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords

  if (is.null(predict_locs)) {
    outer_chull_poly <- inner_chull_poly_buffer
    outer_poly <- inner_poly
  } else {
    ## Calculate outer boundary polygon (full extent of geographical extent in environmental space)
    if (nrow(predict_locs) > 5000000) { # convex hull
      outer_chull <- grDevices::chull(x = stats::na.omit(predict_locs)[ , 3], y = stats::na.omit(predict_locs)[ , 4])
      outer_chull_pts <- predict_locs[c(outer_chull, outer_chull[1]), 3:4]
    } else { # concave hull
      outer_chull <- concaveman::concaveman(as.matrix(stats::na.omit(predict_locs)[ , 3:4]))
      outer_chull_pts <- sp::coordinates(outer_chull)
    }
    outer_chull_pts <- rbind(outer_chull_pts, outer_chull_pts[1, ])
    outer_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(outer_chull_pts)), 1)))
    #add small buffer around polygon to include boundary points
    outer_chull_poly_buffer <- rgeos::gBuffer(outer_chull_poly, width = poly_buffer, byid = TRUE)
    outer_poly <- outer_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon
  }

  if (conserve == TRUE) { window_poly <- inner_poly } else { window_poly <- outer_poly }

  if (is.null(obs_window)) {
    wind <- spatstat.core::owin(poly = list(x = rev(window_poly[ , 1]),
                                       y = rev(window_poly[ , 2])))
  } else { wind <- obs_window }

  # Input Preparation
  ## presence and absence point pattern datasets
  presence_locs <- subset(obs_locs, obs_locs[ , 4] == 1)
  absence_locs <- subset(obs_locs, obs_locs[, 4] == 0)

  ppp_presence <- spatstat.core::ppp(x = presence_locs[ , 5],
                            y = presence_locs[ , 6],
                            window = wind,
                            checkdup = FALSE)
  ppp_absence <- spatstat.core::ppp(x = absence_locs[ , 5],
                               y = absence_locs[ , 6],
                               window = wind,
                               checkdup = FALSE)

  # Calculate observed kernel density ratio
  obs <- sparr::risk(f = ppp_presence, g = ppp_absence,
                         tolerate = TRUE, verbose = verbose, ...)
  bandw <- obs$f$h0

  if (predict == FALSE) {
    output <- list("obs" = obs,
                   "presence" = ppp_presence,
                   "absence" = ppp_absence,
                   "outer_poly" = outer_poly,
                   "inner_poly" = inner_poly)
    } else {
      # Project relative risk surface into geographic space
      if (verbose == TRUE) { message("Predicting area of interest") }

      ## Create index coordinates
      rx <- rep(obs$rr$xcol, length(obs$rr$yrow))
      for(i in 1:length(obs$rr$yrow)) {
        if (i == 1) { ry <- rep(obs$rr$yrow[i], length(obs$rr$xcol)) }
        if (i != 1) { ry <- c(ry, rep(obs$rr$yrow[i], length(obs$rr$xcol))) }
        }

    # Convert to semi-continuous raster
    rr <-  data.frame("x" = rx,
                      "y" = ry,
                      "v" = as.vector(t(obs$rr$v)))
    rr$v <- ifelse(is.infinite(rr$v), NA, rr$v)
    rr <- stats::na.omit(rr) # remove NAs
    sp::coordinates(rr) <- ~ x + y # coordinates
    sp::gridded(rr) <- TRUE # gridded
    rr_raster <- raster::raster(rr)

    # Convert to categorical raster
    pval <-  data.frame("x" = rx,
                        "y" = ry,
                        "v" = as.vector(t(obs$P$v)))
    pval$v <- ifelse(is.infinite(pval$v), NA, pval$v)
    pval <- stats::na.omit(pval) # remove NAs
    sp::coordinates(pval) <- ~ x + y # coordinates
    sp::gridded(pval) <- TRUE # gridded
    pval_raster <- raster::raster(pval)

    # Prediction locations
    extract_points <- cbind(predict_locs[ , 3], predict_locs[ , 4])
    extract_predict <-  data.frame("predict_locs" = predict_locs,
                                          "rr" = raster::extract(rr_raster, extract_points),
                                          "pval" = raster::extract(pval_raster, extract_points))

    output <- list("obs" = obs,
                   "presence" = ppp_presence,
                   "absence" = ppp_absence,
                   "outer_poly" = outer_poly,
                   "inner_poly" = inner_poly,
                   "predict" = extract_predict)
    }

  # K-Fold Cross Validation
  if (cv == FALSE) { cv_results <- NULL
  } else {

    cv_predictions_rank <- list()
    cv_predictions_quant <- list()
    cv_labels <- list()
    cv_pvals <- list()

    ## Combine function used in foreach
    comb <- function(x, ...) {
      lapply(seq_along(x),
             function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
    }

    ## Partition n-folds
    ### Randomly sample data into n-folds
    if (balance == FALSE) {
      cv_segments <- pls::cvsegments(nrow(obs_locs), nfold)
      cv_seg_cas <- NULL
      cv_seg_con <- NULL
    } else {
      cv_seg_cas <-  pls::cvsegments(nrow(presence_locs), nfold)
      cv_seg_con <-  pls::cvsegments(nrow(absence_locs), nfold)
      cv_segments <- NULL
    }

    ### Progress bar
    if (verbose == TRUE & parallel == FALSE) {
      message("Cross-validation in progress")
      pb <- utils::txtProgressBar(min = 0, max = nfold, style = 3)
    }

    ### Set function used in foreach
    if (parallel == TRUE) {
      loadedPackages <- c("doParallel", "parallel")
      invisible(lapply(loadedPackages, require, character.only = TRUE))
      if (is.null(n_core)) { n_core <- parallel::detectCores() - 1 }
      cl <- parallel::makeCluster(n_core)
      doParallel::registerDoParallel(cl)
      `%fun%` <- foreach::`%dopar%`
    } else { `%fun%` <- foreach::`%do%` }

    ### Foreach loop
    out_par <- foreach::foreach(k = 1:nfold,
                                .combine = comb,
                                .multicombine = TRUE,
                                .packages = c("sparr", "spatstat.core", "dplyr", "raster"),
                                .init = list(list(), list(), list())
                                ) %fun% {

      #### Progress bar
      if (verbose == TRUE & parallel == FALSE) { utils::setTxtProgressBar(pb, k) }

      if (balance == FALSE) {
        testing <- obs_locs[cv_segments[k]$V, ]
        training <- obs_locs[-(cv_segments[k]$V), ]
      } else {
        ind <- 1:length(cv_seg_con[k]$V)
        randind <- sample(ind, length(cv_seg_cas[k]$V), replace = FALSE)
        testing_cas <- presence_locs[cv_seg_cas[k]$V, ]
        testing_con <- absence_locs[cv_seg_con[k]$V, ]
        testing_con <- testing_con[randind, ] # undersample the absences for testing
        testing <- rbind(testing_cas,testing_con)
        training_cas <- presence_locs[-(cv_seg_cas[k]$V), ]
        training_con <- absence_locs[-(cv_seg_con[k]$V), ]
        training <- rbind(training_cas,training_con)
      }

      ##### training data
      ###### presence and absence point pattern datasets
      ppp_presence_training <- spatstat.core::ppp(x = training[ , 5][training[ , 4] == 1],
                                         y = training[ , 6][training[ , 4] == 1],
                                         window = wind,
                                         checkdup = FALSE)
      ppp_absence_training <- spatstat.core::ppp(x = training[ , 5][training[ , 4] == 0],
                                            y = training[ , 6][training[ , 4] == 0],
                                            window = wind,
                                            checkdup = FALSE)

      ##### Calculate observed kernel density ratio
      rand_lrr <- sparr::risk(f = ppp_presence_training, g = ppp_absence_training,
                              tolerate = TRUE, verbose = FALSE, ...)

      ##### Create index coordinates
      rx <- rep(rand_lrr$rr$xcol, length(rand_lrr$rr$yrow))
      for(i in 1:length(rand_lrr$rr$yrow)) {
        if (i == 1) { ry <- rep(rand_lrr$rr$yrow[i], length(rand_lrr$rr$xcol)) }
        if (i != 1) { ry <- c(ry, rep(rand_lrr$rr$yrow[i], length(rand_lrr$rr$xcol))) }
        }

      ##### Convert to semi-continuous raster
      rr <-  data.frame("x" = rx,
                        "y" = ry,
                        "v" = as.vector(t(rand_lrr$rr$v)))
      rr$v <- ifelse(is.infinite(rr$v), NA, rr$v)
      sp::coordinates(rr) <- ~ x + y # coordinates
      sp::gridded(rr) <- TRUE # gridded
      rr_raster <- raster::raster(rr)
      rr_raster[is.na(rr_raster[])] <- 0 # if NA, assigned null value (log(rr) = 0)
      ##### Convert to categorical raster
      pval <-  data.frame("x" = rx,
                          "y" = ry,
                          "v" = as.vector(t(rand_lrr$P$v)))
      pval$v <- ifelse(is.infinite(pval$v), NA, pval$v)
      sp::coordinates(pval) <- ~ x + y # coordinates
      sp::gridded(pval) <- TRUE # gridded
      pval_raster <- raster::raster(pval)
      pval_raster[is.na(pval_raster[])] <- 0.5 # if NA, assigned null value (p = 0.5)

      ##### Predict testing dataset
      extract_testing <- testing[ , 5:6]

      ##### Output for each n-fold
      ###### Record category (semi-continuous) of testing data
      cv_predictions_rr <- raster::extract(rr_raster, extract_testing)
      ###### Record category (categorical) of testing data
      cv_predictions_pval <- raster::extract(pval_raster, extract_testing)
      cv_labels <- testing[ , 4] # Record labels (marks) of testing data

      par_results <- list("cv_predictions_rr" = cv_predictions_rr,
                          "cv_predictions_pval" = cv_predictions_pval,
                          "cv_labels"= cv_labels)
      return(par_results)
    }

    # Stop clusters, if parallel
    if (parallel == TRUE) {
      parallel::stopCluster(cl)
    }

    if (verbose == TRUE) { message("\nCalculating Cross-Validation Statistics") }
    cv_predictions_rr <- out_par[[1]]
    cv_predictions_pval <- out_par[[2]]
    cv_labels <- out_par[[3]]

    cv_results <- list("cv_predictions_rr" = cv_predictions_rr,
                       "cv_predictions_pval" = cv_predictions_pval,
                       "cv_labels" = cv_labels)
    }

  # Output
  lrren_output <- list("out" = output,
                       "cv" = cv_results,
                       "dat" = obs_locs)
}
