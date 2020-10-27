#' Spatially perturb an ecological niche model that uses a log relative risk surface
#' 
#' Estimates the ecological niche of a single species with presence/absence data and two covariates, iteratively, by randomly perturbing ('jittering') the coordinates of observations.
#' 
#' @param obs_ppp Input object of class 'ppp' a marked point pattern of presence and absence observations with 5 (five) features (columns): 1) ID, 2) longitude, 3) latitude, 4) presence/absence binary variable, 5) ordinal ID for spatial perturbation.
#' @param covariates Input object of class 'imlist' of 2 (two) covariates within the same spatial window and in the same coordinate reference system as \code{obs_ppp}.
#' @param predict Logical. If TRUE (the default), will predict the ecological niche in geographic space. If FALSE, will not predict.
#' @param predict_locs Input data frame of prediction locations with 4 features (columns): 1) longitude, 2) latitude, 3) covariate 1 as x-coordinate, 4) covariate 2 as y-coordinate. If unspecified (the default), automatically computed from an 'im' object within \code{covariates}.
#' @param radii Vector of length equal to the number of levels of ordinal ID in \code{obs_ppp}. Specifies the radii of the spatial perturbation at each level in units equivalent to the coordinate reference system of \code{obs_ppp}.
#' @param n_sim Integer, specifying the number of simulation iterations to perform.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default is 0.05).
#' @param parallel Logical. If TRUE, will execute the function in parallel. If FALSE (the default), will not execute the function in parallel.
#' @param n_core Optional. Integer specifying the number of CPU cores on current host to use for parallelization (the default is 2 cores).
#' @param verbose Logical. If TRUE (the default), will print function progress during execution. If FALSE, will not print.
#' @param ... Arguments passed to \code{\link{lrren}}.
#' 
#' @details This function performs a sensitivity analysis of an ecological niche model of a single species (presence/absence data), or the presence of one species relative to another, that uses two covariates. The observation locations (presence and absence data) are randomly spatially perturbed (i.e., "jittered") uniformly within a circular disc of a specified radius centered at their recorded location using the \code{\link[spatstat]{rjitter}} function. This method simulates the spatial uncertainty of observations, how that may affect the covariate values at each observation (i.e., misclassification error), and the estimated ecological niche based on the two specified covariates. Observations can be grouped into categories of uncertainty of class 'factor' and can vary by degrees of uncertainty specified using the \code{radii} argument. 
#' 
#' The function iteratively estimates the ecological niche using the \code{\link{lrren}} function and computes four summary statistics at every grid cell (i.e., knot) of the estimated surface: 1) mean of the log relative risk, 2) standard deviation of the log relative risk, 3) mean of the asymptotically normal p-value, and 4) proportion of iterations were statistically significant based on a two-tailed alpha-level threshold (argument \code{alpha}). The process can be performed in parallel if \code{parallel = TRUE} using the \code{\link[foreach]{foreach}} function. The computed surfaces can be visualized using the \code{\link{plot_perturb}} function. If \code{predict = TRUE}, this function will predict the four summary statistics at every location specified with \code{predict_locs} and can also be visualized using the \code{\link{plot_perturb}} function. 
#' 
#' For more information about the spatial perturbation, please refer to the \code{\link[spatstat]{rjitter}} function documentation.
#' 
#' @return An object of class "list". This is a named list with the following components:
#' 
#' \describe{
#' \item{\code{sim}}{An object of class 'list' for the summary statistics of the iterative ecological niche.}
#' \item{\code{predict}}{An object of class 'ppp' a marked point pattern with summary statistics for the iterative ecological niche in geographic space.}
#' }
#' 
#' The returned \code{sim} is a named list with the following components:
#' 
#' \describe{
#' \item{\code{lrr_mean}}{An object of class 'im' for the mean log relative risk surface.}
#' \item{\code{lrr_sd}}{An object of class 'im' for the standard deviation of log relative risk surface.}
#' \item{\code{pval_mean}}{An object of class 'im' for the mean p-value surface.}
#' \item{\code{pval_prop}}{An object of class 'im' for the proportion of iterations were statistically significant surface.}
#' }
#' 
#' If \code{predict = FALSE}, the returned \code{predict} is empty. If \code{predict = TRUE}, the returned \code{predict} is an object of class 'ppp' a marked point pattern with the following features:
#' 
#' \describe{
#' \item{\code{x}}{Values for x-coordinate in geographic space (e.g., longitude).}
#' \item{\code{y}}{Values for y-coordinate in geographic space (e.g., latitude).}
#' \item{\code{v}}{Values for x-coordinate in covariate space.}
#' \item{\code{z}}{Values for x-coordinate in covariate space.}
#' \item{\code{lrr_mean}}{Values for the mean log relative risk surface.}
#' \item{\code{lrr_sd}}{Values for the standard deviation of log relative risk surface.}
#' \item{\code{pval_mean}}{Values for the mean p-value surface.}
#' \item{\code{pval_prop}}{Values for the proportion of iterations were statistically significant surface.}
#' }
#' 
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom raster crs
#' @importFrom spatstat as.solist im.apply marks owin ppp rjitter superimpose
#' @importFrom stats sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' 
#' @examples
#' if (interactive()) {
#'   set.seed(1234) # for reproducibility
#' 
#' # Using the 'bei' and 'bei.extra' data within {spatstat.data}
#' 
#' # Covariate data (centered and scaled)
#'   ims <- spatstat.data::bei.extra
#'   ims[[1]]$v <- scale(ims[[1]]$v)
#'   ims[[2]]$v <- scale(ims[[2]]$v)
#'   
#' # Presence data
#'   presence <- spatstat.data::bei
#'   spatstat::marks(presence) <- data.frame("presence" = rep(1, presence$n),
#'                                               "lon" = presence$x,
#'                                               "lat" = presence$y)
#'                                           
#' # (Pseudo-)Absence data
#'   absence <- spatstat::rpoispp(0.008, win = ims[[1]])
#'   spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
#'                                               "lon" = absence$x,
#'                                               "lat" = absence$y)
#' # Combine into readable format
#'   obs_locs <- spatstat::superimpose(presence, absence, check = FALSE)
#'   spatstat::marks(obs_locs)$id <- seq(1, obs_locs$n, 1)
#'   spatstat::marks(obs_locs) <- spatstat::marks(obs_locs)[ , c(4, 2, 3, 1)]
#'  
#' # Specify categories for varying degrees of spatial uncertainty
#' ## Creates three groups
#'   spatstat::marks(obs_locs)$levels <- as.factor(stats::rpois(obs_locs$n,
#'                                                                   lambda = 0.05))
#'                                                                   
#' # Run perlrren
#'   test_perlrren <- perlrren(obs_ppp = obs_locs,
#'                             covariates = ims,
#'                             radii = c(10, 100, 500),
#'                             n_sim = 10)
#' }
#' 
perlrren <- function(obs_ppp,
                     covariates,
                     predict = TRUE,
                     predict_locs = NULL,
                     radii = NULL,
                     n_sim = 2,
                     alpha = 0.05,
                     parallel = FALSE,
                     n_core = NULL,
                     verbose = FALSE,
                     ...) {
  
  if (!identical(raster::crs(obs_ppp), raster::crs(covariates))) {
    stop("The arguments 'obs_ppp' and 'covariates' must have the same coordinate reference system")
  }
  
  if (is.null(radii)) {
    radii <- rep(0, nlevels(spatstat::marks(obs_ppp)[ , 5]))
    message("The argument 'radii' is unspecified and the observation coordinates are not perturbed")
  }
  
  if (length(radii) != nlevels(spatstat::marks(obs_ppp)[ , 5])) {
    stop("The argument 'radii' must have a length equal to the number of levels in 'obs_ppp'")
  }
  
  if (alpha >= 1 | alpha <= 0) {
    stop("The argument 'alpha' must be a numeric value between 0 and 1")
  }
  
  if (is.null(predict_locs)) { predict_locs <- ims2df(covariates) }
  
  ### Progress bar
  if (verbose == TRUE) {
    message("Randomly perturbing the spatial coordinates and estimating ecological niche")
    if(parallel == FALSE) { 
      pb <- utils::txtProgressBar(min = 0, max = n_sim, style = 3) 
    }
  }
  
  ### Set function used in foreach
  if (parallel == TRUE) {
    if (is.null(n_core)) { n_core <- parallel::detectCores() - 1 }
    cl <- parallel::makeCluster(n_core)
    doParallel::registerDoParallel(cl)
    `%fun%` <- foreach::`%dopar%`
  } else { `%fun%` <- foreach::`%do%` }
  
  out_par <- foreach::foreach(k = 1:n_sim,
                              .combine = comb,
                              .multicombine = TRUE,
                              .packages = c("envi", "spatstat", "utils"),
                              .init = list(list(), list(), list())
  ) %fun% {
    
    if (verbose == TRUE & parallel == FALSE) {
      utils::setTxtProgressBar(pb, k)
      if (k == n_sim) cat("\n")
      }
    
    x <- spatstat::split.ppp(obs_ppp, f = "levels")
    z <- vector("list", length(x))
    
    # Spatially perturb points based on categories
    for (i in 1:length(x)) {
      z[[i]] <- spatstat::rjitter(x[[i]], radius = radii[i])
      names(z) <- names(x)
      z <- spatstat::as.solist(z, demote = TRUE)
    }
    xx <- spatstat::superimpose(z) # re-combine
    
    # Extract Covariate Values
    for (i in 1:length(covariates)) {
      spatstat::marks(xx)[[5 + i]] <- covariates[[i]][xx, drop = FALSE]
      names(spatstat::marks(xx))[5 + i] <- names(covariates[i])
    }
    
    xxx <- spatstat::marks(xx)[ , -5]
    
    # remove observations with NA covariates values ()
    ## typically will not be an issue
    ## unless obs_ppp and covariates have dissimilar windows, even slightly dissimilar
    xxx <- na.omit(xxx) 
    
    xxxx <- lrren(obs_locs = xxx, 
                  predict_locs = predict_locs,
                  conserve = FALSE,
                  ...)
    
    if (k == 1) {
      outer_poly <- xxxx$out$outer_poly
    } else { outer_poly <- NULL }
    
    # Output for each n-fold
    par_results <- list("sim_risk" = xxxx$out$obs$rr,
                        "sim_pval" = xxxx$out$obs$P,
                        "outer_poly" = outer_poly)
  }
  
  # Stop clusters, if parallel
  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }
  
  # Post-statistics
  ## mean of log relative risk
  lrr_mean <- spatstat::im.apply(out_par[[1]],
                                      mean,
                                      fun.handles.na = TRUE,
                                      na.rm = TRUE)
  ## standard deviation of log relative risk
  lrr_sd <- spatstat::im.apply(out_par[[1]],
                                    stats::sd,
                                    fun.handles.na = TRUE,
                                    na.rm = TRUE)
  ## mean p-value 
  pval_mean <- spatstat::im.apply(out_par[[2]],
                                       mean,
                                       fun.handles.na = TRUE,
                                       na.rm = TRUE)
  ## proportion significant
  pval_sig <- lapply(out_par[[2]],
                     function(x) ifelse(x$v < alpha / 2 | x$v > (1 - alpha / 2),
                                        TRUE,
                                        FALSE))
  pval_count <- Reduce(`+`, pval_sig) / n_sim
  pval_prop <- pval_mean
  pval_prop$v <- pval_count
  
  out_sim <- list("lrr_mean" = lrr_mean,
                  "lrr_sd" = lrr_sd,
                  "pval_mean" = pval_mean,
                  "pval_prop" = pval_prop)
  
  if (predict == FALSE) {
    output <- list("sim" = out_sim,
                   "predict" = NULL)
    return(output)
  } else {
    # Project relative risk surface into geographic space
    if (verbose == TRUE) { message("Predicting area of interest") }
    window_poly <- out_par[[3]][[1]]
    wind <- spatstat::owin(poly = list(x = rev(window_poly[ , 1]),
                                            y = rev(window_poly[ , 2])))
    
    xxxxx <- spatstat::ppp(x = predict_locs[ , 3],
                                y = predict_locs[ , 4],
                                window = wind,
                                marks = predict_locs,
                                check = FALSE) 
    # points along polygon border will be lost
   
    spatstat::marks(xxxxx)[ , 5] <- lrr_mean[xxxxx]
    spatstat::marks(xxxxx)[ , 6] <- lrr_sd[xxxxx]
    spatstat::marks(xxxxx)[ , 7] <- pval_mean[xxxxx]
    spatstat::marks(xxxxx)[ , 8] <- pval_prop[xxxxx]
    names(spatstat::marks(xxxxx))[5:8] <- c("lrr_mean",
                                                 "lrr_sd",
                                                 "pval_mean",
                                                 "pval_prop")
    out_pred <- spatstat::marks(xxxxx)
    out_ppp <- spatstat::ppp(x = out_pred$x,
                                  y = out_pred$y,
                                  window = spatstat::as.owin(covariates[[1]]),
                                  marks = out_pred)
    
    output <- list("sim" = out_sim,
                   "predict" = out_ppp)
    return(output)
  }
}
