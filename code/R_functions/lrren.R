lrren <- function(obs_locs,
                  predict_locs,
                  predict = TRUE,
                  conserve = TRUE,
                  cv = FALSE,
                  nfold = 10,
                  balance = FALSE,
                  parallel = FALSE,
                  n_core = NULL,
                  poly_buffer = NULL,
                  verbose = FALSE,
                  ...) {

  if (verbose == TRUE) { message("Estimating relative risk surfaces\n") }

  # Compute spatial windows
  ## Calculate inner boundary polygon (extent of case and control locations in environmental space)
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
      outer_chull <- grDevices::chull(x = predict_locs[ , 3], y = predict_locs[ , 4])
      outer_chull_pts <- predict_locs[c(outer_chull, outer_chull[1]), 3:4]
    } else { # concave hull
      outer_chull <- concaveman::concaveman(as.matrix(predict_locs[ , 3:4]))
      outer_chull_pts <- sp::coordinates(outer_chull)
    }
    outer_chull_pts <- rbind(outer_chull_pts, outer_chull_pts[1, ])
    outer_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(outer_chull_pts)), 1)))
    #add small buffer around polygon to include boundary points
    outer_chull_poly_buffer <- rgeos::gBuffer(outer_chull_poly, width = poly_buffer, byid = TRUE)
    outer_poly <- outer_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords #extract coordinates of new polygon
  }

  if (conserve == TRUE) { window_poly <- inner_poly } else { window_poly <- outer_poly }

  # Input Preparation
  ## case and control point pattern datasets
  case_locs <- subset(obs_locs, obs_locs$mark == 1)
  control_locs <- subset(obs_locs, obs_locs$mark == 0)

  ppp_case <- spatstat::ppp(x = case_locs[ , 5],
                            y = case_locs[ , 6],
                            window = spatstat::owin(poly = list(x = rev(window_poly[ , 1]),
                                                                y = rev(window_poly[ , 2]))),
                            checkdup = FALSE)
  ppp_control <- spatstat::ppp(x = control_locs[ , 5],
                               y = control_locs[ , 6],
                               window = spatstat::owin(poly = list(x = rev(window_poly[ , 1]),
                                                                   y = rev(window_poly[ , 2]))),
                               checkdup = FALSE)

  # Calculate observed kernel density ratio
  obs <- sparr::risk(f = ppp_case, g = ppp_control,
                         tolerate = TRUE, verbose = verbose, ...)
  bandw <- obs$f$h0

  if (predict == FALSE) {
    output <- list("obs" = obs,
                   "case" = ppp_case,
                   "control" = ppp_control,
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
    rr <-  dplyr::data_frame(x = rx,
                             y = ry,
                             v = as.vector(t(obs$rr$v)))
    rr$v <- ifelse(is.infinite(rr$v), NA, rr$v)
    rr <- na.omit(rr) # remove NAs
    sp::coordinates(rr) <- ~ x + y # coordinates
    sp::gridded(rr) <- TRUE # gridded
    rr_raster <- raster::raster(rr)

    # Convert to categorical raster
    pval <-  dplyr::data_frame(x = rx,
                               y = ry,
                               v = as.vector(t(obs$P$v)))
    pval$v <- ifelse(is.infinite(pval$v), NA, pval$v)
    pval <- na.omit(pval) # remove NAs
    sp::coordinates(pval) <- ~ x + y # coordinates
    sp::gridded(pval) <- TRUE # gridded
    pval_raster <- raster::raster(pval)

    # Prediction locations
    extract_points <- cbind(predict_locs[ , 3], predict_locs[ , 4])
    extract_predict <-  dplyr::data_frame(predict_locs = predict_locs,
                                          rr = raster::extract(rr_raster, extract_points),
                                          pval = raster::extract(pval_raster, extract_points))


    output <- list("obs" = obs,
                   "case" = ppp_case,
                   "control" = ppp_control,
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
      cv_seg_cas <-  pls::cvsegments(nrow(case_locs), nfold)
      cv_seg_con <-  pls::cvsegments(nrow(control_locs), nfold)
      cv_segments <- NULL
    }

    ### Progress bar
    if (verbose == TRUE & parallel == FALSE) {
      message("Cross-validation in progress")
      pb <- txtProgressBar(min = 0, max = nfold, style = 3)
    }

    ### Set function used in foreach
    if (parallel == TRUE) {
      loadedPackages <- c("doParallel", "parallel")
      invisible(lapply(loadedPackages, require, character.only = TRUE))
      if (is.null(n_core)) { n_core <- parallel::detectCores() - 1 }
      cl <- parallel::makeCluster(n_core)
      doParallel::registerDoParallel(cl)
      `%fun%` <- `%dopar%`
    } else { `%fun%` <- `%do%` }

    ### Foreach loop
    out_par <- foreach::foreach(k = 1:nfold,
                                .combine = comb,
                                .multicombine = TRUE,
                                .packages = c("sparr", "spatstat", "dplyr", "raster"),
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
        testing_cas <- case_locs[cv_seg_cas[k]$V, ]
        testing_con <- control_locs[cv_seg_con[k]$V, ]
        testing_con <- testing_con[randind, ] # undersample the controls for testing
        testing <- rbind(testing_cas,testing_con)
        training_cas <- case_locs[-(cv_seg_cas[k]$V), ]
        training_con <- control_locs[-(cv_seg_con[k]$V), ]
        training <- rbind(training_cas,training_con)
      }

      ##### training data
      ###### case and control point pattern datasets
      ppp_case_training <- spatstat::ppp(x = training[ , 5][training[ , 4] == 1],
                                         y = training[ , 6][training[ , 4] == 1],
                                         window = spatstat::owin(poly = list(x = rev(window_poly[ , 1]),
                                                                             y = rev(window_poly[ , 2]))), checkdup = FALSE)
      ppp_control_training <- spatstat::ppp(x = training[ , 5][training[ , 4] == 0],
                                            y = training[ , 6][training[ , 4] == 0],
                                            window = spatstat::owin(poly = list(x = rev(window_poly[ , 1]),
                                                                                y = rev(window_poly[ , 2]))), checkdup = FALSE)

      ##### Calculate observed kernel density ratio
      rand_lrr <- sparr::risk(f = ppp_case_training, g = ppp_control_training,
                              tolerate = TRUE, verbose = FALSE, ...)

      ##### Create index coordinates
      rx <- rep(rand_lrr$rr$xcol, length(rand_lrr$rr$yrow))
      for(i in 1:length(rand_lrr$rr$yrow)) {
        if (i == 1) { ry <- rep(rand_lrr$rr$yrow[i], length(rand_lrr$rr$xcol)) }
        if (i != 1) { ry <- c(ry, rep(rand_lrr$rr$yrow[i], length(rand_lrr$rr$xcol))) }
        }

      ##### Convert to semi-continuous raster
      rr <-  dplyr::data_frame(x = rx,
                               y = ry,
                               v = as.vector(t(rand_lrr$rr$v)))
      rr$v <- ifelse(is.infinite(rr$v), NA, rr$v)
      sp::coordinates(rr) <- ~ x + y # coordinates
      sp::gridded(rr) <- TRUE # gridded
      rr_raster <- raster::raster(rr)
      rr_raster[is.na(rr_raster[])] <- 0 # if NA, assigned null value (log(rr) = 0)
      ##### Convert to categorical raster
      pval <-  dplyr::data_frame(x = rx,
                                 y = ry,
                                 v = as.vector(t(rand_lrr$P$v)))
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
