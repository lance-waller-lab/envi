#' Visualizations for the prediction diagnostics of an estimated ecological niche
#' 
#' Create multiple plots of output from the \code{\link{lrren}} function, specifically for the internal k-fold cross-validation diagnostics.
#' 
#' @param input An object of class 'list' from the \code{\link{lrren}} function.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default is 0.05).
#' 
#' @return This function produces two plots: 1) area under the receiver operating characteristic curve, and 2) precision-recall curve. Each plot shows predictions for the log relative risk surface. The red-colored lines are the average curves. 
#' 
#' @importFrom cvAUC ci.cvAUC cvAUC
#' @importFrom fields image.plot
#' @importFrom graphics abline layout legend lines mtext par plot plot.new title
#' @importFrom ROCR performance prediction
#' @export
#'
#' @examples
#' if (interactive()) {
#'   set.seed(1234) # for reproducibility
#'
#' # Using the 'bei' and 'bei.extra' data within {spatstat.data}
#' 
#' # Covariate data (centered and scaled)
#'   elev <- spatstat.data::bei.extra[[1]]
#'   grad <- spatstat.data::bei.extra[[2]]
#'   elev$v <- scale(elev)
#'   grad$v <- scale(grad)
#'   elev_raster <- raster::raster(elev)
#'   grad_raster <- raster::raster(grad)
#' 
#' # Presence data
#'   presence <- spatstat.data::bei
#'   spatstat::marks(presence) <- data.frame("presence" = rep(1, presence$n),
#'                                           "lon" = presence$x,
#'                                           "lat" = presence$y)
#'   spatstat::marks(presence)$elev <- elev[presence]
#'   spatstat::marks(presence)$grad <- grad[presence]
#' 
#' # (Pseudo-)Absence data
#'   absence <- spatstat::rpoispp(0.008, win = elev)
#'   spatstat::marks(absence) <- data.frame("presence" = rep(0, absence$n),
#'                                               "lon" = absence$x,
#'                                               "lat" = absence$y)
#'   spatstat::marks(absence)$elev <- elev[absence]
#'   spatstat::marks(absence)$grad <- grad[absence]
#' 
#' # Combine into readable format
#'   obs_locs <- spatstat::superimpose(presence, absence, check = FALSE)
#'   obs_locs <- spatstat::marks(obs_locs)
#'   obs_locs$id <- seq(1, nrow(obs_locs), 1)
#'   obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)]
#'   
#' # Prediction Data
#'   predict_locs <- data.frame(raster::rasterToPoints(elev_raster))
#'   predict_locs$layer2 <- raster::extract(grad_raster, predict_locs[, 1:2])
#' 
#' # Run lrren
#'   test_lrren <- lrren(obs_locs = obs_locs,
#'                       predict_locs = predict_locs,
#'                       predict = TRUE,
#'                       cv = TRUE)
#'                       
#' # Run plot_cv                 
#'   plot_cv(input = test_lrren)
#' }
#' 
plot_cv <- function(input, alpha = 0.05) {
  
  if (alpha >= 1 | alpha <= 0) {
    stop("The argument 'alpha' must be a numeric value between 0 and 1")
  }

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  kfold <- length(input$cv$cv_predictions_rr)
  nsamp <- input$out$presence$n

  out_cv_rr <- cvAUC::cvAUC(input$cv$cv_predictions_rr, input$cv$cv_labels)
  out_ci_rr <- cvAUC::ci.cvAUC(input$cv$cv_predictions_rr, input$cv$cv_labels,
                               confidence = 1 - alpha)
  pred_rr <- ROCR::prediction(input$cv$cv_predictions_rr, input$cv$cv_labels)
  perf_rr <- ROCR::performance(pred_rr, "prec", "rec") # PRREC same as "ppv", "tpr"
  
  graphics::layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), heights = c(4, 1))
  graphics::par(oma = c(0, 1, 0, 0), mar = c(0.1, 4.1, 4.1, 2.1), pty = "s")
  graphics::plot(out_cv_rr$perf, col = "black", lty = 3,
                 xlab = "False Positive Rate (FPR)\n",
                 ylab = "\nTrue Positive Rate (TPR)") #Plot fold AUCs
  graphics::abline(0, 1, col = "black", lty = 2)
  graphics::plot(out_cv_rr$perf, col = "red", avg = "vertical", add = TRUE, lwd = 2) #Plot CV AUC
  graphics::title(paste("Area Under the ROC Curve\nAUC = ",
                        round(out_cv_rr$cvAUC, digits = 3), " (95% CI: ",
                        round(out_ci_rr$ci[1], digits = 3), " - ",
                        round(out_ci_rr$ci[2], digits = 3), ")", sep = ""),
                  cex.main = 1.1)

  graphics::plot(perf_rr, ylim = c(0, 1), xlim = c(0, 1), lty = 3,
                 xlab = "True Positive Rate (Sensitivity or Recall)\n",
                 ylab = "\nPositive Predictive Value (Precision)")
  graphics::abline((nsamp / kfold) / length(input$cv$cv_labels[[1]]), 0, lty = 2, col = "black")
  suppressWarnings(graphics::lines(colMeans(do.call(rbind, perf_rr@x.values)),
                                   colMeans(do.call(rbind, perf_rr@y.values)),
                                   col = "red", lty = 1, lwd = 2)) # mean PRREC
  graphics::title("Precision-Recall Curve", cex.main = 1.1)

  graphics::par(mai = c(0, 0, 0, 0), mar = c(5.1, 4.1, 0.1, 2.1) / 5, pty = "m")
  graphics::plot.new()
  graphics::legend(x = "top", inset = 0, title = "Legend",
                   legend = c("individual k-fold",
                              "average",
                              "luck (reference)"),
                   lty = c(3, 1, 2), bty = "n",
                   col = c("black", "red", "black"))
  graphics::mtext(paste("Internal ", kfold,
                        "-fold cross-validation, alpha = ", alpha, sep = ""),
                  side = 3, line = -4, outer = TRUE, cex = 1.25)
}
