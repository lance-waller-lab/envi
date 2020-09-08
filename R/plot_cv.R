#' Plots for the prediciton diagnostics of the ecological niche
#' 
#' Create multiple plots of output from the \code{\link{lrren}} function, specifically for the internal n-fold cross-validation diagnostics.
#' 
#' @param input An object of class "list" from the \code{\link{lrren}} function.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default is 0.05).
#' 
#' @return This function produces two plots: 1) area under the receiver operating characteristic curve, and 2) precision-recall curve. Each plot shows predictions for the log relative risk surface and asymptotic tolerance (p-value) surface. The red-colored lines are the average curves. 
#' 
#' @importFrom cvAUC ci.cvAUC cvAUC
#' @importFrom fields image.plot
#' @importFrom graphics abline legend layout mtext par plot.new title
#' @importFrom ROCR performance prediction
#' @export
#'
#' @examples
#' \donttest{
#' plot_cv(input = test_lrren)
#' }
#' 
plot_cv <- function(input, alpha = 0.05) {

  op <- graphics::par(no.readonly = TRUE)
  nfold <- length(input$cv$cv_predictions_rr)
  nsamp <- input$out$presence$n

  out_cv_rr <- cvAUC::cvAUC(input$cv$cv_predictions_rr, input$cv$cv_labels)
  out_cv_pval <- cvAUC::cvAUC(input$cv$cv_predictions_pval, input$cv$cv_labels)
  out_ci_rr <- cvAUC::ci.cvAUC(input$cv$cv_predictions_rr, input$cv$cv_labels,
                               confidence = 1 - alpha)
  out_ci_pval <- cvAUC::ci.cvAUC(input$cv$cv_predictions_pval, input$cv$cv_labels,
                                 confidence = 1 - alpha)

  graphics::layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), heights = c(4, 1))
  graphics::par(oma = c(0, 1, 0, 0), mar = c(0.1, 4.1, 4.1, 2.1), pty = "s")
  plot(out_cv_rr$perf, col = "black", lty = 3,
       xlab = "False Positive Rate (FPR)\n",
       ylab = "\nTrue Positive Rate (TPR)") #Plot fold AUCs
  graphics::abline(0, 1, col = "black", lty = 2)
  plot(out_cv_rr$perf, col = "red", avg = "vertical", add = TRUE, lwd = 2) #Plot CV AUC
  graphics::title(paste("log relative risk prediction\nAUC = ",
                        round(out_cv_rr$cvAUC, digits = 3), " (95% CI: ",
                        round(out_ci_rr$ci[1], digits = 3), " - ",
                        round(out_ci_rr$ci[2], digits = 3), ")", sep = ""),
                  cex.main = 1.1)

  plot(out_cv_pval$perf, col = "black", lty = 3,
       xlab = "False Positive Rate (FPR)\n",
       ylab = "\nTrue Positive Rate (TPR)") #Plot fold AUCs
  graphics::abline(0, 1, col = "black", lty = 2)
  plot(out_cv_pval$perf, col = "red", avg = "vertical", add = TRUE, lwd = 2) #Plot CV AUC
  graphics::title(paste("Asymptotic p-value prediction\nAUC = ",
                        round(out_cv_pval$cvAUC,digits = 3),
              " (95% CI: ", round(out_ci_pval$ci[1], digits = 3)," - ",
              round(out_ci_pval$ci[2], digits = 3), ")", sep = ""), cex.main = 1.1)

  graphics::par(mai = c(0, 0, 0, 0), mar = c(5.1, 4.1, 0.1, 2.1) / 5, pty = "m")
  graphics::plot.new()
  graphics::legend(x = "top", inset = 0, title = "Legend",
                   legend = c("Individual n-fold",
                              "Average",
                              "Luck (Reference)"),
                   lty = c(3, 1, 2), bty = "n",
                   col = c("black", "red", "black"))
  graphics::mtext(paste("Area Under the Receiver Operating Characteristic Curve\n", nfold,
                        "-fold cross-validation, alpha = ", alpha, sep = ""),
                  side = 3, line = -4, outer = TRUE, cex = 1.25)

  ## Precision Recall

  pred_rr <- ROCR::prediction(input$cv$cv_predictions_rr, input$cv$cv_labels)
  perf_rr <- ROCR::performance(pred_rr, "prec", "rec") # PRREC same as "ppv", "tpr"
  pred_pval <- ROCR::prediction(input$cv$cv_predictions_pval, input$cv$cv_labels)
  perf_pval <- ROCR::performance(pred_pval, "prec", "rec") # PRREC same as "ppv", "tpr"

  graphics::layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), heights = c(4, 1))
  graphics::par(oma = c(0, 1, 0, 0), mar = c(0.1, 4.1, 4.1, 2.1), pty = "s")
  plot(perf_rr, ylim = c(0, 1), xlim = c(0, 1), lty = 3,
       xlab = "True Positive Rate (Sensitivity or Recall)\n",
       ylab = "\nPositive Predictive Value (Precision)")
  graphics::abline((nsamp / nfold) / length(input$cv$cv_labels[[1]]), 0, lty = 2, col = "black")
  suppressWarnings(lines(colMeans(do.call(rbind, perf_rr@x.values)),
                         colMeans(do.call(rbind, perf_rr@y.values)),
                         col = "red", lty = 1, lwd = 2)) # mean PRREC
  graphics::title("log relative risk prediction", cex.main = 1.1)

  plot(perf_pval, ylim = c(0, 1), xlim = c(0, 1), lty = 3,
       xlab = "True Positive Rate (Sensitivity or Recall)\n",
       ylab = "\nPositive Predictive Value (Precision)")
  graphics::abline((nsamp / nfold) / length(input$cv$cv_labels[[1]]), 0, lty = 2, col = "black")
  suppressWarnings(lines(colMeans(do.call(rbind,perf_pval@x.values)),
                         colMeans(do.call(rbind,perf_pval@y.values)),
                         col = "red", lty = 1, lwd = 2)) # mean PRREC
  graphics::title("Asymptotic p-value prediction", cex.main = 1.1)

  graphics::par(mai = c(0, 0, 0, 0), mar = c(5.1, 4.1, 0.1, 2.1) / 5, pty = "m")
  graphics::plot.new()
  graphics::legend(x = "top", inset = 0, title = "Legend",
                   legend = c("Individual n-fold",
                              "Average",
                              "Luck (Reference)"),
                   lty = c(3, 1, 2), bty = "n",
                   col = c("black", "red", "black"))
  graphics::mtext(paste("Precision-Recall Curve\n", nfold, "-fold cross-validation, alpha = ",
                        alpha, sep=""),
                  side = 3, line = -4, outer = TRUE, cex = 1.25)

 on.exit(graphics::par(op))
}
