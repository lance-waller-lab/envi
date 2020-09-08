#' Plots for the estimated ecological niche in covariate space
#' 
#' Create multiple plots of output from the \code{\link{lrren}} functions, specifically for the observation data and estimated ecological niche. 
#' 
#' @param input An object of class "list" from the \code{\link{lrren}} function.
#' @param plot_cols Character string of length three (3) specifying the colors for plotting: 1) presense, 2) neither, and 3) absence. The default colors in hex are \code{c("#8b3a3a", "#cccccc", "#0000cd")} or \code{c("indianred4", "grey80", "blue3")}.
#' @param alpha Numeric. The two-tailed alpha level for significance threshold (default is 0.05).
#' @param ... Arguments passed to \code{\link[spatstat.core]{plot.ppp}} and \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces three plots in a two-dimensional space where the axes are the two specified covariates: 1) observation locations by group, 2) log relative risk surface, and 3) significant p-value surface. 
#' 
#' @importFrom fields image.plot
#' @importFrom graphics par
#' @importFrom raster values
#' @importFrom spatstat.core plot.ppp setmarks superimpose
#' @export
#'
#' @examples
#' \donttest{
#' plot_obs(input = test_lrren)
#' }
#' 
plot_obs <- function(input,
                     plot_cols = c("#8b3a3a", "#cccccc", "#0000cd"),
                     alpha = 0.05,
                     ...) {

  op <- graphics::par(no.readonly = TRUE)
  graphics::par(pty = "s")
  names_obs <- names(input$dat)
  presence <- spatstat.core::setmarks(input$out$presence, "presence")
  absence <-  spatstat.core::setmarks(input$out$absence, "absence")
  dat <- spatstat.core::superimpose(absence, presence, check = FALSE)

  # Plot 1: Locations
  p1 <- spatstat.core::plot.ppp(dat,
                                pch = 1,
                                cex = 0.8,
                                cols = c(plot_cols[3], plot_cols[1]),
                                leg.side = "bottom",
                                leg.args = list(cex.axis = 0.9, cex = 1, pch = c(1,1)),
                                main = "Locations",
                                main.panel = "",
                                xlab = names_obs[5],
                                ylab = names_obs[6],
                                axes = TRUE,
                                ann = TRUE,
                                ...)

  # Plot 2: log relative risk
  rrp <- lrr_plot(input = input$out$obs$rr,
                  cols = plot_cols,
                  midpoint = 0)

  p2 <- spatstat.core::plot.ppp(dat,
                                cols = c("transparent", "transparent"),
                                leg.side = "bottom",
                                leg.args = list(annotate = FALSE),
                                main = "log relative risk",
                                xlab = names_obs[5],
                                ylab = names_obs[6],
                                axes = TRUE,
                                ann = TRUE,
                                ...)
  fields::image.plot(rrp$v,
                     add = TRUE,
                     breaks = rrp$breaks,
                     col = rrp$cols,
                     legend.mar = 3.1,
                     axis.args = list(at = rrp$at,
                                      las = 0,
                                      labels = rrp$labels,
                                      cex.axis = 0.67))

  # Plot 3: Significant p-values
  pvalp <- pval_plot(input$out$obs$P, alpha = alpha)
  if (all(raster::values(pvalp)[!is.na(raster::values(pvalp))] == 2)) {
    pcols <- plot_cols[2]
    brp <- c(1, 3)
    atp <- 2
    labp <- "Insignificant"
  } else {
    pcols <- plot_cols
    brp <- c(1, 1.67, 2.33, 3)
    atp <- c(1.33, 2, 2.67)
    labp <- c("Presence", "Insignificant", "Absence")
  }

  p3 <- spatstat.core::plot.ppp(dat,
                                cols = c("transparent", "transparent"),
                                leg.side = "bottom",
                                leg.args = list(annotate = FALSE),
                                xlab = names_obs[5],
                                ylab = names_obs[6],
                                axes = TRUE,
                                ann = TRUE,
                                main = paste("Significant p-values\nalpha =", alpha, sep = " "),
                                ...)
  fields::image.plot(pvalp,
                     add = TRUE,
                     breaks = brp,
                     col = pcols,
                     legend.mar = 3.1,
                     axis.args = list(at = atp,
                                      las = 0,
                                      labels = labp,
                                      cex.axis = 0.67))
  on.exit(graphics::par(op))
}
