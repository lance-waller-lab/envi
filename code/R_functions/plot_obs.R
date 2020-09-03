plot_obs <- function(input,
                     plot_cols = c("#0000cd", "#cccccc", "#8b3a3a"),
                     alpha = 0.05,
                     ...) {

  op <- par(no.readonly = TRUE)
  par(pty = "s")
  names_obs <- names(input$dat)
  case <- spatstat::setmarks(input$out$case, "presence")
  control <-  spatstat::setmarks(input$out$control, "absence")
  dat <- spatstat::superimpose(control,case, check = FALSE)

  # Plot 1: Locations
  p1 <- spatstat.core::plot.ppp(dat,
                                pch = 1,
                                cex = 0.8,
                                cols = c(plot_cols[1], plot_cols[3]),
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
                  cols = plot_cols[c(3,2,1)],
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
                     add = T,
                     breaks = rrp$breaks,
                     col = rrp$cols,
                     cex.lab = 1,
                     cex = 1,
                     axis.args = list(at = rrp$at,
                                      labels = rrp$labels,
                                      cex.axis = 0.67))

  # Plot 3: Significant p-values
  pvalp <- pval_plot(input$out$obs$P, alpha = alpha)
  if(all(raster::values(pvalp)[!is.na(raster::values(pvalp))] == 2)){
    pcols <- plot_cols[2]
    brp <- c(1, 3)
    atp <- 2
    labp <- "Insignificant"
  } else {
    pcols <- plot_cols[c(3,2,1)]
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
                     add = T,
                     breaks = brp,
                     col = pcols,
                     cex = 1,
                     axis.args = list(at = atp,
                                      las = 0,
                                      labels = labp,
                                      cex.axis = 0.67)
                     )
  suppressMessages(suppressWarnings(par(op)))
}
