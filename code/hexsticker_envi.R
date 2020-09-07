# ----------------------------------------------------- #
# HexSticker for the {envi} package
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: September 5, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) Uses the "hexSticker" package
# B) https://stackoverflow.com/questions/8082429/plot-a-heart-in-r
# ----------------------------------------------------- #

# Packages
loadedPackages <- c("hexSticker", "sp", "spatstat")
suppressMessages(invisible(lapply(loadedPackages, require, character.only = TRUE)))

dat <- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 15*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y <- yhrt(dat$t)
dat$x <- xhrt(dat$t)
with(dat, plot(x,y, type="l"))

dat <- sp::Polygon(dat[,c(3,2)])
dat <- Polygons(list(dat),1)
dat <- SpatialPolygons(list(dat))
plot(dat, col = "forestgreen")

heart <- as(dat, "owin")

nclust <- function(x0, y0, radius, n) {
  return(runifdisc(n, radius, centre = c(x0, y0)))
}

set.seed(1234); test <- spatstat.core::rNeymanScott(kappa = 0.025, expand = 0.2, rcluster = nclust, radius = 3, n = 20,  win = heart); spatstat::marks(test) <- as.factor(rbinom(test$n, 1, 0.5)); plot.ppp(test, pch = 1, fill = "black", cols = c("forestgreen", "black"))


set.seed(1234); test1 <- spatstat.core::rNeymanScott(kappa = 0.025, expand = 0.2, rcluster = nclust, radius = 3, n = 20,  win = heart); spatstat::marks(test1) <- rep(0, test1$n); test2 <- spatstat.core::rNeymanScott(kappa = 0.025, expand = 0.2, rcluster = nclust, radius = 3, n = 20,  win = heart); spatstat::marks(test2) <- rep(1, test2$n); test <- spatstat::superimpose(test2,test1); spatstat::marks(test) <- as.factor(spatstat::marks(test)); plot.ppp(test, pch = 1, cols = c("#72b688", "#b6df7b"), main = "", legend = FALSE, border = "#489c63")




help(points)

# Subplot
p <- plot.ppp(test, pch = 1, cols = c("#72b688", "#b6df7b"), main = "", legend = FALSE, border = "#489c63", cex = 0.1)

# Create hexSticker
## the Balance Within palette https://www.color-hex.com/color-palette/14625
s <- hexSticker::sticker(subplot = ~ plot.ppp(test, pch = "♥", cols = c("#000000", "#b6df7b"), main = "", legend = FALSE, border = "#FFFFFF", cex = 0.1),
                         package = "envi", p_size = 8, p_color = "#c8ec96",
                         s_x = 0.75, s_y = 0.45, s_width = 2, s_height = 2,
                         h_fill = "#6ea181",
                         h_color = "#489c63",
                         dpi = 1000,
                         # url = "envi",
                         # u_x = 0.17,
                         # u_y = 1.37,
                         # u_color = "#c8ec96",
                         # u_size = 6,
                         white_around_sticker = F)


# Option 2: Using output from lrren (NOT DONE YET)
input <- data.frame("id" = seq(1,test$n,1),
                    "lon" = test$x,
                    "lat" = test$y,
                    "group" = spatstat::marks(test),
                    "X" = test$x,
                    "Y" = test$y
)


out <- lrren(input, predict = F, cv = F, conserve = F,
             obs_window = heart)


out_plots <- plot_obs(out, plot_cols = c("#000000", "#6ea181", "#b6df7b"), axes = FALSE, ann = FALSE)

fplot(out_plots[3])

c("#0000cd", "#cccccc", "#8b3a3a")

points<-structure(list(x = test$x, y = test$y), .Names = c("x",
                                                           "y"), row.names = c(NA, test$n), class = "data.frame")


p <- ggplot2::ggplot(data = points) +
  ggplot2::geom_point(x = points$x, y = points$y)
# -------------------- END OF CODE -------------------- #
