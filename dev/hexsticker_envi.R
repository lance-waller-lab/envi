# ----------------------------------------------------- #
# HexSticker for the {envi} package
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: September 5, 2020
#
# Recently modified by: @idblr
# Recently modified on: February 4, 2022
#
# Notes:
# A) Uses the "hexSticker" package
# B) https://stackoverflow.com/questions/8082429/plot-a-heart-in-r
# ----------------------------------------------------- #

# Packages
loadedPackages <- c("hexSticker", "sp", "spatstat.geom", "spatstat.random")
suppressMessages(invisible(lapply(loadedPackages, library, character.only = TRUE)))

devtools::load_all()

dat <- data.frame(t = seq(0, 2*pi, by = 0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 15*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y <- yhrt(dat$t)
dat$x <- xhrt(dat$t)
with(dat, plot(x,y, type = "l"))

dat <- sp::Polygon(dat[,c(3,2)])
dat <- Polygons(list(dat),1)
dat <- SpatialPolygons(list(dat))

heart <- as(dat, "owin")

nclust <- function(x0, y0, radius, n) {
  return(runifdisc(n, radius, centre = c(x0, y0)))
}

set.seed(1234)
g1 <- spatstat.random::rNeymanScott(kappa = 0.025, expand = 0.2, rcluster = nclust, radius = 3, n = 50,  win = heart)
spatstat.geom::marks(g1) <- as.factor(rbinom(g1$n, 1, 0.5))
plot.ppp(g1, pch = 1, fill = "black", cols = c("forestgreen", "black"))

# Subplot
p <- spatstat.geom::plot.ppp(g1, pch = 1, cols = c("#72b688", "#b6df7b"), main = "", legend = FALSE, border = "#489c63", cex = 0.1)

# Create hexSticker
## the Envy palette https://www.color-hex.com/color-palette/20984
s <- hexSticker::sticker(subplot = ~ spatstat.geom::plot.ppp(g1,
                                                             pch = "♥",
                                                             cols = c("#FFFFFF", "#000000"),
                                                             main = "",
                                                             legend = FALSE,
                                                             border = "#c8ec96",
                                                             cex = 0.1),
                         package = "envi", p_size = 8, p_color = "#c8ec96",
                         s_x = 0.95, s_y = 0.67, s_width = 1.6, s_height = 1.6,
                         h_fill = "#6ea181",
                         h_color = "#489c63",
                         dpi = 1000,
                         filename = "./man/figures/envi.png",
                         white_around_sticker = F)

# -------------------- END OF CODE -------------------- #
