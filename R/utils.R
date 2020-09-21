# The combine function used in foreach
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

# Convert an object of class 'imlist' to an object of class 'data.frame'
ims2df <- function(ims) {
  rx <- rep(ims[[1]]$xcol, length(ims[[1]]$yrow))
  for(i in 1:length(ims[[1]]$yrow)) {
    if (i == 1) { ry <- rep(ims[[1]]$yrow[i], length(ims[[1]]$xcol)) }
    if (i != 1) { ry <- c(ry, rep(ims[[1]]$yrow[i], length(ims[[1]]$xcol))) }
  }
  out <- data.frame("x" = rx,
                    "y" = ry,
                    "v" = as.vector(t(ims[[1]]$v)))
  out$v <- ifelse(is.infinite(out$v), NA, out$v)
  out$z <- as.vector(t(ims[[2]]$v))
  out$z <- ifelse(is.infinite(out$z), NA, out$z)
  return(out)
}