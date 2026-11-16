scale_modis_lai <- function(x) {
  x[x < 0 | x > 100] <- NA
  x <- x * 0.1
  return(x)
}