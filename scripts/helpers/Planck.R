#' Used by albedo calculations - calculates wavelength specific spectral density
.Planck <- function(wavelength, temperature =  5504.85) {
  d <- wavelength * 1e-09
  h <- 6.6256e-34
  cc <- 299792458
  tt <- temperature + 273.15
  k <- 1.38054e-23
  b <- (2 * pi * h * cc^2) / (d^5 * (exp((h * cc) / (k * d * tt)) - 1))
  b
}