
fit_sinusoid <- function(y, design, Xinv) {
  ok <- is.finite(y)
  if (sum(ok) < 3) {
    return(rep(NA_real_, 3))
  }
  # All 12 months available
  if (all(ok)) {
    return(as.vector(Xinv %*% y))
  }
  # Fit using available months
  coef <- qr.solve(
    design[ok, , drop = FALSE],
    y[ok]
  )
  return(as.vector(coef))
}