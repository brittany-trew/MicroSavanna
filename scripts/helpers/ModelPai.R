model.pai <- function(r.stk, nyr){
  r.array <- as.array(r.stk)
  nmons <- nlyr(r.stk)
  
  nrow <- dim(r.array)[1]
  ncol <- dim(r.array)[2]
  nmon <- 12
  
  if (nmons != nyr * nmon) stop("Missing Months.")
  
  stk.array <- array(
    r.array,
    dim = c(nrow, ncol, nmon, nyr)
  )
  
  maximums <- apply(
    stk.array,
    c(1, 2),
    function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        max(x, na.rm = TRUE)
      }
    }
  )
  
  # calculate 20% of maximum LAI to account for PAI
  pai_multi <- maximums * 0.2
  pai_4D <- array(
    rep(pai_multi, times = 12 * nyr),
    dim = dim(stk.array)
  )
  PAI_array <- stk.array + pai_4D
  
  mean_mon <- apply(
    PAI_array,
    c(1, 2, 3),
    function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        mean(x, na.rm = TRUE)
      }
    }
  )
  
  month <- 1:nmon
  omega <- 2 * pi / nmon
  # Matrix for sinusoidal regression:
  X <- cbind(
    1,
    sin(omega * month),
    cos(omega * month)
  )
  
  Xinv <- solve(t(X) %*% X) %*% t(X)
  
  mean_mon_flat <- matrix(
    mean_mon,
    nrow = nrow * ncol,
    ncol = nmon
  )
  
  coefs <- t(apply(
    mean_mon_flat,
    1,
    fit_sinusoid,
    design = X,
    Xinv = Xinv
  ))
  coef_array <- array(
    coefs,
    dim = c(nrow, ncol, 3)
  )
  return(
    list(
      coef_array = coef_array,
      pai_MODIS = mean_mon
    )
  )
}