reflectance_calc <- function(alb, lai, x, plotprogress = TRUE, maxiter = 50, tol = 0.001, bwgt = 0.5){
  e1 <- intersect(ext(lai), ext(alb))
  e <- intersect(e1, ext(x))
  lai <- crop(lai, e)
  alb <- crop(alb, e)
  x <- crop(x, e)
  # check dims
  all_same <- compareGeom(lai, alb, x)
  if (all_same) {
    lref <- x * 0 + 0.5
    gref <- x * 0 + 0.15
    mxdif <- tol * 10
    paim <- as.matrix(lai, wide = TRUE)
    xm <- as.matrix(x, wide = TRUE)
    albm <- as.matrix(alb, wide = TRUE)
    itr <- 1
    while (mxdif > tol) {
      gref2 <- .rast(find_gref(as.matrix(lref, wide = TRUE), paim, xm, albm), x)
      gref2 <- .fillna(gref2, x, zerotoNA = FALSE)
      lref2 <- .rast(find_lref(paim, as.matrix(gref, wide = TRUE), xm, albm), x)
      lref2 <- .fillna(lref2, x, zerotoNA=FALSE)
      gref <- bwgt * gref + (1 - bwgt) * gref2
      lref <- bwgt * lref + (1 - bwgt) * lref2
      mxdif1 <- mean(abs(as.vector(gref) - as.vector(gref2)), na.rm = TRUE)
      mxdif2 <- mean(abs(as.vector(lref) - as.vector(lref2)), na.rm = TRUE)
      mxdif <- max(mxdif1, mxdif2)
      if (plotprogress & itr%%3 == 0) {
        tp1 <- paste0("Ground difference from previous: ", round(mxdif1, 4))
        tp2 <- paste0("Leaf difference from previous: ", round(mxdif2, 4))
        par(mfrow = c(1, 2))
        plot(gref, main = tp1, cex.main = 1)
        plot(lref, main = tp2, cex.main = 1)
      }
      itr <- itr+1
      if (itr > maxiter) mxdif <- 0
    }
  } else (stop("Geometries of input rasters do not match"))
  return(list(gref = gref, lref = lref))
}
