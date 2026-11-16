albedo_fromaerial <- function(RGBimage, CIRimage, RGBbandmins = c(620, 495, 450),
                              RGBbandmaxs = c(750, 570, 495),
                              CIRbandmins = c(750, 620, 495),
                              CIRbandmaxs = c(900, 750, 570)) {
  # Check resolutions and resample to coarser resolutions if not matching
  res1 <- res(RGBimage)[1]
  res2 <- res(CIRimage)[1]
  if (res1 > res2) CIRimage <- resample(CIRimage, RGBimage)
  if (res2 > res1) RGBimage <- resample(RGBimage, CIRimage)
  # Check extents and intersect
  e1 <- ext(RGBimage)
  e2 <- ext(CIRimage)
  e <- ext(max(e1$xmin, e2$xmin), min(e1$xmax, e2$xmax),
           max(e1$ymin, e2$ymin), min(e1$ymax, e2$ymax))
  RGBimage <- crop(RGBimage, e)
  CIRimage <- crop(CIRimage, e)
  
  RGBimage <- RGBimage / 10000
  CIRimage <- CIRimage / 10000
  # Create weights
  wgt1 <- sum(.Planck(seq(RGBbandmins[1], RGBbandmaxs[1], by = 1))/(10^13))
  wgt2 <- sum(.Planck(seq(RGBbandmins[2], RGBbandmaxs[2], by = 1))/(10^13))
  wgt3 <- sum(.Planck(seq(RGBbandmins[3], RGBbandmaxs[3], by = 1))/(10^13))
  wgt4 <- sum(.Planck(seq(CIRbandmins[1], CIRbandmaxs[1], by = 1))/(10^13))
  wgt5 <- sum(.Planck(seq(CIRbandmins[2], CIRbandmaxs[2], by = 1))/(10^13))
  wgt6 <- sum(.Planck(seq(CIRbandmins[3], CIRbandmaxs[3], by = 1))/(10^13))
  # calculate albedo
  albedo <- (RGBimage[[1]] * wgt1 + RGBimage[[2]] * wgt2 + RGBimage[[3]] * wgt3 +
               CIRimage[[1]] * wgt4 + CIRimage[[2]] * wgt5 + CIRimage[[3]] * wgt6)  /
    (wgt1 + wgt2 + wgt3 + wgt4 + wgt5 + wgt6)
  # albedo <- albedo / 255 REMOVED TO REFLECT SENTINEL-2 DATA INPUT
  albedo <- mask(albedo, RGBimage[[1]])
  return(albedo)
}
