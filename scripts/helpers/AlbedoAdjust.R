albedo_adjust<-function(photoalbedo, modisalbedo) {
  # crop modis to match extent of aerial
  if (terra::crs(photoalbedo) != terra::crs(modisalbedo)) modisalbedo<-project(modisalbedo, terra::crs(photoalbedo))
  v1 <- as.vector(crop(modisalbedo, ext(photoalbedo)))
  v1 <-v1[is.na(v1) == FALSE]
  v2 <- as.vector(resample(photoalbedo, modisalbedo))
  v2 <-v2[is.na(v2) == FALSE]
  # logit transform
  lv1 <- log(v1/ (1 - v1))
  lv2 <- log(v2/ (1 - v2))
  # if length of either is 1
  n <- min(length(v1), length(v2))
  if (n == 1) {
    mu <- lv1 - lv2
    lphoto <- log(photoalbedo / (1 - photoalbedo)) + mu
    albedo <- 1 / (1 + exp(-lphoto))
  } else {
    mum <- mean(lv1) - mean(lv2)
    mus <- sd(lv1) / sd(lv2)
    lphoto <- log(photoalbedo / (1 - photoalbedo))
    me <- mean(as.vector(lphoto), na.rm = TRUE)
    lphoto <- ((lphoto - me) * mus) + mum + me
    albedo <- 1 / (1 + exp(-lphoto))
  }
  return(albedo)
}