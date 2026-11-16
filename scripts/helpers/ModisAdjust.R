.MODISAdjust <- function(MODISpath, PAI, input_month){
  
  files <- list.files(
    MODISpath,
    pattern = "\\.tif$",
    full.names = TRUE
  )
  r.list <- lapply(files, terra::rast)
  r.stk <- terra::rast(r.list)
  
  #' Scale Raw MODIS LAI data.
  r.stk <- scale_modis_lai(r.stk)
  nyr <- length(r.list)
  #' **Convert LAI to PAI**
  pai_m <- model.pai(r.stk, nyr)
  
  #' **Derive intra-annual variation**
  coef.r <- terra::rast(
    pai_m$coef_array,
    ext = terra::ext(r.stk),
    crs = terra::crs(r.stk)
  )
  
  names(coef.r) <- c("a0", "a1", "b1")
  
  coef.rf <- terra::project(
    coef.r,
    PAI[[1]],
    method = "bilinear"
  )
  
  a0_fine <- coef.rf$a0
  a1_fine <- coef.rf$a1
  b1_fine <- coef.rf$b1
  omega <- 2 * pi / 12
  
  monthly <- vector("list", 12)
  for (m in 1:12) {
    monthly[[m]] <- a0_fine +
      a1_fine * sin(omega * m) +
      b1_fine * cos(omega * m)
  }
  
  fine_predicted_stack <- terra::rast(monthly)
  names(fine_predicted_stack) <- month.name
  
  fine_predicted_stack <- terra::clamp(
    fine_predicted_stack,
    lower = 0,
    values = TRUE
  )
  
  #' **Model monthly PAI in LiDAR**
  pai_list <- vector(
    "list",
    nlyr(PAI)
  )
  
  for (i in seq_len(nlyr(PAI))) {
    pai_temp <- PAI[[i]]
    pai_list[[i]] <- scale_pai(
      pai_temp = pai_temp,
      input_month = input_month,
      predicted = fine_predicted_stack
    )
  }
  names(pai_list) <- names(PAI)
  
  return(pai_list)
}
