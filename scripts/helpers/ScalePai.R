scale_pai <- function(pai_temp, input_month, predicted) {
  
  if (!input_month %in% 1:12) {
    stop("input_month must be an integer from 1 to 12.")
  }
  
  expected <- predicted[[input_month]]
  
  # Relative seasonal change compared with the LiDAR acquisition month
  relative_change <- predicted / expected
  
  # Avoid division by zero / invalid MODIS predictions
  relative_change <- terra::ifel(
    is.finite(expected) & expected > 0,
    relative_change,
    NA
  )
  
  # Apply seasonal variation to LiDAR-derived PAI
  scaled_stack <- pai_temp * relative_change
  
  names(scaled_stack) <- month.name
  
  return(scaled_stack)
}
