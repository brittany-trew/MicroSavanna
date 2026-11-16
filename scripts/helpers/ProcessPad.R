processPAD <- function(pad.r, chm_mosaic){
  #' Tiles need to be padded with NAs at higher levels because tiles have different heights.
  #' Maximum number of vertical layers across tiles
  max_nlyr <- max(sapply(pad.r, terra::nlyr))
  
  pad.list <- lapply(pad.r, function(r) {
    n_missing <- max_nlyr - terra::nlyr(r)
    if (n_missing > 0) {
      # Empty layer with exactly the same spatial geometry
      empty <- r[[1]]
      empty[] <- NA
      # Add missing layers to TOP of the vertical stack
      for (j in seq_len(n_missing)) {
        r <- c(r, empty)
      }
    }
    r
  })
  
  #' Mosaic the tiles together.
  pad.r <- do.call(mosaic,pad.list)
  pad.r <- terra::resample(pad.r, chm_mosaic)
  
  start_heights <- seq(
    from = 0,
    by = dzd,
    length.out = nlyr(pad.r)
  )
  
  names(pad.r) <- sprintf(
    "PAD_%.1f_%.1fm",
    start_heights,
    start_heights + dzd
  )
  
  
  #' Identify pixels containing PAD data.
  valid <- sum(!is.na(pad.r)) > 0
  
  #' Treat above-canopy PAD as zero.
  pad0 <- terra::ifel(
    is.na(pad.r),
    0,
    pad.r
  )
  
  #' Retain NA where there is genuinely no PAD data.
  pad0 <- terra::mask(
    pad0,
    valid,
    maskvalues = 0
  )
  
  return(pad0)
}
