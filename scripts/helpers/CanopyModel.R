#' @title Generate Canopy Height Model (CHM) from Normalized Point Cloud
#' @description
#' Internal helper function that computes a Canopy Height Model (CHM) from a height-normalized LAS object. Users can specify one of three algorithms: `"pitfree"` (default), `"p2r"`, or `"dsmtin"`. Optional post-processing (e.g., smoothing) is applied when appropriate.
#' @param las_norm A height-normalized `LAS` object from the `{lidR}` package.
#' @param res Numeric. Desired resolution of the output CHM raster.
#' @param chmA Character. Algorithm to use for CHM generation. Options are `"pitfree"` (default), `"p2r"`, or `"dsmtin"`. If `NULL`, `"pitfree"` is used.
#' @return A `RasterLayer` or `SpatRaster` object representing the Canopy Height Model.
#' @import lidR
#' @importFrom terra focal
#' @noRd
.CanopyModel <- function(las_norm, res, chmA = NULL){
  if(chmA == "pitfree" || is.null(chmA)){
    chm <- rasterize_canopy(las_norm, res, algorithm = pitfree())
  } else if (chmA == "p2r"){
    chm <- rasterize_canopy(las_norm, res, algorithm = p2r(na.fill = tin()))
    w <- matrix(1, 3, 3)
    chm <- terra::focal(chm, w, fun = mean, na.rm = TRUE)
  } else if (chmA == "dsmtin"){
    chm <- rasterize_canopy(las_norm, res, algorithm = dsmtin())
  } else {
    print("CHM Algorithm not recognised.")
  }
  return(chm)
}