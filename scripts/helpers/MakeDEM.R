#' @title Generate Digital Elevation Model (DEM) from a Point Cloud.
#' @description
#' This internal helper function creates a Digital Elevation Model (DEM) from a LAS point cloud by extracting ground-classified points and applying a specified interpolation algorithm. The DEM is returned as a raster.
#' @param las A `LAS` or `LAScluster` object from the `{lidR}` package. Must contain ground-classified points (classification code 2).
#' @param res Numeric. Desired output raster resolution (in the same units as the LAS projection).
#' @param dem.algorithm Character. Interpolation method to use for rasterizing terrain. Options are `"tin"` (default), `"knnidw"`, or `"kriging"`.
#' @return A `RasterLayer` or `SpatRaster` (depending on your environment) representing the interpolated DEM.
#' @import lidR
#' @noRd
.makeDEM <- function(las, res, dem.algorithm) {
  if(dem.algorithm == "tin") dtm <- rasterize_terrain(
    las, res = res, algorithm = tin(), use_class = 2L)
  if(dem.algorithm == "knnidw") dtm <- rasterize_terrain(
    las, res = res, algorithm = knnidw(), use_class = 2L)
  if(dem.algorithm == "kriging") dtm <- rasterize_terrain(
    las, res = res, algorithm = kriging(), use_class = 2L)
  return(dtm)
}
