#' @title Derive CHMs and DEMs.
#' @description
#' This function splits large LiDAR point cloud files (`.las` or `.laz`) into smaller tiles when their file size exceeds a user-defined threshold (`max.size`). 
#' Each output tile will be approximately `cut.off` GB in size, split along the X-axis with an optional spatial buffer to avoid edge artifacts. 
#' This is useful for managing large files that are too big to process efficiently or that exceed memory constraints. Note, any original files which have been split will be deleted if delete = T (default).
#' @param all.tiles Character vector. Full file paths to `.las` or `.laz` files to be evaluated and potentially split. Use `list.files(..., full.names = TRUE)` to generate.
#' @param defined_proj Numeric. Threshold file size in gigabytes (GB) above which a file will be split. Files smaller than this are ignored.
#' @param outdir Numeric. Approximate target size in gigabytes (GB) for each split tile. Determines the number of output chunks.
#' @param res Numeric. Spatial resolution of outputs (in metres). 
#' @param canopyHeight Numeric. Width of buffer to include on either side of each tile (in the same units as the point cloud's coordinate system). Helps prevent artifacts along tile edges.
#' @param dem.algorithm Character. Interpolation method to use for rasterizing terrain. Options are `"tin"` (default), `"knnidw"`, or `"kriging"`.
#' @import lidR
#' @export
.LASprocess <- function(all.tiles, defined_proj, outdir,res,
                        canopyHeight = NULL, dem.algorithm = "tin"){
  
  #' Loops through multiple tiles if needed.
  for (a in 1:length(all.tiles)) {
    tryCatch({
      #' **Read in .las/.laz tile**
      las <- readLAS(all.tiles[a])
      tile <- sub(
        ".*_([0-9]+)$",
        "\\1",
        tools::file_path_sans_ext(basename(all.tiles[a]))
      )
      print(paste0("Processing tile: ", tile, "... ", a, " of ", length(all.tiles), "."))
      
      #' **Initial Checks**
      .stopifnotlas(las)
      st_crs(las) <- defined_proj
      chk <- las_check(las, print = FALSE)
      
      #' **Ground Classification**
      if ("The point cloud is ground classified" %in% chk$messages) {
        print("The point cloud is ground classified.")
      } else {
        print("The point cloud isn't ground classified. Classifying ground points...")
        las <- classify_ground(las, algorithm = pmf())
      }
      
      #' **Normalise Heights**
      if ("The point cloud is height normalized" %in% chk$messages) {
        print("The point cloud is height normalized.")
        las_norm <- las
      } else {
        print("The point cloud isn't height normalised. Normalising heights...")
        las_norm <- normalize_height(las, tin())
      }
      
      #' **Digital Elevation Model**
      print("Calculating a digital elevation model...")
      dem <- .makeDEM(las, res, dem.algorithm)
      
      path <- paste0(outdir, "dem/")
      dir.create(path, showWarnings = FALSE)
      writeRaster(dem, paste0(path, "dem_", tile, ".tif"), overwrite = TRUE)
      rm(las)  # save memory
      
      #' **Canopy Height Model**
      if (is.null(canopyHeight)) {
        chm <- .CanopyModel(las_norm, res)
        path <- paste0(outdir, "chm/")
        dir.create(path, showWarnings = FALSE)
        writeRaster(chm, paste0(path, "chm_", tile, ".tif"), overwrite = TRUE)
      }
      rm(las_norm)
      gc()
    }, error = function(e) {
      warning(paste("Error processing tile", all.tiles[a], ":", e$message))
    })
    gc() }
}
