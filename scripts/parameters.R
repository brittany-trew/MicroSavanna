#' Manuscript: Revealing the hidden microclimates of savanna habitat mosaics
#' Date: August 2026
#' Author: Brittany Trew (*brittany.trew@fas.harvard.edu*)
#' Description: XXXX
#' ----------------------------------------------------------------------------
#' ----------------------------------------------------------------------------
scripts.path <- "scripts/"
upscaler::source_batch(paste0(scripts.path,"helpers"))

#' [Required R packages]
packages <- c("dplyr", "tidyr", "readr", "microclimf",
              "myClim", "ggplot2", "sf", 'lidR',"RCSF", "RMCC", "Rcpp", 
              "rgl", "luna", "geodata","sf","xml2","jsonlite",
              "httr", "ecmwfr", "mcera5", "lubridate", "curl",
              "mgcv", "colorspace", "dendextend", "cluster", "factoextra","terra",
              "canopyLazR","patchwork")
load_packages(packages)

#' [Set Pathways.]
#' (1) LiDAR Point Clouds.
lidar.data <- "/Volumes/HD01/LIDAR/"

#' (4) Input and output locations for the manuscript analysis (post microclimate modelling)
in.data <- "data/"
out.data <- "output/"
pad.path <- paste0(in.data,"pad/")
dir.create(pad.path)

site <- "Gorongosa"
tile <- 13
lidar.yr <- 2023
defined_proj <- "EPSG:32736"

logger.dat <- paste0(in.data,"Loggers_Aug2026/")
log.records <- read.csv(paste0(in.data,"logger_locs.csv"),header = T)

res <- 1; dzd <- 0.2

all.tiles <- list.files(paste0(lidar.data,site,"/","Gorongosa",tile,"/",lidar.yr), full.names = T)

savanna_cols <- c(
  sand         = "#F2C38E",
  apricot      = "#E6A360",
  ochre        = "#C4742B",
  brown        = "#6D451D",
  dark_brown   = "#32150A",
  jungle_shade = "#1A1E0D",
  palm_canopy  = "#353D13",
  olive_drift  = "#626B2D"
)
