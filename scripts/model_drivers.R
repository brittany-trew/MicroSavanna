#' Set up models.
#' Compare point with gridded version?

scripts.path <- "scripts/"
source(paste0(scripts.path, "parameters.R"))

#' For each logger.
files <- list.files(logger.dat, pattern = ".csv", full.names = T)
logger.list <- list()
