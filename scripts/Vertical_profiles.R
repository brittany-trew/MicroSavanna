#' Example: Woodland.
scripts.path <- "scripts/"
source(paste0(scripts.path,"parameters.R"))

lidar.products <- paste0(in.data,"lidar_products/")
pad <- rast(paste0(pad.path,"pad_voxels_000001.tif"))
pad <- pad[[-1]]
#' Extract PAD profiles.
chm <- rast(paste0(lidar.products,"chm.tif"))
chm_cropped <- crop(chm,pad)
chm_mask <- chm_cropped
chm_mask[chm_mask < 0.2] <- NA

#' Process PAD layers.
#' **If multiple tiles, they need to be padded with NAs at higher levels because tiles have different heights.**
#' pad0 <- processPAD(pad.r, chm)
  
start_heights <- seq(
  from = dzd,
  by = dzd,
  length.out = nlyr(pad)
)
  
df1 <- do.call(rbind, lapply(seq_len(nlyr(pad)), function(j) {
    data.frame(height = start_heights[j],
               pad = values(pad[[j]], mat = FALSE))}))

#' Remove Na's and 0's
dff <- df1 %>%
  dplyr::mutate(pad = tidyr::replace_na(pad, 0))

#' Remove extreme outliers
pad_cap <- quantile(dff$pad, 0.99, names = FALSE)

overall_summary <- dff %>%
  dplyr::mutate(pad_scaled = pmin(pad, pad_cap) / pad_cap) %>%
  dplyr::group_by(height) %>%
  dplyr::summarise(
    mean_pad = mean(pad_scaled),
    median_pad = median(pad_scaled),
    lo = quantile(pad_scaled, 0.1),
    hi = quantile(pad_scaled, 0.9),
    .groups = "drop"
  ) %>%
  dplyr::arrange(height)

final_plot <- ggplot(overall_summary, aes(y = height)) +
  geom_ribbon(
    aes(xmin = lo, xmax = hi),
    orientation = "y",
    fill = savanna_cols[["palm_canopy"]],
    alpha = 0.5
  ) +
  geom_path(
    aes(x = mean_pad),
    colour = savanna_cols[["jungle_shade"]],
    linewidth = 1
  )+
  labs(title = "Mixed Woodland",
       x = "Relative Plant Area Density", 
       y = "Height (m)")+
  theme_classic()

final_plot

