#' Logger Data.
library(dplyr)

logger.dat <- "data/Loggers_Aug2026/"
log.records <- read.csv("data/logger_locs.csv",header = T)

files <- list.files(logger.dat, pattern = ".csv", full.names = T)
logger.list <- list()

for(i in seq_along(files)) {
  
  log.dat <- read.csv(files[[i]], sep = ";", header = TRUE)
  serial.no <- as.numeric(unlist(strsplit(files[[i]], "_"))[[3]])
  print(serial.no)
  ind.record <- dplyr::filter(log.records, serial == serial.no)
  
  if(nrow(ind.record) == 0) next
  
  if(ind.record$height > 0) {
    
    log.dat <- log.dat[, c(2, 4)]
    
  } else if(ind.record$height == 0) {
    
    log.dat <- log.dat[, c(2, 6)]
  }
  
  colnames(log.dat) <- c("DateTime", "TMS")
  
  log.dat <- log.dat %>%
    mutate(
      DateTime = lubridate::parse_date_time(DateTime, orders = "Y.m.d H:M"),
      TMS = as.numeric(TMS)
    ) %>%
    filter(DateTime >= lubridate::ymd("2026-07-01")) %>%   # after June 30
    arrange(DateTime)
  
  logger.list[[i]] <- c(ind.record, log.dat)
}

logger_df <- purrr::map_dfr(logger.list, function(x) {
  tibble(
    Serial.Number = x$serial,
    Site.ID = x$site,
    Habitat = x$habitat,
    Height_m = x$height,
    Shade = x$shade,
    Lat = x$lot,
    Lon = x$lon,
    DateTime = lubridate::ymd_hms(x$DateTime),
    TMS = as.numeric(x$TMS)
  )
})

option3 <- c(
  "#c85a3e",
  "#eea83b",
  "#004d47",
  "#a09c33",
  "#192835",  
  "#68AE9A", 
  "#d9416b", 
  "#8A211B"  
)

require(purrr)
require(ggplot2)
logger_df <- logger_df %>%
  mutate(Height_m = factor(Height_m))

habitat_plots <- logger_df %>%
  split(.$Habitat) %>%
  map(~ ggplot(.x, aes(
    x = DateTime,
    y = TMS,
    colour = Height_m,
    group = Serial.Number
  )) +
    geom_line(
      alpha = 0.8,
      linewidth = 0.4,
      show.legend = TRUE
    ) +
    scale_colour_manual(values = unname(option3), drop = FALSE) +
    labs(
      x = "Date/time",
      y = "Temperature (°C)",
      colour = "Height (m)",
      title = paste("Logger observations -", unique(.x$Habitat))
    ) +
    theme_classic()
  )

plot_grids <- split(habitat_plots, rep(1:3, each = 5)) %>%
  map(function(plots) {
    
    plots <- imap(plots, function(p, name) {
      p +
        theme(
          legend.position = if (name == names(plots)[1]) "bottom" else "none",
          plot.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
    
    wrap_plots(plots, ncol = 2, guides = "collect") +
      plot_annotation(
        theme = theme(legend.position = "bottom")
      )
  })

print(plot_grids[[1]])
print(plot_grids[[2]])
print(plot_grids[[3]])

for (i in seq_along(plot_grids)) {
  ggsave(
    filename = paste0("habitat_grid_", i, ".png"),
    plot = plot_grids[[i]],
    width = 14,
    height = 10,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}


