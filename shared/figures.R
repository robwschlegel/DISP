# figures.R
# The purpose of this script is to house all of the code used to make the figures for the final publication


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)


# Data --------------------------------------------------------------------

source("shared/statistics.R")


# Figure 1 ----------------------------------------------------------------
# The map
map <- ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
  geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
  geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
  geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
  borders(fill = "ivory2", colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  labs(x = NULL, y = NULL)
  labs(x = "Longitude", y = "Latitude")
ggsave(filename = "shared/map.png", plot = map, height = 5, width = 6)


# Figure 2 ----------------------------------------------------------------
# MHW figure
ggplot(data = north_MHW, aes())
heatwaveR::event_line(north_MHW)


# Figure 3 ----------------------------------------------------------------
# Correlation figure
ggplot(data = north_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ",round(north_cor[1,2],2)), x = 1.2, y = 20))

