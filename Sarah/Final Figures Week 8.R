# 1st Analysis - Week 3
#
# Sarah Swim
# January 24th 2019


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(cowplot)


# Data --------------------------------------------------------------------

source("shared/statistics.R")
source("shared/load.R")


# Figure 1 ----------------------------------------------------------------

# Study area map
# Temperature yearly
map1 <- ggplot(data = grid_polys_extra, aes(x = lon, y = lat, group = grid)) +
  borders(fill = "ivory2", colour = "black") +
  geom_polygon(aes(fill = mean_temp), colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Longitute", y = "Latitude") +
  guides(fill = guide_colorbar(title = "Mean\nTemp")) +
  theme(legend.justification = c(0,1), legend.position = c(0,1), legend.direction = "horizontal", legend.background = element_rect(fill = "white"))
  

map2 <- ggplot(data = grid_polys_extra_CPUE, aes(x = lon, y = lat, group = grid)) +
  borders(fill = "ivory2", colour = "black") +
  geom_polygon(aes(fill = CPUE_count), colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "A") +
  labs(x = "Longitude", y = "Latitude") +
  guides(fill = guide_colorbar(title = "Mean\nCPUE")) +
  theme(legend.justification = c(0,1), legend.position = c(0,1),
        panel.background = element_rect(fill = "aquamarine"), legend.direction = "horizontal", legend.background = element_rect(fill = "white"))

map3 <- ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
 geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
  geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
  geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
  borders(fill = "ivory2", colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  labs(x = "Longitute", y = "Latitude") #use for figures in report paper

map <- ggarrange(map1, map2, map3, nrow = 3, ncol = 1)

ggsave(filename = "Sarah/map.png", plot = map, height = 12, width = 6)

map

# Figure 2 ----------------------------------------------------------------

# MHW

heatwaveR::event_line(north_MHW)

# Lolliplots (for time series)
# Flame Plots (for specific time)

# explain what a MHW is visually


# Figure 3 ----------------------------------------------------------------

# Correlation

ggplot(data = north_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ",round(north_cor[1,2], 2)), x = 1.2, y = 20))
    
# Scatter plot (best and worst correlation?)
# y = mean intensity, x = CPUE, with line


