# Figures for the research project

# Libraries --------------------------

library(tidyverse)
library(ggpubr)

library(heatwaveR)
library(lubridate)

# Data -------------------------------

source("shared/statistics.R")

# Figure 1 ---------------------------
# Map of study area

ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
  geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
  geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
  geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
  borders(fill = "ivory2", colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  labs(x = NULL, y = NULL)
# you can change you  X to longtiude and Y to latitude BUT not needed

# Figure 2 ---------------------------
# MHW figure

# 1. Lolliplot

MHW_north <- detect_event(data=climatology_north)

MHW_south <- detect_event(data=climatology_south)

MHW_fundy <- detect_event(data =climatology_fundy)

plot1 <- lolli_plot(MHW_north, event_count = 7)

plot2 <- lolli_plot(MHW_south, event_count = 7)

plot3 <- lolli_plot(MHW_fundy, event_count = 7)

ggarrange(plot1, plot2, nrow = 2, ncol = 1) # end of lolliplot function

plot1.1 <- event_line(MHW_north, category = T)

plot1.2 <- event_line(MHW_south, category = T) 

ggarrange(plot1.1, plot1.2, nrow = 2, ncol = 1) # ggarrange figure

# 2. Flameplot

heatwaveR::event_line(north_MHW)


# Figure 3 ---------------------------
# Correlation figure

# Scatterplot

ggplot(data= north_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_label(aes(label = paste0("r = ", round(north_cor[1,2],2)), x = 1.2, y = 20))

