#Figures document, putting figures into word documents
#March 7th, 2019
#Caelin Randall-Scott



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(lubridate)
library(gridExtra)
source("shared/statistics.R")




# Figure 1 ----------------------------------------------------------------
#The Map of study area
ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
  geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
  geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
  geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
  borders(fill = "ivory2", colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  labs(x = NULL, y = NULL) +
  labs(x = "Longitude", y = "Latitude")

ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "green", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = month_count), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  facet_wrap(~month)


# Figure 2 ----------------------------------------------------------------
#Lolli plot
#Explain what a MHW is visually using a lolli plot of a flame diagram

Plot1 <- lolli_plot(MHW_north, event_count = 5)

Plot2 <- lolli_plot(MHW_south, event_count = 5)

grid.arrange(Plot1, Plot2, nrow=2, ncol=1)


event_line(res, spread = 100, metric = "intensity_max_relThresh",
           start_date = "2006-01-01", end_date = "2016-12-23", category = TRUE)
# Figure 3 ----------------------------------------------------------------
#North Correlation
#Scatter plots... y=Mean intensity, x=CPUE... and maybe include a best fit line

North <- ggplot(data = north_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ", round(north_cor[1,2],2)), x = 1.2, y = 20))

# Figure 4 ----------------------------------------------------------------
#South Correlation
#Scatter plots... y=Mean intensity, x=CPUE... and maybe include a best fit line

South <- ggplot(data = south_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ", round(north_cor[1,2],2)), x = 1.2, y = 20))


# Figure 5  ---------------------------------------------------------------
#Fundy Correlation 
#Scatter plots... y=Mean intensity, x=CPUE... and maybe include a best fit line

Fundy <- ggplot(data = fundy_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ", round(north_cor[1,2],2)), x = 1.2, y = 20))






# Figures 3, 4, and 5 -----------------------------------------------------

library(ggpubr)
ggarrange(North, South, Fundy)
