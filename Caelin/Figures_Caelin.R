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
Study_Area <- ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
  geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
  geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
  geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
  borders(fill = "ivory2", colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  labs(x = NULL, y = NULL) +
  labs(x = "Longitude", y = "Latitude") +
  labs(title = "Map of Study Areas",
       subtitle = "Divided by colour",
       legend = c('North', 'South', 'Fundy'))
Study_Area

ggsave(filename = "Caelin/Map_of_Study_Area.png")


# Figure 2 ----------------------------------------------------------------

#Month Count Temperatures
library(tidyverse)
library(heatwaveR)
library(lubridate)
library(gridExtra)
library(ggplot2)

source(file = "shared/load.R")

temperature_month_summary <- bot_temp_CPUE %>% 
  mutate(month = lubridate::month(date, label = TRUE)) %>% 
  na.omit() %>%
  group_by(grid, month) %>% 
  summarise(month_count = n())

grid_polys_extra <- grid_polys %>% 
  left_join(temperature_month_summary, by = "grid")

ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "bisque4", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = month_count), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  facet_wrap(~month) +
  labs(fill = "Month\nCount", x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.9,0.1))

ggsave("Caelin/Temperature_Month_Summary.png", height = 7, width = 9)
# Figure 3 ----------------------------------------------------------------

library(tidyverse)
source(file = "shared/load.R")
require(gridExtra)


mean_data <-  bot_temp_CPUE %>% 
  group_by(grid) %>% 
  summarise(mean_temperature = mean(temp, na.rm = TRUE),
            mean_CPUE = mean(CPUE, na.rm = TRUE)) %>% 
  na.omit() %>% #discards all missing data
  arrange(-mean_temperature)

grid_polys_extra <- grid_polys %>% 
  left_join(mean_data, by = "grid")

plot1 <- ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "bisque4", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = mean_temperature), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean\nTemp.")

plot2 <- ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "bisque4", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = mean_CPUE), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Longitude", y = "Latitude", fill = "Mean\nCPUE")

grid.arrange(plot1, plot2, nrow=2, ncol=1)


# Scatter plot ------------------------------------------------------------

duration = North$duration 
CPUE = North$CPUE    
plot(duration, CPUE,  
      xlab="CPUE", 
      ylab="Duration (days)",
     abline(lm(CPUE ~ duration))) 


# Figure 4 ----------------------------------------------------------------
#Lolli plot
#Explain what a MHW is visually using a lolli plot of a flame diagram

temp_north <- bot_temp_CPUE %>%
  filter(grid %in% 323:368) %>% # filter by different values
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

temp_south <- bot_temp_CPUE %>% 
  filter(grid %in% 92:322) %>% # '!' mark means not the thing we want
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

temp_fundy <- bot_temp_CPUE %>% 
  filter(!grid %in% 92:368) %>% # '!' mark means not the thing we want
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)


climat_north <- ts2clm(temp_north, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7) # fills the gaps in to make smooth lines

climat_south <- ts2clm(temp_south, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)

climat_fundy <- ts2clm(temp_fundy, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)


MHW_north <- detect_event(data = climat_north)

MHW_south <- detect_event(data = climat_south)

MHW_fundy <- detect_event(data = climat_fundy)


p1 <- lolli_plot(MHW_north, event_count = 3)

p2 <- lolli_plot(MHW_south, event_count = 3)

p3 <- lolli_plot(MHW_fundy, event_count = 3)

ggpubr::ggarrange(p1, p2, p3, labels = "AUTO", ncol = 1, nrow = 3)


plot_1 <- event_line(data = MHW_north, metric = "intensity_max", category = TRUE)

plot_2 <- event_line(data = MHW_south, metric = "intensity_max", category = TRUE)

plot_3 <- event_line(data = MHW_fundy, metric = "intensity_max", category = TRUE)


grid.arrange(plot_1, plot_2, plot_3, nrow = 3, ncol = 1)



event_line(res, spread = 100, metric = "intensity_max_relThresh",
           start_date = "2006-01-01", end_date = "2016-12-23", category = TRUE)
# Figure 5 ----------------------------------------------------------------
#North Correlation
#Scatter plots... y=Mean intensity, x=CPUE... and maybe include a best fit line

North_Duration <- ggplot(data = north_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE")

North_temp <- ggplot(data = north_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = NA, x = 1, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE")

grid.arrange(North_Duration, North_temp)


South_Duration <- ggplot(data = south_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE")

South_temp <- ggplot(data = south_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = NA, x = 0.6, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE")

grid.arrange(South_Duration, South_temp, ncol = 2, nrow =1)


Fundy_Duration <- ggplot(data = fundy_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE")

Fundy_temp <- ggplot(data = fundy_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE")

ggarrange(Fundy_Duration, Fundy_temp)


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

North <- ggplot(data = north_MHW_res, aes(x = mean_CPUE, y = duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ", round(north_cor[1,2],2)), x = 1.2, y = 20))






# Figures 3, 4, and 5 -----------------------------------------------------

library(ggpubr)
ggarrange(North, South, Fundy)



#From Sarah
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
  geom_polygon(aes(fill = mean_temperature), colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Longitute", y = "Latitude") +
  guides(fill = guide_colorbar(title = "Mean\nTemp")) +
  theme(legend.justification = c(0,1), legend.position = c(0,1), legend.direction = "horizontal", legend.background = element_rect(fill = "white"))


map2 <- ggplot(data = grid_polys_extra, aes(x = lon, y = lat, group = grid)) +
  borders(fill = "ivory2", colour = "black") +
  geom_polygon(aes(fill = mean_CPUE), colour = "black") +
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

mapfinal <- ggarrange(map1, map2, map3, nrow = 3, ncol = 1)

ggsave(filename = "Caelin/map.png", plot = map, height = 12, width = 6)

mapfinal




# Scatter Plot-------------------------------------------------------------

library(ggplot2)
head(Fundy)
ggplot(Fundy, aes(x = duration, y = CPUE)) +
  geom_point(shape=1) +  geom_smooth(method=lm , color="red", se=FALSE)
  