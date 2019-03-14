# study_areas.R
# The purpose of this script is to ascertain which catch areas belong in the North or South study area
# This is done by determining which areas are sampled only during spring/summer and which are sampled year-round


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

source("shared/load.R")


# Month proportions -------------------------------------------------------

temperature_month_proportion <- bot_temp_CPUE %>% 
  mutate(month = lubridate::month(date, label = TRUE)) %>% 
  na.omit() %>% 
  group_by(grid) %>% 
  mutate(total_count = n()) %>%
  group_by(grid, month, total_count) %>%
  summarise(month_count = n()) %>% 
  mutate(month_prop = round(month_count/total_count, 2))

# Visualise the count per month
ggplot(temperature_month_proportion, aes(x = month, y = grid)) +
  geom_raster(aes(fill = month_count)) +
  scale_fill_distiller(na.value = "white", palette = "RdPu", direction = 1)

# Visualise the proportion per month
ggplot(temperature_month_proportion, aes(x = month, y = grid)) +
  geom_raster(aes(fill = month_prop)) +
  scale_fill_distiller(na.value = "white", palette = "OrRd", direction = 1)


# Study areas -------------------------------------------------------------

# Determine the study areas
north_area <- temperature_month_proportion %>% 
  filter(month == "Jun", grid > 300) %>% 
  ungroup() %>% 
  select(grid) %>% 
  left_join(bot_temp_CPUE, by = "grid")

south_area <- temperature_month_proportion %>% 
  filter(month == "Jan", grid >= 60) %>% 
  ungroup() %>% 
  select(grid) %>% 
  left_join(bot_temp_CPUE, by = "grid")

fundy_area <- temperature_month_proportion %>% 
  filter(grid < 60) %>% 
  ungroup() %>% 
  select(grid) %>% 
  left_join(bot_temp_CPUE, by = "grid")

# Create smaller dataframes of just the grid areas for each study area
north_grid <- left_join(north_area, grid_polys, by = "grid") %>% 
  select(grid, lon, lat) %>% 
  unique()

south_grid <- left_join(south_area, grid_polys, by = "grid") %>% 
  select(grid, lon, lat) %>% 
  unique()

fundy_grid <- left_join(fundy_area, grid_polys, by = "grid") %>% 
  select(grid, lon, lat) %>% 
  unique()

# Visualise the three different areas
ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
  geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
  geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
  geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
  borders(fill = "ivory2", colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  labs(x = "Longitute", y = "Latitude")

# Visualise the time series within each region
north_ts <- ggplot(data = north_area, aes(x = date, y = temp)) +
  geom_line(aes(group = grid), colour = "navy", alpha = 0.7)

south_ts <- ggplot(data = south_area, aes(x = date, y = temp)) +
  geom_line(aes(group = grid), colour = "green4", alpha = 0.7)

fundy_ts <- ggplot(data = fundy_area, aes(x = date, y = temp)) +
  geom_line(aes(group = grid), colour = "orange", alpha = 0.7)

all_ts <- ggpubr::ggarrange(north_ts, south_ts, fundy_ts, ncol = 1, nrow = 3)
# all_ts

# Visualise the time series within each region
north_ts <- ggplot(data = north_area, aes(x = date, y = temp)) +
  geom_line(aes(group = grid), colour = "navy", alpha = 0.7)

south_ts <- ggplot(data = south_area, aes(x = date, y = temp)) +
  geom_line(aes(group = grid), colour = "green4", alpha = 0.7)

fundy_ts <- ggplot(data = fundy_area, aes(x = date, y = temp)) +
  geom_line(aes(group = grid), colour = "orange", alpha = 0.7)

all_ts <- ggpubr::ggarrange(north_ts, south_ts, fundy_ts, ncol = 1, nrow = 3)
# all_ts

# ggplot(north_area, aes(x = lon, y = lat, group = grid)) +
#   geom_polygon(data = north_grid, fill = "navy", colour = "black", alpha = 0.7) +
#   geom_polygon(data = south_grid, fill = "green4", colour = "black", alpha = 0.7) +
#   geom_polygon(data = fundy_grid, fill = "orange", colour = "black", alpha = 0.7) +
#   borders(fill = "ivory2", colour = "black") +
#   coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
#   labs(x = "Longitude", y = "Latitude")

# Visualise the time series within each region
# north_ts <- ggplot(data = north_area, aes(x = date, y = temp)) +
#   geom_line(aes(group = grid), colour = "navy", alpha = 0.7)
# 
# south_ts <- ggplot(data = south_area, aes(x = date, y = temp)) +
#   geom_line(aes(group = grid), colour = "green4", alpha = 0.7)
# 
# fundy_ts <- ggplot(data = fundy_area, aes(x = date, y = temp)) +
#   geom_line(aes(group = grid), colour = "orange", alpha = 0.7)
# 
# all_ts <- ggpubr::ggarrange(north_ts, south_ts, fundy_ts, ncol = 1, nrow = 3)
# all_ts


# Finding funny time series -----------------------------------------------

odd_ts <- south_area %>%
  filter(date >= "2015-01-01", date <= "2015-04-30")

odd_mean <- south_area %>%
  filter(date >= "2015-01-01", date <= "2015-04-30") %>%
  group_by(date) %>%
  summarise(mean_temp = mean(temp, na.rm = T))

ggplot(odd_ts, aes(x = date, y = temp)) +
  geom_line(aes(colour = as.factor(grid))) +
  geom_line(data = odd_mean, aes(y = mean_temp))

# The few odd time series do not appear to have a large effect on the overall mean and so have not been removed

