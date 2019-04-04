# 1st Statistics - Week 5
# Statistical analysis of data for MHW and correlation
# Sarah Swim
# Feburary 7th 2019


# Libaries ----------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(lubridate)

# Load Data ---------------------------------------------------------------

source(file = "shared/load.R")

# Creating Temperature ----------------------------------------------------

temperature_month_summary <- bot_temp_CPUE %>% 
  mutate(month = lubridate::month(date, label = TRUE)) %>% 
  na.omit() %>% 
  group_by(grid, month) %>% 
  summarise(month_count = n())

grid_polys_extra_month <- grid_polys %>% 
  left_join(temperature_month_summary, by = "grid")

# Making maps -------------------------------------------------------------

# map of each month and the temperature
ggplot(data = grid_polys_extra_month, aes(x = lon, y = lat, group = grid)) +
  borders(fill = "ivory2", colour = "black") +
  geom_polygon(aes(fill = month_count), colour = "black") +
  coord_equal(xlim = c(-68, -58), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  labs(x = "Longitute", y = "Latitude") +
  guides(fill = guide_colorbar(title = "Mean\nTemp")) +
  theme(legend.position = "right", 
        panel.background = element_rect(fill = "aquamarine")) +
  facet_wrap(~month)


# Temperature Analysis ----------------------------------------------------


temp_north <- bot_temp_CPUE %>%
  filter(grid %in% 322:368) %>% # filter by different values
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

temp_south <- bot_temp_CPUE %>% 
  filter(!grid %in% 322:368) %>% # '!' mark means not the thing we want
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

temp_fundy <- bot_temp_CPUE %>% 
  filter(!grid %in% 322:368) %>% # '!' mark means not the thing we want
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)


# Calculating Climatology -------------------------------------------------

climat_north <- ts2clm(temp_north, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7) # fills the gaps in to make smooth lines
  
climat_south <- ts2clm(temp_south, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)

climat_fundy <- ts2clm(temp_south, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)


MHW_north <- detect_event(data = climat_north)

MHW_south <- detect_event(data = climat_south)

MHW_fundy <- detect_event(data = climat_fundy)



# Graphs ------------------------------------------------------------------

p1 <- lolli_plot(MHW_north, event_count = 3)

p2 <- lolli_plot(MHW_south, event_count = 3)

p3 <- lolli_plot(MHW_fundy, event_count = 3)


grid.arrange(p1, p2, p3)

 # to make time series

plot_1 <- event_line(data = MHW_north, metric = "intensity_max", category = TRUE)

plot_2 <- event_line(data = MHW_south, metric = "intensity_max", category = TRUE)

plot_3 <- event_line(data = MHW_fundy, metric = "intensity_max", category = TRUE)


grid.arrange(plot_1, plot_2, plot_3, nrow = 3, ncol = 1)


# New Object --------------------------------------------------------------

event_categories <- category(MHW_north, S = FALSE, name = "Nova Scotia")

event_categories2 <- category(MHW_south, S = FALSE, name = "Nova Scotia")


# Saving MHW Data ---------------------------------------------------------

save(MHW_north, file = "Sarah/MHW_north.Rdata")

save(MHW_south, file = "Sarah/MHW_south.Rdata")

#homework: event_line function (make flame) & show as time series for north and south
