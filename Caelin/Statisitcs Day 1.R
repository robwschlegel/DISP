#February 7th, 2019
#Stats Analysis
#Caelin Randall-Scott


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(lubridate)
library(gridExtra)

# Load Data --------------------------------------------------------------

source(file = "shared/load.R")


# new object --------------------------------------------------------------

temperature_month_summary <- bot_temp_CPUE %>% 
  mutate(month = lubridate::month(date, label = TRUE)) %>% 
  na.omit() %>%
  group_by(grid, month) %>% 
  summarise(month_count = n())

grid_polys_extra <- grid_polys %>% 
  left_join(temperature_month_summary, by = "grid")


# Map ---------------------------------------------------------------------

ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "green", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = month_count), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D") +
  facet_wrap(~month)


# Comparing/separating North and South ------------------------------------

temperature_north <- bot_temp_CPUE %>%
  filter(grid %in% 322:368) %>%  #this is how we filter something
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)


temperature_south <- bot_temp_CPUE %>% 
  filter(!grid %in% 322:368) %>%  #! means we want the grid coloums that are not 323:368
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

temperature_fundy <- bot_temp_CPUE %>% 
  filter(!grid %in% 322:368) %>%  #! means we want the grid coloums that are not 323:368
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)


# Calculating Climatologies ------------------------------------------------------------

climatology_north <- ts2clm(temperature_north, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)

climatology_south <- ts2clm(temperature_south, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)

climatology_fundy <- ts2clm(temperature_fundy, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)


MHW_north <- detect_event(data = climatology_north)
  
MHW_south <- detect_event(data = climatology_south)  

MHW_fundy <- detect_event(data = climatology_fundy)  


# Figure ------------------------------------------------------------------

Plot1 <- lolli_plot(MHW_north, event_count = 5)

Plot2 <- lolli_plot(MHW_south, event_count = 5)

grid.arrange(Plot1, Plot2, nrow=2, ncol=1)


# Event Line Function -----------------------------------------------------


ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
res <- detect_event(ts)

event_line(res, spread = 100, metric = "intensity_max_relThresh",
           start_date = "2006-01-01", end_date = "2016-12-23", category = TRUE)


# Save --------------------------------------------------------------------

save(MHW_north, file = "Caelin/MHW_north.Rdata")
save(MHW_south, file = "Caelin/MHW_south.Rdata")

#I tried to use this code from your website and t had a bunch of NULLs... what does that mean
#then these errors...

#homework: event line function (the lame one) and show as time series (event_line) 
