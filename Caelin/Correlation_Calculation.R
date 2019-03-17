#Feb. 14th, 2019
#Week 6
#Starting Correlations 


# Libraries ----------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(lubridate)
library(gridExtra)
library(corrgram)


# Load  -------------------------------------------------------------------
source("shared/study_areas.R")
source(file = "shared/load.R")
load(file = "Caelin/MHW_north.Rdata")
load(file = "Caelin/MHW_south.Rdata")

# Rename ------------------------------------------------------------------
#This renames the date coloum in bot_temp_CPUE from "data" to "t"
bot_temp_CPUE = bot_temp_CPUE %>% 
  rename(t = date)

north_temp <- north_area %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            mean_CPUE = mean(CPUE, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

south_temp <- south_area %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            mean_CPUE = mean(CPUE, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

fundy_temp <- fundy_area %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            mean_CPUE = mean(CPUE, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

# Temperatures ------------------------------------------------------------

temperature_north <- bot_temp_CPUE %>%
  filter(grid %in% 322:368) %>%  #this is how we filter something
  group_by(date = t) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)


temperature_south <- bot_temp_CPUE %>% 
  filter(!grid %in% 322:368) %>%  #! means we want the grid coloums that are not 323:368
  group_by(date = t) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)

temperature_fundy <- bot_temp_CPUE %>% 
  filter(!grid %in% 322:368) %>%  #! means we want the grid coloums that are not 323:368
  group_by(date = t) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  dplyr::rename(t = date, temp = mean_temp)


# Climatologies -----------------------------------------------------------

climatology_north <- ts2clm(temperature_north, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)

climatology_south <- ts2clm(temperature_south, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)

climatology_fundy <- ts2clm(temperature_fundy, climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7)


# MHWs --------------------------------------------------------------------

MHW_north <- detect_event(data = climatology_north)

MHW_south <- detect_event(data = climatology_south)  

MHW_fundy <- detect_event(data = climatology_fundy)  

# North -----------------------------------------------------------------

north_MHW <- detect_event(ts2clm(north_temp, 
                                 climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7))
north_cat <- category(north_MHW)
north_MHW_res <- north_temp %>% 
  left_join(north_MHW$climatology, by = c("t", "temp")) %>% 
  left_join(north_MHW$event, by = "event_no") %>% 
  left_join(north_cat, by = c("event_no", "duration"))

North <- bot_temp_CPUE %>% 
  left_join(MHW_north$climatology, by = "t") %>% 
  left_join(MHW_north$event, by = "event_no")

# South -------------------------------------------------------------------

south_MHW <- detect_event(ts2clm(south_temp, 
                                 climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7))
south_cat <- category(south_MHW)
south_MHW_res <- south_temp %>% 
  left_join(south_MHW$climatology, by = c("t", "temp")) %>% 
  left_join(south_MHW$event, by = "event_no") %>% 
  left_join(south_cat, by = c("event_no", "duration"))

South <- bot_temp_CPUE %>% 
  left_join(MHW_south$climatology, by = "t") %>% 
  left_join(MHW_south$event, by = "event_no")

# Fundy -------------------------------------------------------------------

fundy_MHW <- detect_event(ts2clm(fundy_temp, 
                                 climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7))
fundy_cat <- category(fundy_MHW)
fundy_MHW_res <- fundy_temp %>% 
  left_join(fundy_MHW$climatology, by = c("t", "temp")) %>% 
  left_join(fundy_MHW$event, by = "event_no") %>% 
  left_join(fundy_cat, by = c("event_no", "duration"))

Fundy <- bot_temp_CPUE %>% 
  left_join(MHW_fundy$climatology, by = "t") %>% 
  left_join(MHW_fundy$event, by = "event_no")

# Correlation CPUE and duration -------------------------------------------------------------

cor(x = North$CPUE, y = North$duration, use = "complete.obs")

cor(x = South$CPUE, y = South$duration, use = "complete.obs")

cor(x = Fundy$CPUE, y = Fundy$duration, use = "complete.obs")


# Correlation CPUE and intensite_mean_relThresh -------------------------------------

cor(x = North$CPUE, y = North$intensity_mean_relThresh, use = "complete.obs")

cor(x = South$CPUE, y = South$intensity_mean_relThresh, use = "complete.obs")

cor(x = Fundy$CPUE, y = Fundy$intensity_mean_relThresh, use = "complete.obs")



# Correlation CPUE and intensity_mean_abs----------------------------------------------------

cor(x = North$CPUE, y = North$intensity_mean_abs, use = "complete.obs")

cor(x = South$CPUE, y = South$intensity_mean_abs, use = "complete.obs")

cor(x = Fundy$CPUE, y = Fundy$intensity_mean_abs, use = "complete.obs")


# Correlation CPUE and season----------------------------------------------------

cor(x = North$CPUE, y = North$seas, use = "complete.obs")

cor(x = South$CPUE, y = South$seas, use = "complete.obs")

cor(x = Fundy$CPUE, y = Fundy$seas, use = "complete.obs")




