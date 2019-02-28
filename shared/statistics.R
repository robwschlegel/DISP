# statistics.R
# With the study areas determined, the purpose of this script is to run the statistics necessary
# For this project we will need to calculate the MHWs that occurred in each study area,
# and then correlate the mean CPUE for those areas against the MHW metrics


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(corrgram)


# Data --------------------------------------------------------------------

source("shared/study_areas.R")

# Create mean temperature time series for each area
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


# MHWs --------------------------------------------------------------------

# Each clump below calculates the MHWs, their categories, and joins those results to the base data

## North study area= 
north_MHW <- detect_event(ts2clm(north_temp, 
                                 climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7))
north_cat <- category(north_MHW)
north_MHW_res <- north_temp %>% 
  left_join(north_MHW$climatology, by = c("t", "temp")) %>% 
  left_join(north_MHW$event, by = "event_no") %>% 
  left_join(north_cat, by = c("event_no", "duration"))

## South study area
south_MHW <- detect_event(ts2clm(south_temp, 
                                 climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7))
south_cat <- category(south_MHW)
south_MHW_res <- south_temp %>% 
  left_join(south_MHW$climatology, by = c("t", "temp")) %>% 
  left_join(south_MHW$event, by = "event_no") %>% 
  left_join(south_cat, by = c("event_no", "duration"))

## Fundy study area
fundy_MHW <- detect_event(ts2clm(fundy_temp, 
                                 climatologyPeriod = c("2006-01-01", "2016-12-23"), maxPadLength = 7))
fundy_cat <- category(fundy_MHW)
fundy_MHW_res <- fundy_temp %>% 
  left_join(fundy_MHW$climatology, by = c("t", "temp")) %>% 
  left_join(fundy_MHW$event, by = "event_no") %>% 
  left_join(fundy_cat, by = c("event_no", "duration"))


# Correlations ------------------------------------------------------------

# The clumps below produce the base correlation stats as well as a correlogram

## North study area
north_cor <- cor(select(north_MHW_res, mean_CPUE, duration, intensity_mean:rate_decline), 
                 use = "pairwise.complete.obs", method = "pearson")
corrgram(select(north_MHW_res, mean_CPUE, duration, intensity_mean:rate_decline), 
         order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "CPUE against all MHW metrics in the north study area") 

## South study area
south_cor <- cor(select(south_MHW_res, mean_CPUE, duration, intensity_mean:rate_decline), 
                 use = "pairwise.complete.obs", method = "pearson")
corrgram(select(south_MHW_res, mean_CPUE, duration, intensity_mean:rate_decline), 
         order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "CPUE against all MHW metrics in the south study area") 

## Fundy study area
fundy_cor <- cor(select(fundy_MHW_res, mean_CPUE, duration, intensity_mean:rate_decline), 
                 use = "pairwise.complete.obs", method = "pearson")
corrgram(select(fundy_MHW_res, mean_CPUE, duration, intensity_mean:rate_decline), 
         order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "CPUE against all MHW metrics in the fundy study area") 
