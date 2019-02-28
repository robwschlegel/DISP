# Correlation for North and South- Week 6
#
# Sarah Swim
# February 14th 2019


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(lubridate)


# Load Data ---------------------------------------------------------------

source(file = "shared/load.R")
load(file = "Sarah/MHW_north.Rdata")
load(file = "Sarah/MHW_south.Rdata")

bot_temp_CPUE = bot_temp_CPUE %>% 
  rename(t = date)


# Joining -----------------------------------------------------------------

North <- bot_temp_CPUE %>% 
  left_join(MHW_north$climatology, by = "t") %>% 
  left_join(MHW_north$event, by = "event_no")

South <- bot_temp_CPUE %>% 
  left_join(MHW_south$climatology, by = "t") %>% 
  left_join(MHW_south$event, by = "event_no")

# Correlation -------------------------------------------------------------

cor(x = North$CPUE, y = North$intensity_cumulative_abs, use = "complete.obs")

cor(x = South$CPUE, y = South$intensity_cumulative_abs, use = "complete.obs")


# Correlogram -------------------------------------------------------------

library(corrgram)
corrgram(South, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="South Coast Correlation") 

library(corrgram)
corrgram(North, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="North Coast Correlation") 
#homework: read hobday paper *table specifically*
#homework: correlogram, remove colombs you may not need, they are mess now

