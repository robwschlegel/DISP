#Feb. 14th, 2019
#Week 6
#Starting Correlations 


# Libraries ----------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(lubridate)
library(gridExtra)

# Load  -------------------------------------------------------------------

source(file = "shared/load.R")
load(file = "Caelin/MHW_north.Rdata")
load(file = "Caelin/MHW_south.Rdata")

# Rename ------------------------------------------------------------------
#This renames the date coloum in bot_temp_CPUE from "data" to "t"
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

cor(x = North$CPUE, y = North$thresh, use = "complete.obs")

cor(x = South$CPUE, y = South$intensity_cumulative, use = "complete.obs")


# Correlogram -------------------------------------------------------------

library(corrgram)
corrgram(North, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order") 

#homework: read hobday paper *table specifically*
#homework: correlogram, remove colombs you may not need, they are mess now

