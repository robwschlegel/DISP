# Calisa Staniforth
# Thursday, Jan 24th, 2019 (Week 3)
# 1st Analysis

# Week 3 ------------------------------------------------------------------

library(tidyverse)

library(lubridate)


# Load data ---------------------------------------------------------------

source(file='shared/load.R')

# 1st Data Analysis (lobsters) -------------------------------------------------

new_data <- bot_temp_CPUE %>% 
  filter(grid%in%c(318,22,47))

total_count_grid <- bot_temp_CPUE %>% 
  group_by(grid) %>% 
  na.omit(CPUE) %>% 
  summarise(CPUE_count=n()) %>% 
  arrange(CPUE_count)

average_temp <- bot_temp_CPUE %>% 
  group_by(grid) %>% 
  summarise(mean_temp=mean(temp, na.rm = T)) %>% 
  na.omit() %>% 
  arrange(mean_temp)



  
# Figures -----------------------------------------------------------------

ggplot(data = new_data, aes(x= date, y = temp)) +
         geom_line()

histogram_1 <- ggplot(data = total_count_grid, aes(x = CPUE_count)) +
  geom_histogram(aes(bindwidth=2)) + 
  labs(x = "CPUE_count")
histogram_1


# BoxPlot -----------------------------------------------------------------

box_1 <- ggplot(data = bot_temp_CPUE, aes(x = grid, y = CPUE)) +
  geom_boxplot(aes(fill = CPUE, group=grid)) +
  labs(x = "grid", y = "CPUE")
box_1


