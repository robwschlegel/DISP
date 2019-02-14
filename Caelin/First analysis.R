#First analysis Learning

library(tidyverse)
library(lubridate)


# Load Data ---------------------------------------------------------------

source(file = "shared/load.R")

#use this when you want to use this data

# Analysis ----------------------------------------------------------------

new_data <- bot_temp_CPUE %>% 
  filter(grid %in% c(1, 20, 8))

total_count_grid <- bot_temp_CPUE %>% 
  group_by(grid) %>% 
  na.omit(CPUE) %>% 
  summarise(CPUE_Count = n()) %>% #n counts the number of things you give it
  arrange(-CPUE_Count)


temp_data <- bot_temp_CPUE %>% 
  group_by(grid) %>% 
  summarise(mean_temperature = mean(temp, na.rm = TRUE)) %>% 
  na.omit() %>% #discards all missing data
  arrange(-mean_temperature)



# Figures -----------------------------------------------------------------

ggplot(data = new_data, aes(x = date, y = temp)) + 
  geom_line()

histogram_1 <- ggplot(data = total_count_grid, aes(x = CPUE_Count)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Count") +
  geom_histogram(fill = "turquoise")
histogram_1 #run histogram_1 or whatever you named your figure after you run the command and it goes in your environment then run histogram_1


#for homework make a box plot and think of a human question for anything to do with these lobster data.. bonus gold star write the code to answer your question

box_1 <- ggplot(data = total_count_grid, aes(x = grid, y = CPUE_Count)) +
  geom_boxplot(aes(fill = CPUE_Count, group = grid)) + #here it told me to add a group, what does that do?
  labs(x = "grid", y = "CPUE_Count")
box_1
