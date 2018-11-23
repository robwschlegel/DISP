# The purpose of this script is to load all of the 
# lobster CPUE and temperature data


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(R.matlab)


# Data --------------------------------------------------------------------

# The CPUE data
## NB: This information is also found in "data/bot_temp_CPUE.csv"
# suppressMessages(
#   CPUE <- read_csv("data/GriddedCPUE.csv")
# )

# The polygon data
suppressMessages(
  grid_polys <- read_csv("data/GridPolys.csv")
)

grid_polys <- grid_polys %>% 
  rename(grid = SID)
write_csv(grid_polys, path = "data/GridPolys.csv")

# The bottom temperature data
suppressMessages(
  bot_temp_CPUE <- read_csv("data/bot_temp_CPUE.csv")
)

bot_temp <- readMat("data/CPUE_botTemp.mat")
bot_rows <- data.frame(date = as.Date(as.vector(bot_temp$t)-1, origin = "0000-01-01"),
                   year = as.vector(bot_temp$year),
                   week = as.vector(bot_temp$week),
                   day = as.vector(bot_temp$day))

bot_T <- as.data.frame(t(bot_temp$T))
colnames(bot_T) <- as.vector(bot_temp$grids)
bot_T <- cbind(bot_rows, bot_T) %>% 
  gather(key = grid, value = temp, -(date:day)) %>% 
  replace(is.na(.), NA)

bot_CPUE <- as.data.frame(t(bot_temp$CPUE))
colnames(bot_CPUE) <- as.vector(bot_temp$grids)
bot_CPUE <- cbind(bot_rows, bot_CPUE) %>% 
  gather(key = grid, value = CPUE, -(date:day)) %>% 
  replace(is.na(.), NA)

bot_temp_CPUE <- left_join(bot_T, bot_CPUE)

write_csv(bot_temp_CPUE, path = "data/bot_temp_CPUE.csv")

# Calculate the mean temperature per grid
poly_data <- bot_temp_CPUE %>% 
  group_by(grid) %>% 
  summarise(mean_temp = mean(temp, na.rm = T)) %>% 
  replace(is.na(.), NA)

# Join the mean temperatures to the grid dataframe
grid_polys <- grid_polys %>% 
  left_join(poly_data)

# Visualise the results
ggplot(grid_polys, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = grid, fill = mean_temp), colour = "black") +
  scale_fill_viridis_c()
