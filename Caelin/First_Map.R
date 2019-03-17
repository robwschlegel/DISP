#January 31st, 2019
#Making Maps
#Study Area Map


# libraries ---------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

source(file = "shared/load.R")


# Analysis ----------------------------------------------------------------




# Making the Map ----------------------------------------------------------



mean_data <-  bot_temp_CPUE %>% 
  group_by(grid) %>% 
  summarise(mean_temperature = mean(temp, na.rm = TRUE),
            mean_CPUE = mean(CPUE, na.rm = TRUE)) %>% 
  na.omit() %>% #discards all missing data
  arrange(-mean_temperature)

grid_polys_extra <- grid_polys %>% 
  left_join(mean_data, by = "grid")

-----------------------------------------


# Two maps on one figure --------------------------------------------------


require(gridExtra)

plot1 <- ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "green", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = mean_temperature), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D")

plot2 <- ggplot(data = grid_polys_extra, aes(x = lon, y= lat, group = grid)) +
  borders(fill = "green", colour = "black") +  #colour is line colour and fill is the fill colour in the lines
  geom_polygon(aes(fill = mean_CPUE), colour = "black") +
  coord_equal(xlim = c(-58, -68), ylim = c(42, 48)) +
  scale_fill_viridis_c(option = "D")

grid.arrange(plot1, plot2, nrow=2, ncol=1)

plot1

plot2



# Homework ----------------------------------------------------------------

#RECREATE THIS MAP SHOING MEAN CPUE INSTEAD OF MEAN TEMP
#MAKE A FIGURE WHERE BOTH MAPS ARE ON THE SAME FIGURE
