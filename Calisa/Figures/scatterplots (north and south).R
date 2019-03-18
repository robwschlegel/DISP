#Calisa Staniforth


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(cowplot)


# Data --------------------------------------------------------------------

source("shared/statistics.R")
source("shared/load.R")


#scatterplots for north and south

#North
ggplot(data = north_MHW_res, aes(x = mean_CPUE, y = duration)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ",round(north_cor[1,2], 2)), x = 1.2, y = 20))

#South
ggplot(data = south_MHW_res, aes(x = mean_CPUE, y = duration)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(aes(label = paste0("r = ",round(south_cor[1,2], 2)), x = 2, y = 45))
