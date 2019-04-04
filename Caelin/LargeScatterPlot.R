
library(tidyverse)
library(heatwaveR)
library(lubridate)
library(gridExtra)
source("shared/statistics.R")


# Preparing the Data ------------------------------------------------------
#Short cut way of adding a new colomb

north_MHW_res$site <- "North"

south_MHW_res$site <- "South"

fundy_MHW_res$site <- "Fundy"


all_MHW_res <- rbind(north_MHW_res, south_MHW_res, fundy_MHW_res)
all_MHW_res$site <- factor(all_MHW_res$site, levels = c("North", "South", "Fundy"))

All_Duration <- ggplot(data = all_MHW_res, aes(x = duration, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 20, y = 1.2)) +
  labs(x = "Duration (days)", y = "Mean CPUE") +
  facet_wrap(~site, scales = "free_x")

All_Duration


All_intensity <- ggplot(data = all_MHW_res, aes(x = intensity_mean_relThresh, y = mean_CPUE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_label(aes(label = NA, x = 1, y = 1.2)) +
  labs(x = "Mean Intensity (relative to 90th percentile)", y = "Mean CPUE") +
  facet_wrap(~site, scales = "free_x")

All_intensity

grid.arrange(All_Duration, All_intensity)







# Custom Lolli Plots ------------------------------------------------------

all_MHW_res_lolli <- rbind(north_MHW$event, south_MHW$event, fundy_MHW$event)
all_MHW_res_lolli$site <- c(rep("North", nrow(north_MHW$event)), 
                            rep("South", nrow(south_MHW$event)),
                            rep("Fundy", nrow(fundy_MHW$event)))  
  
all_MHW_res_lolli$site <- factor(all_MHW_res_lolli$site, levels = c("North", "South", "Fundy"))

ggplot(data = all_MHW_res_lolli, aes(x = date_peak, y = intensity_max)) + 
  geom_lolli(colour = "salmon", colour_n = "red", 
             fill = "grey70", n = 3) +
  labs(x = "Peak Date", y = "Max. Intensity [Deg. C]") + 
  facet_wrap(~site, ncol = 1) +
  theme_light()
  # scale_y_continuous(expand = c(0, 0), limits = y_limits) + 
  # theme(plot.background = element_blank(), panel.background = element_rect(fill = "white"), 
  #       panel.border = element_rect(colour = "black", fill = NA, 
  #                                   size = 0.75), panel.grid.minor = element_line(colour = NA), 
  #       panel.grid.major = element_line(colour = "black", 
  #                                       size = 0.2, linetype = "dotted"), axis.text = element_text(colour = "black"), 
  #       axis.text.x = element_text(margin = unit(c(0.5, 0.5, 
  #                                                  0.5, 0.5), "cm")), axis.text.y = element_text(margin = unit(c(0.5, 
  #                                                                                                                0.5, 0.5, 0.5), "cm")), axis.ticks.length = unit(-0.25, 
                                                                                                                                                                  # "cm"))








