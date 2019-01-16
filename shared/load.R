# The purpose of this script is to load the lobster CPUE and temperature data
# as well as the grid cell coordinates.


# Libraries ---------------------------------------------------------------

library(tidyverse)


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

# The bottom temperature and CPUE data
suppressMessages(
  bot_temp_CPUE <- read_csv("data/bot_temp_CPUE.csv")
)

