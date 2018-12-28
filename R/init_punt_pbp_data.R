# This file generates the nflscrapR punt data from 2009 to 2017

# Access tidyverse
# install.packages("tidyverse")
library(tidyverse)

# Join together the regular season play-by-play data including a column to 
# denote the season:

pbp_data <- map_dfr(c(2009:2017),
                    function(x) {
                      read_csv(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_",
                                      x, ".csv")) %>%
                        mutate(pbp_season = x) %>%
                        select(-fumble_recovery_2_yards)
                    })

# Filter to only include punt attempts (for now including penalties as well)
punt_pbp_data <- pbp_data %>%
  filter(punt_attempt == 1) %>%
  # Create a column that takes the various indicators about punts and denotes
  # what type of punt return occurred:
  mutate(punt_return_type = case_when(
    str_detect(tolower(desc), "touchback") ~ "touchback",
    punt_downed == 1 ~ "downed",
    punt_fair_catch == 1 ~ "fair_catch",
    punt_out_of_bounds == 1 ~ "out_of_bounds",
    punt_blocked == 1 ~ "blocked",
    TRUE ~ "return"
  ))

# Save this file to the data folder:
write_csv(punt_pbp_data, "data/punt_pbp_data.csv")
