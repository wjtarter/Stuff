library(tidyverse)
library(usmap)

states <- map_data("state")
abbre <- usmapdata::centroid_labels("states")

state_names <- left_join(states, 
                         abbre %>% mutate(full = tolower(full)), 
                         by = c("region" = "full"))

