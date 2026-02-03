###################################################
######### Average phenology dates 2023 ############
#########      for Bracelets       ################
#########   By Bailey Bingham      ################                        
#########   February 3rd, 2026   ##################
###################################################

library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(lubridate)
library(zoo)
library(janitor)

### IMPORT 2023 phenology data ###
phen <- read_csv("phenology_2023.csv") %>% 
  clean_names() %>% # Automatically fixes 90_snowfree, leaf-out, etc.
  select(phenocam, year, site, first_plants_vis, x90_snowfree, frst_100_snowfree,
         frst_snow_return, x50_snow_cover, x100_snow_cover, leaf_out,
         x50_green_leaves, x100_green_leaves, senes_begins, x50_yellow,
         x100_yellow)

phen_clean <- phen %>%
  # Convert all columns to Dates
  mutate(across(where(is.character) & !c(phenocam, site), dmy)) %>%
  
  # Calculate the average for each date column
  summarize(across(where(is.Date), ~ mean(.x, na.rm = TRUE)))

print(phen_clean)
