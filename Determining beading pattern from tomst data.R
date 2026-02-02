###################################################
######### Determining Temperature #################
#########   Bins for Bracelets    #################
#########   By Bailey Bingham #####################                        
#########   February 1st, 2026 ####################
###################################################

# The data needs to be split into 52 unique units which will be represented by 
# 52 beads (i.e. if using size 8/0 (3mm)beads, a wrist length of 6.5"). 

# The temps should be binned into about 8-10 bins, so that there are no more 
# than that many colours to bead. 

####################### Download packages  ####################################
library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(lubridate)
library(zoo)

### IMPORT AGGREGATED DAILY TOMST DATA ###
tomst<-read.csv("~/1. PhD Research/GitHub/TOMST-QHI/export_data/2025_TOMSTdata_preprocessed_daily.csv") %>%
    # Read datetime as a date
  mutate(datetime = ymd(datetime))%>%
    #seperate QHI as the location, and the TOMST ID out into seperate columns
  separate_wider_regex( #using regex so that we can account for there being two underscores in locality_id
    locality_id,
    patterns = c(
      id = ".*",      # Greedily matches everything until...
      "_",      # ...the last underscore (discarded)
      location = ".*") # Everything after that last underscore
  ) %>%
    # remove TMS for the sensors that include it
  mutate(sensor = str_remove(sensor_name, "^TMS_"), .keep = "unused") %>%
    # remove all sensors other than the air temperature data (T3)
  filter(sensor %in% c("T3_mean", "T3_min", "T3_max")) %>%
    # filter to only include 2023 data
  filter(year %in% ("2023")) %>%
    #Rearrange the headings 
  select(location, id, sensor, datetime, 
         year, month, week, day, doy, value)  %>%
    #pivot to wide format
  pivot_wider(
    # define id columns
    id_cols = c(location, id, datetime, year,
                month, week, day, doy,), 
    # use values in 'sensor' for new headers
    names_from = sensor, 
    # fill measurement columns with values from the 'value' column
    values_from = value)

### Graph all 2023 TOMSTS onto one graph, to get an idea of the range
ggplot(tomst, aes(x = datetime, y = T3_mean, color = id)) +
  geom_line(alpha = 0.5) + 
  theme_minimal() +
  facet_wrap(~id)+
  labs(title = "Time Series by Sensor ID",
       x = "Date",
       y = "Mean Temperature (T3)") 

### TOMST_21 and _22 look kinda weird, but they are right near each other on 
### the island so the data is probably accurate.
  
### Average all stations to get one daily average for the whole island ###
tomst_daily_avg <- tomst %>%
  group_by(datetime) %>%
  summarise(
  # Average of the mean temperatures across all stations
  QHI_mean = mean(T3_mean, na.rm = TRUE),
  # The absolute lowest temperature recorded by ANY station that week
  QHI_min  = min(T3_min, na.rm = TRUE),
  # The absolute highest temperature recorded by ANY station that week
  QHI_max  = max(T3_max, na.rm = TRUE))

### Graph the average to see if it makes sense
ggplot(tomst_daily_avg, aes(x = datetime, y = QHI_mean)) +
  geom_line(alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_ribbon(aes(ymin = QHI_min, ymax = QHI_max), alpha = 0.2, fill = "grey") +
  scale_x_date(
    date_breaks = "1 month",   # Set marks at every month
    date_labels = "%b %Y"      # Format: %b = Abbr Month, %y = year (e.g., Jan 22)
  ) +
  theme_minimal() +
  labs(title = "QHI average temperature 2023",
       subtitle = "Grey shaded area shows the extreme temperatures for each day",
       x = "Date",
       y = "Mean Daily Air Temperature") 



rollavg <-tomst_daily_avg %>%
   mutate(
    # Rolling Average: Smoothes data, but lowers peaks
    rolling_avg = rollmean(QHI_mean, k = 6, fill = NA, align = "center"),
    # Rolling Max: Preserves the heatwave intensity for the week
    rolling_max = rollmax(QHI_max, k = 6, fill = NA, align = "center"),
    # Rolling Min: Captures the coolest dip of the week
    rolling_min = rollapply(QHI_min, width = 6, FUN = min, fill = NA, align = "center"))  %>%
  # Select 52 points (one for each bead)
  slice(seq(1, n(), by = 7)) %>%
  drop_na()

rollavg %>%
  pivot_longer(cols = c(rolling_avg, rolling_max, rolling_min), 
               names_to = "Method", values_to = "Temp") %>%
  ggplot(aes(x = datetime, y = Temp, color = Method)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Bead Pattern Comparison: Avg vs Max",
       subtitle = "Rolling Max preserves heatwave peaks for better visualization",
       y = "Degrees (C)") +
  theme_minimal()

