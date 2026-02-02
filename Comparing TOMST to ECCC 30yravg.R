#######################################################
########## Comparing TOMST to ECCC Data ##############
##########    By Bailey Bingham            ############
######################################################

library(tidyverse) #includes ggplot, tidyr, dplyr, etc. 
library(lubridate)
library(zoo)

### IMPORT AGGREGATED DAILY TOMST DATA ###
eccc<-read.csv("~/1. PhD Research/GitHub/Growth-chambers/export_data/ECCC_dailymeantemp_1996-2025.csv") %>%
  drop_na()%>%
  # Read datetime as a date
  mutate(dummydate = ymd(dummydate)) %>% 
  # change column headers
  select(dummydate,Mean_Temp, Max_Temp, Min_Temp)  %>%
  rename(
    eccc_mean_temp = Mean_Temp,
    eccc_max_temp = Max_Temp,
    eccc_min_temp = Min_Temp
  )

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

### Calculate rolling average
rollavg <-tomst_daily_avg %>%
  mutate(
    # Rolling Average: Smoothes data, but lowers peaks
    rolling_avg = rollmean(QHI_mean, k = 6, fill = NA, align = "center"),
    # Rolling Max: Preserves the heatwave intensity for the week
    rolling_max = rollmax(QHI_max, k = 6, fill = NA, align = "center"),
    # Rolling Min: Captures the coolest dip of the week
    rolling_min = rollapply(QHI_min, width = 6, FUN = min, fill = NA, align = "center"))

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

#### Combine TOMST with ECCC to compare 2023 to the 30 year average
cdata<-cbind(eccc, rollavg)

# graph it
ggplot(cdata, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  #ECCC Ribbon (Grey Background)
  geom_ribbon(aes(ymin = eccc_min_temp, ymax = eccc_max_temp),fill = "grey", alpha = 0.3) +
  #ECCC Mean Line
  geom_line(aes(y = eccc_mean_temp), color = "grey20", size = 0.8) +
  #QHI Ribbon 
  geom_ribbon(aes(ymin = QHI_min, ymax = QHI_max), fill = "darkred", alpha = 0.3) +
  #QHI Mean Line
  geom_line(aes(y = QHI_mean), color = "darkred", size = 1) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "QHI ECCC weather station historical temps compared to 2023 TOMST average, max and min temps",
       subtitle = "Grey: ECCC (30-yr) | Red: QHI TOMST (2023)",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()


# what about with rolling 2023 averages?
ggplot(cdata, aes(x = dummydate)) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  #ECCC Ribbon (Grey Background)
  geom_ribbon(aes(ymin = eccc_min_temp, ymax = eccc_max_temp),fill = "grey", alpha = 0.3) +
  #ECCC Mean Line
  geom_line(aes(y = eccc_mean_temp), color = "grey20", size = 0.8) +
  #QHI actual extremes Ribbon 
  geom_ribbon(aes(ymin = QHI_min, ymax = QHI_max), fill = "darkred", alpha = 0.3) +
  #QHI Mean rolling average Line
  geom_line(aes(y = rolling_avg), color = "darkred", size = 1) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  labs(title = "QHI ECCC weather station historical temps compared to 2023 TOMST rolling average and max and min temps",
       subtitle = "Grey: ECCC (30-yr) | Red: rolling average and actual max and mins of the TOMST (2023)",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()

### Should consider comparing the TOMST 2023 data to the ECCC 2023 data if it exists?

