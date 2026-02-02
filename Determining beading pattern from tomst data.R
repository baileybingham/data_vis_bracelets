###################################################
######### Determining Temperature #################
#########   Bins for Bracelets    #################
#########   By Bailey Bingham #####################                        
#########   February 1st, 2026 ####################
###################################################

# The data needs to be split into at minimum 52 unique units which will be 
# represented by 52 beads (i.e. if using size 8/0 (3mm)beads, a wrist length 
# of 6.5"). 

# If we bin the year by every 3 days, then we would need 121 beads to cover
# the full year, or 61 to cover 6 months. 
# So the smallest bracelet size would cover 156 days (6.5"; May 1st-October 3rd), and the largest
# would cover 228 days (9.5"; March 26-November 8).


# The temps should be binned into about 8-10 bins, so that there are no more 
# than that many colours to bead. 


####################### Download packages  ####################################
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
    rolling_avg = rollmean(QHI_mean, k = 3, fill = NA, align = "center"),
    # Rolling Max: Preserves the heatwave intensity for the week
    rolling_max = rollmax(QHI_max, k = 3, fill = NA, align = "center"),
    # Rolling Min: Captures the coolest dip of the week
    rolling_min = rollapply(QHI_min, width = 7, FUN = min, fill = NA, align = "center"))%>%
  drop_na()

### Graph the visualize rolling average
ggplot(rollavg, aes(x = datetime, y = rolling_avg)) +
  geom_line(alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_ribbon(aes(ymin = rolling_min, ymax = rolling_max), alpha = 0.2, fill = "grey") +
  scale_x_date(
    date_breaks = "1 month",   # Set marks at every month
    date_labels = "%b %Y"      # Format: %b = Abbr Month, %y = year (e.g., Jan 22)
  ) +
  theme_minimal() +
  labs(title = "QHI average temperature 2023",
       subtitle = "Grey shaded area shows the extreme temperatures for each day",
       x = "Date",
       y = "Mean Daily Air Temperature") 

  # Bin the year into 3 day sections by rolling average
rollbinned<- rollavg %>%
  slice(seq(1, n(), by = 3)) %>%
  drop_na()

cropbinned <- rollbinned %>%
  filter(datetime >= "2023-03-26" & datetime <= "2023-11-08")

range(cropbinned$rolling_avg, na.rm = TRUE)

# Define your temperature range
min_temp <- -16
max_temp <- 18

# Create 12 bins
cropbinned$temp_bins <- cut(cropbinned$rolling_avg, 
                    breaks = seq(min_temp, max_temp, length.out = 13), 
                    include.lowest = TRUE)
unique(cropbinned$temp_bins) # view the bins

# Create a 15-color palette from "RdBu" (Red to Blue)
my_colors <- colorRampPalette(c("darkblue","blue", "white","red", "darkred"))(12)

ggplot(cropbinned, aes(x = datetime, y = 1, fill = temp_bins)) +
  geom_tile(color = "white", linewidth = 0.1)  +
  scale_fill_manual(values = my_colors)+ 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "2023 growing season", subtitle = "Rolling averages binned every 3 days.", x = "Date", fill = "Temp Range") +
  theme_minimal()  +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.text.x  = element_text(angle = 70, vjust = 0.5, size = 6)
  )


#################################################################################
#### Let's try it without the rolling average ####

# Bin the year into 3 day sections by rolling average
bin<- tomst_daily_avg %>%
  slice(seq(1, n(), by = 3)) %>%
  drop_na()

crop <- bin %>%
  filter(datetime >= "2023-03-26" & datetime <= "2023-11-08")

range(crop$QHI_mean, na.rm = TRUE)

# Define your temperature range
min_temp <- -16
max_temp <- 19

# Create 10 bins
crop$temp_bins <- cut(crop$QHI_mean, 
                            breaks = seq(min_temp, max_temp, length.out = 11), 
                            include.lowest = TRUE)
unique(crop$temp_bins) # view the bins

# Create a 10-color palette from "RdBu" (Red to Blue)
my_colors <- colorRampPalette(c("darkblue","blue", "white","red", "darkred"))(10)

# Create letter labels
num_bins <- length(levels(crop$temp_bins))
# change legend labels
raw_levels <- levels(crop$temp_bins)
# Use regex to remove brackets [ ] and parentheses ( )
# We then replace the comma with "°C to " and add "°C" to the end
clean_levels <- gsub("[^-0-9.]+", " ", raw_levels)
clean_levels <- trimws(clean_levels)
clean_levels <- gsub(" ", "°C to ", clean_levels)
clean_levels <- paste0(clean_levels, "°C")
# Combine with the LETTERS for your final legend text
letter_labels <- paste0(LETTERS[1:num_bins], ": ", clean_levels)
crop$bin_letter <- LETTERS[as.numeric(crop$temp_bins)]
text_colours <- c("white", "white", "white", "white", "black", 
                  "black", "black", "black", "black", "white")



ggplot(crop, aes(x = datetime, y = 1, fill = temp_bins)) +
  geom_tile(color = "white", linewidth = 0.1)  +
  geom_text(aes(y = 1.45,label = bin_letter, color = temp_bins), 
            size = 3, fontface = "bold") +
  # Apply the conditional text colours
  scale_color_manual(values = text_colours, guide = "none") + 
  scale_fill_manual(values = my_colors, labels = letter_labels)+ 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "2023 Growing Season on Qikiqtaruk - Herschel Island",
       subtitle = "Daily average temperature ranges over 3 day periods on QHI.",
       x = "Date", 
       fill = "Bead colour and Temp range (°C)") +
  theme_minimal()  +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.text.x  = element_text(angle = 70, vjust = 0.5, size = 8),
    legend.text = element_text(size = 8)
  )

### I like it better without the rolling averages, so let's go with this one!

# Create a purchasing list
bead_shopping_list <- crop %>%
  group_by(temp_bins) %>%
  summarise(beads_per_bracelet = n()) %>%
  mutate(
    letter = LETTERS[1:n()],
    # Multiply the daily count by 100
    total_beads_needed = beads_per_bracelet * 100,
    temp_range = clean_levels # Uses "-16°C to -12.5°C" format
  ) %>%
  select(letter, beads_per_bracelet,total_beads_needed, temp_range)

print(bead_shopping_list)
