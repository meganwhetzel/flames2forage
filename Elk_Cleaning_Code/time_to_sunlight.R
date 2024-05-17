# Calculating time from dawn/dusk

# Load packages ####

library(lubridate)
library(tidyverse)

# Create a range of date-times ####

d <- seq(from = ymd_hms("2022-01-01 00:00:00"),
         to = ymd_hms("2022-01-03 00:00:00"),
         by = 60) 

# Add a made-up time for dawn and dusk ####

test <- data.frame(date = d) %>% 
  mutate(dawn = ymd_hms(paste0(as_date(date), "06:00:00")),
         dusk = ymd_hms(paste0(as_date(date), "18:00:00")))

# Calculate time from dawn/dusk ####

test <- test %>% 
  mutate(time_to_dawn = difftime(date, dawn, tz = "MST", units = "hours"),
         time_to_dusk = difftime(date, dusk, tz = "MST", units = "hours")) %>% 
  mutate(time_to_sunlight = case_when(
    # If we are farther away in time from dusk than from dawn...
           abs(difftime(date, dusk, tz = "MST", units = "hours")) >
             abs(difftime(date, dawn, tz = "MST", units = "hours")) ~
             # ...then assign time from dawn 
             # (will be negative at nighttime and positive in the daytime)
             difftime(date, dawn, tz = "MST", units = "hours"),
           # If we are closer in time to dusk than to dawn...
           abs(difftime(date, dusk, tz = "MST", units = "hours")) <
             abs(difftime(date, dawn, tz = "MST", units = "hours")) ~
             # ...then assign time from dusk 
             # (will be negative at nighttime and positive in the daytime)
             -difftime(date, dusk, tz = "MST", units = "hours"),
           # If we are at an equal distance in time from dawn or dusk
           # (which means it's the middle of the day)...
           abs(difftime(date, dusk, tz = "MST", units = "hours")) ==
             abs(difftime(date, dawn, tz = "MST", units = "hours")) ~ 
             # ...then assign the absolute value of either 
             # (we want it to be positive)
             abs(difftime(date, dusk, tz = "MST", units = "hours"))
           )) %>% 
  mutate(time_to_sunlight = as.numeric(time_to_sunlight))

# Plot ####

ggplot(test, aes(x = date, y = time_to_sunlight)) +
  geom_line() +
  # Add vertical line to mark dawn in red
  geom_vline(aes(xintercept = dawn), color = "red") +
  # Add vertical line to mark dusk in blue
  geom_vline(aes(xintercept = dusk), color = "blue") +
  theme_bw()
