## Need to:
# 1) remove the flagged points
# 1.5) Round time stamps and remove those duplicates - use dop?
# 2) add NDVI pixels for each point - how green is it? (0 for snow)
# 3) add ruggedness


# Code to regularize tracks ####
# Simona Picardi
# December 2021

# Load packages ####

library(tidyverse)
library(lubridate)

# Load data ####

# This is a snippet of example data
dat <- readRDS("test_data_simona.rds")
dat <- test_data_simona

# Take a look
dat

# Round timestamps ####

# We expect data every hour. Sometime locations were taken not exactly on the
# hour (e.g., first row has timestamp 06:58:00). We want to round these so they
# are exactly on the hour.

dat <- dat %>% 
  # Only round locations that are close enough to the hour
  filter(minute(timestamp) %in% c(0:3, 57:59)) %>% 
  # Retain the original timestamp
  mutate(timestamp_original = timestamp) %>% 
  mutate(timestamp = round(timestamp, units = "hours")) 

# Any locations that were way too far off the hour?
dat %>% 
  filter(minute(timestamp) != 0)
# Nope, all good.

# Did we introduce any duplicates by rounding the timestamps?
# E.g., if a location was taken at 06:58 and another at 07:00,
# after rounding we'd have two 07:00 locations. If this ever
# happens, remove duplicate.
dat <- dat[!duplicated(paste(dat$id, dat$timestamp)), ]

# Regularize tracks ####

# We expect locations every hour, but we didn't always get them.
# For example, the first location is at 7 AM and the second one at 8 PM.
# There should have been locations between 8 AM and 7 PM as well.
# Insert those timestamps (adding NA for the coordinates) so that the 
# track is at regular time intervals.

# Expected time ranges: between first and last location of each individual
ranges <- dat %>% 
  group_by(id) %>% 
  summarize(start = ymd_hms(min(timestamp)),
            end = ymd_hms(max(timestamp)))

# Create empty data frame to store output
out <- data.frame(id = NA,
                  timestamp = ymd_hms(NA),
                  x = NA,
                  y = NA,
                  timestamp_original = ymd_hms(NA))

for (i in 1:length(unique(dat$id))) {
  
  # Current individual
  who <- unique(dat$id)[i]
  print(who)
  
  # Create vector of expected timestamps
  ts <- seq(ranges[i,]$start, ranges[i, ]$end, by = 3600)
  
  # Get data for that individual
  coord <- dat %>% 
    filter(id == who) 
  
  # Create data frame with id and expected timestamps
  # Left join data: this will add coordinates for timestamps that are 
  # present in the data, and timestamps that were not present in the data
  # will be left with NA coordinates
  res <- data.frame(id = who,
                    timestamp = ts) %>% 
    left_join(coord, by = c("id", "timestamp"))
  
  out <- rbind(out, res)
  
}

# First row is NA and was introduced by us, remove it
out <- out[-1, ]  

# Check output
View(out)
nrow(dat)
nrow(out)
# We introduced 13 rows for fixes that were expected but not taken