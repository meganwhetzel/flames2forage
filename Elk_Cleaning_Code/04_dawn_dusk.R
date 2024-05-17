# Load packages ####
install.packages("clock")
install.packages("spatialEco")
library(tidyverse)
library(dplyr)
library(sp)
library(maptools)
library(sf)
library(lubridate)
library(clock)
library(spatialEco)
library(amt)

# Load in RDS of clean data ####
clean_flagged_data_30 <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/clean_flagged_data_30.rds")
# on mac 
clean_flagged_data_30 <- readRDS("~/Library/CloudStorage/Box-Box/MeganResearch/MSc work/Elk_Cleaning_new/elk_gps_cleaning/clean_flagged_data_30.rds") %>%
  as.data.frame()


# Dawn, dusk for entire dataset ####

# Select just the latitude and longitude columns from the data
clean_test <- clean_flagged_data_30 %>%
  select(lat_y, lon_x) %>%
  as.data.frame() %>% 
  relocate(lon_x, .before = "lat_y")

# Pull just datetime_UTC column and convert into a data frame
clean_times <- clean_flagged_data_30 %>%
  select(datetime_UTC) %>%
  mutate(datetime_UTC = as.character(datetime_UTC))

date_time <- as.POSIXct(clean_times$datetime_UTC, tz = "MST")

# For entire test dataset ####
sp_points_all <- SpatialPoints(clean_test, proj4string = CRS("+proj=longlat +datum=WGS84"))

data_all_dawn <- crepuscule(sp_points_all, date_time, solarDep = 6, direction = "dawn", POSIXct.out = TRUE) %>%
  rename(day_frac_dawn = day_frac,
         dawn_time = time)
data_all_dusk <- crepuscule(sp_points_all, date_time, solarDep = 6, direction = "dusk", POSIXct.out = TRUE) %>%
  rename(day_frac_dusk = day_frac,
         dusk_time = time)

# Bind dawn, dusk columns to clean dataset ####
complete <- clean_flagged_data_30 %>%
  cbind(data_all_dawn, data_all_dusk)


# Time until dawn, dusk & time since dawn, dusk on whole dataset ####
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

# ################ USING HMM DATA ############
# 
# # Subset 1 elk from dataset as a test ####
# 
# test <- HMM_ready_elk_data_20220926_hierarchical %>% 
#     filter(is.na(x) == FALSE)
# 
# # Transform utm to lat, long
# install.packages("terra")
# library(terra)
# 
# # # Select just the latitude and longitude columns from the data ####
# # clean_test <- test %>%
# #   select(y, x) %>%
# #   as.data.frame()
# # #  relocate(x, .before = "lat_y")
# 
# # Pull just datetime_UTC column and convert into a data frame ####
# clean_times <- test %>%
#   select(dt) %>%
#   mutate(dt = as.character(dt))
# 
# date_time <- as.POSIXct(clean_times$dt, tz = "MST")
# 
# # For test dataset ####
# 
# sp_points_all <- SpatialPoints(clean_test, proj4string = CRS("+proj=utm +zone=12 +datum=WGS84"))
# sp_points_1 <- spTransform(sp_points_all, CRS("+proj=longlat +datum=WGS84"))
# lnlt <- coordinates(sp_points_1)
# 
# 
# data_all_dawn <- crepuscule(lnlt, date_time, solarDep = 6, direction = "dawn", POSIXct.out = TRUE) %>%
#   rename(day_frac_dawn = day_frac,
#          dawn_time = time)
# data_all_dusk <- crepuscule(lnlt, date_time, solarDep = 6, direction = "dusk", POSIXct.out = TRUE) %>%
#   rename(day_frac_dusk = day_frac,
#          dusk_time = time)
# 
# test_complete <- test %>%
#   cbind(data_all_dawn, data_all_dusk)
# 
# setwd("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data")
# 
# saveRDS(test_complete, "HMM_ready_crepsuscule.rds")
# 
# 
# # Dawn, dusk for entire dataset ####
# 
# # Select just the latitude and longitude columns from the data
# clean_test <- clean_flagged_data_30 %>%
#   select(lat_y, lon_x) %>%
#   as.data.frame() %>% 
#   relocate(lon_x, .before = "lat_y")
# 
# # Pull just datetime_UTC column and convert into a data frame
# clean_times <- clean_flagged_data_30 %>%
#   select(datetime_UTC) %>%
#   mutate(datetime_UTC = as.character(datetime_UTC))
# 
# date_time <- as.POSIXct(clean_times$datetime_UTC, tz = "MST")

## TEST CODE ###
# # Subset 1 elk from dataset as a test ####
# test <- subset(clean_flagged_data_30, animal_id == "EL19F0060")
# 
# # Select just the latitude and longitude columns from the data ####
# clean_test <- test %>%
#   select(lat_y, lon_x) %>%
#   as.data.frame() %>% 
#   relocate(lon_x, .before = "lat_y")
# 
# # Pull just datetime_UTC column and convert into a data frame ####
# clean_times <- test %>%
#   select(datetime_UTC) %>%
#   mutate(datetime_UTC = as.character(datetime_UTC))
# 
# date_time <- as.POSIXct(clean_times$datetime_UTC, tz = "MST")
# 
# # For test dataset ####
# sp_points_all <- SpatialPoints(clean_test, proj4string = CRS("+proj=longlat +datum=WGS84"))
# 
# data_all_dawn <- crepuscule(sp_points_all, date_time, solarDep = 6, direction = "dawn", POSIXct.out = TRUE) %>%
#   rename(day_frac_dawn = day_frac,
#          dawn_time = time)
# data_all_dusk <- crepuscule(sp_points_all, date_time, solarDep = 6, direction = "dusk", POSIXct.out = TRUE) %>%
#   rename(day_frac_dusk = day_frac,
#          dusk_time = time)
# 
# test_complete <- test %>%
#   cbind(data_all_dawn, data_all_dusk)
