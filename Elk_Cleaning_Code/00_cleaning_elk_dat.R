# Cleaning elk data with amt functions
# 06/06/2022
# Last updated with mort data -- 09/02/22

# Clean r env
rm(list = ls())
gc()

library(installr)
#install.Rtools()

#install.packages("remotes")
library(remotes)
remotes::install_github("jmsigner/amt", ref = "data-cleaning", build = F)
install.packages("amt")
install.packages("sp")

# animalIDs
#id <- animalID[round(runif(1, 1, length(animalIDs)))]

# Libraries -----
library(amt)
library(dplyr)
library(lubridate)
library(raster)
library(tidyverse)
library(data.table)
library(sp)

# most recent function - bug fixes
remotes::install_github("jmsigner/amt", ref = "data-cleaning")


# Load in data ----
elk_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/20220829_elk_data_BigQuery.rds")

# mac

elk_dat <- readRDS("~/Library/CloudStorage/Box-Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/20220216_elk_data_BigQuery.rds")

# Set up inputs ---
# EPSG
utm <- 32612

# alpha
alpha <- lubridate::hours(24) %>% 
  as.numeric() %>% 
  lubridate::period()

# delta
speed <- 72
time <- lubridate::minutes(10)
# calculate delta w/ amt function
delta <- calculate_sdr(speed, time)

# zeta, eta, theta designations
epsilon <- 20
zeta <- 5
eta <-  5
theta <- lubridate::days(1)

# Select desired fields ----
elk <- elk_dat %>%
  mutate(DT_UTC = as.POSIXct(DT_UTC, 
                                format= "%Y-%m-%d %H:%M:%S", 
                                   tz = "MST")) %>%
  mutate(row_id = as.character(seq(1:nrow(elk_dat))),
         # changing NA's to 1 in 'dop' column as per amt protocol
         Dop = ifelse(is.na(Dop), 1, Dop)) %>%
  rename(collar_id = CollarID,
         animal_id = AnimalID,
         lat_y = Latitude,
         lon_x = Longitude,
         datetime_UTC = DT_UTC,
         dop = Dop) %>% 
  dplyr::select(-c(OBJECTID, Species, Freq, CurrentCohort, ProjectName, 
                   CaptureUnit, CaptureSubunit, CaptureArea, BirthYear, DT_MST,
                   DateAndTime)) %>%   
 # relocate(row_id, .before = "collar_id") %>% 
  # arrange by timestamp
  arrange(datetime_UTC)

# Removing individuals that have too few points
few_points <- elk %>% 
  group_by(animal_id) %>% 
  tally() %>% 
  filter(n < 30)

dat <- elk %>% 
  filter(!animal_id %in% few_points$animal_id)


# Set up loop ----
# filter df for testing
# Change data to 'test_elk' in loop when you want to run subset
# test_elk <- elk %>% 
#   filter(year(datetime_UTC) %in% c(2011:2013))

# grab id's
ids <- unique(dat$animal_id)

# set up empty data frame
clean <- data.frame()

for(i in 1:length(ids)){
  # print status
  print(paste(i, ids[i]))
  
  # run  functions in temporary df
  temp <- elk %>% 
    # filter by individuals
    filter(animal_id == ids[i]) %>% 
    
    # turn into track_xyt obj
    mk_track(.x = lon_x, .y = lat_y, .t = datetime_UTC,
             all_cols = TRUE,
             crs = 4326) %>% 
    # project into utm
    transform_coords(crs_to = utm) %>% 
    
    ### Remove capture effect ----
  
  remove_capture_effect(start = alpha) %>%
    
    ## Remove low quality duplicates ----
    flag_duplicates(gamma = minutes(10)) %>% 
     filter(!duplicate_) %>% 
    
    ## Remove fast steps ----
    flag_fast_steps(delta = delta) %>% 
  #   filter(!fast_step_) %>% 
    
    ## Remove fast roundtrips ----
  # if this doesn't 'look right', play around w/ epsilon
    # ex: 5, 10 
  flag_roundtrips(delta = delta, epsilon = 10) %>% 
  #   filter(!fast_roundtrip_) %>% 
    
    ## Remove mortality/drop-off clusters ----
  flag_defunct_clusters(zeta = zeta, eta = eta, theta = theta) # %>% 
 #  filter(!defunct_cluster_)
  
  # combine temporary and clean data frame
  clean <- rbind(temp, clean)
  
  # End loop
}

# Checks -----
table(clean$fast_roundtrip_)
table(clean$fast_step_)
table(clean$defunct_cluster_)
table(clean$duplicate_)

# Rename and reorganize
# personal preferences here!!!
dat_clean <- clean %>%
  # filter out TRUE for clean dataset
  filter(!defunct_cluster_) %>%
  filter(!fast_step_) %>% 
  filter(!duplicate_) %>% 
  filter(!fast_roundtrip_) %>% 
  # rename columns
  rename(ID = animal_id,
        x = x_,
         y = y_,
         dt = t_)  %>%
  # move ID column to beginning
  relocate(ID, .before = x) %>% 
  relocate(row_id, .before = ID) %>% 
# remove unnecessary columns
  dplyr::select(-defunct_cluster_,
         -duplicate_, 
         -fast_step_,
         -fast_roundtrip_)

# Remove Zombie Points (i.e. points that occur after the animal's death date)

mort_dates <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Exported_CSVs/mort_table.csv") %>%
  filter(Species == "elk" & MortalityCause != "Collar Swapped") %>%
  dplyr::select(AnimalID, UTC) %>%
  mutate(UTC = gsub("T", " ", UTC)) %>%
  rename(ID = "AnimalID",
         death_date = "UTC") %>%
  mutate(death_date = as.POSIXct(death_date,
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz = "MST"))

dat_clean2 <- dat_clean %>%
  left_join(mort_dates, by = "ID") %>%
  mutate(zombie_point = ifelse(!is.na(death_date) & dt > death_date,
                               TRUE,
                               FALSE)) %>%
  filter(zombie_point == FALSE)

# Check rows
nrow(dat)
# [1] 5330742
nrow(clean)
# [1] 5068611
nrow(dat_clean)
# [1] 5066617
nrow(dat_clean2)
# [1] 5006792
# How many indivs left?
x <- unique(dat_clean2$ID)
# 1272



# Save output file -----
getwd()
setwd("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data")

saveRDS(dat_clean, "20220908_clean.rds")

saveRDS(dat_clean2, '20220908_clean_zomb.rds')

write.csv(dat_clean2, "20220908_clean_01.csv", row.names = FALSE)
# DONE!



# Checking elk number discrepancies between Courtney data and my data ----
aug_data <- `20220831_clean`

y <- data.frame(id = unique(aug_data$animal_id))

z <- data.frame(id = unique(raw_elk_21$animal_id))

missing <- y %>% 
  filter(!id %in% z$id)


# 09/19/2022 - DAYMET and NDVI Aqua were last updated in December 30, 2021
# Below code is to filter elk points before this date to run in GEE
library(tidyverse)
copy <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/20220908_clean_zomb.rds")

GEE_ready_test <- copy %>%
  filter(dt < "2021-12-30 11:59:59") # %>% 
#  filter(is.na(dt) == TRUE)


class(GEE_ready_20220919$dt)

saveRDS(GEE_ready_20220919, "GEE_ready.rds")

write.csv(GEE_ready_20220919, "GEE_ready_20220919.csv", row.names = FALSE)


# If you want to look at mortality ----
# # Look at mortality drop-offs
# mort <- clean %>% 
#   filter(defunct_cluster_ == TRUE) 
# # grab ids
# mort_ids <- unique(mort$animal_id)
# 
# # get all data for these individuals
# mort_all_dat <- test_elk %>% 
#   # filter desired individuals 
#   filter(animal_id %in% mort_ids)
# 
# plots <- list()
# 
# # Plot in a loop
# for(k in 1:length(mort_ids)){
#   # print status
#   print(paste(k, mort_ids[k]))
#   
#   # subset data for individuls (all points for the individal)
#   check <- mort_all_dat %>% 
#     filter(animal_id == mort_ids[[k]]) 
# 
#   # checking just mort data(only 'mort' w/ few points)
#   check2 <- mort %>%
#     filter(animal_id == mort_ids[k])
#   
#   # create df w/ last point of data
#   check$max_x <- check[which.max(check$datetime_UTC), ]$lon_x
#   check$max_y <- check[which.max(check$datetime_UTC), ]$lat_y
# 
#   # create df w/ last point of data (last few points)
#   check2$max_x <- check2[which.max(check2$t_), ]$x_
#   check2$max_y <- check2[which.max(check2$t_), ]$y_
# 
# 
#   # # plot points
#   # plots[[k]] <- ggplot() +
#   #   geom_point(aes(x = check$lon_x, y = check$lat_y), col = "blue") +
#   #   geom_point(aes(x = check$max_x, y = check$max_y), col = "red") +
#   #   ggtitle(check$animal_id) +
#   #   theme_minimal()
# 
# 
#   # # plot points
#   # plot[k] <- ggplot() +
#   #   geom_point(aes(check2$x_, check2$y_), col = "blue") +
#   #   geom_point(aes(check2$max_x, check2$max_y), col = "red") +
#   #   ggtitle(check2$animal_id) +
#   #   theme_minimal()
#   
# # plot points (baseR)
# plot(check$lon_x, check$lat_y, main = mort_ids[k],
#                type = "b")
# points(check$max_x, check$max_y, col = "red", add = T)
# 
#   # plot points
#   plot(check2$x_, check2$y_, main = mort_ids[k],
#        type = "b")
#   points(check2$max_x, check2$max_y, col = "red", add = T)
# 
# }
# 
# # look at plots
# plots[[1]]
# 
# # Rename and reorganize
# # personal preferences here!!!
# elk_clean <- clean %>% 
#   # filter out TRUE for mort drop-offs
#   filter(!defunct_cluster_) %>% 
#   # rename columns
#   rename(utm_x = x_,
#          utm_y = y_, 
#          datetime_UTC = t_)  %>% 
#   # move ID column
#   dplyr::relocate(animal_id, .before = utm_x)
# 
# # Save output file -----
# saveRDS(x, "20220617.rds")
# # DONE! 
# 
# 
# # Look at mortality drop-offs
# mort <- clean %>% 
#   filter(defunct_cluster_ == TRUE) 
# # grab ids
# mort_ids <- unique(mort$animal_id)
# 
# # get all data for these individuals
# mort_all_dat <- test_elk %>% 
#   # filter desired individuals 
#   filter(animal_id %in% mort_ids)
# 
# plots <- list()
# 
# # Plot in a loop
# for(k in 1:length(mort_ids)){
#   # print status
#   print(paste(k, mort_ids[k]))
#   
#   # subset data for individuls (all points for the individal)
#   check <- mort_all_dat %>% 
#     filter(animal_id == mort_ids[[k]]) 
#   
#   # checking just mort data(only 'mort' w/ few points)
#   check2 <- mort %>%
#     filter(animal_id == mort_ids[k])
#   
#   # create df w/ last point of data
#   check$max_x <- check[which.max(check$datetime_UTC), ]$lon_x
#   check$max_y <- check[which.max(check$datetime_UTC), ]$lat_y
#   
#   # create df w/ last point of data (last few points)
#   check2$max_x <- check2[which.max(check2$t_), ]$x_
#   check2$max_y <- check2[which.max(check2$t_), ]$y_
#   
#   
#   # # plot points
#   # plots[[k]] <- ggplot() +
#   #   geom_point(aes(x = check$lon_x, y = check$lat_y), col = "blue") +
#   #   geom_point(aes(x = check$max_x, y = check$max_y), col = "red") +
#   #   ggtitle(check$animal_id) +
#   #   theme_minimal()
#   
#   
#   # # plot points
#   # plot[k] <- ggplot() +
#   #   geom_point(aes(check2$x_, check2$y_), col = "blue") +
#   #   geom_point(aes(check2$max_x, check2$max_y), col = "red") +
#   #   ggtitle(check2$animal_id) +
#   #   theme_minimal()
#   
#   # plot points (baseR)
#   plot(check$lon_x, check$lat_y, main = mort_ids[k],
#        type = "b")
#   points(check$max_x, check$max_y, col = "red", add = T)
#   
#   # plot points
#   plot(check2$x_, check2$y_, main = mort_ids[k],
#        type = "b")
#   points(check2$max_x, check2$max_y, col = "red", add = T)
#   
# }
# 
# # look at plots
# plots[[1]]


