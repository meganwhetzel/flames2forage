### Figuring out dawn/dusk ----
### Tal code ###
### 12/12/2022 ###

# Load packages
library(dplyr)
library(tidyverse)
library(sp)
library(maptools)
library(sf)
library(lubridate)
library(clock)
library(spatialEco)

# Load in rds of data

raw_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/HMM_ready_elk_data_20221207_nonhierarchical.rds")

raw_dat <- raw_dat %>% 
  mutate(row_id = as.character(seq(1:nrow(raw_dat)))) %>%
  relocate(row_id, .before = ID)

### Pull out chunks of data that crepuscule wants ----
# Select just the coordinate columns from the data 
clean_pts <- raw_dat %>%
  mutate(remove_coords = case_when(is.na(x) == TRUE ~ "y",
                                   is.na(x) == FALSE ~ "n")) %>%
  #filter(is.na(x) == FALSE) %>%
  mutate(x = case_when(remove_coords == "y" ~ 430581.44,
                       remove_coords == "n" ~ x),
         y = case_when(remove_coords == "y" ~ 4620732.76,
                       remove_coords == "n" ~ y)) %>%
  dplyr::select(x, y) %>%
  as.data.frame() 

# Pull just datetime column and convert into a data frame 
clean_times <- raw_dat %>%
  #filter(is.na(x) == FALSE) %>%
  dplyr::select(dt) %>%
  mutate(dt = as.character(dt)) %>%
  as.data.frame() 

date_time <- as.POSIXct(clean_times$dt, tz = "MST")

# Get ids for data to rejoin with later
clean_ids <- raw_dat %>%
  #filter(is.na(x) == FALSE) %>%
  mutate(remove_coords = case_when(is.na(x) == TRUE ~ "y",
                                   is.na(x) == FALSE ~ "n")) %>%
  mutate(x = case_when(remove_coords == "y" ~ 430581.44,
                       remove_coords == "n" ~ x),
         y = case_when(remove_coords == "y" ~ 4620732.76,
                       remove_coords == "n" ~ y)) %>%
  as.data.frame() 

### Convert UTMs to Latlong ----
sp_points_utm <- SpatialPoints(clean_pts, 
                               proj4string = CRS("+proj=utm +zone=12 +datum=WGS84"))

sp_points_latlong <- spTransform(sp_points_utm, 
                                 CRS("+proj=longlat +datum=WGS84"))

lnlt <- coordinates(sp_points_latlong)

### Calculate dawn and dusk ----
data_all_dawn <- crepuscule(crds = lnlt, 
                            dateTime = date_time, 
                            solarDep = 6, 
                            direction = "dawn", 
                            POSIXct.out = TRUE) %>%
  rename(day_frac_dawn = day_frac,
         dawn_time = time)

data_all_dusk <- crepuscule(lnlt, date_time, solarDep = 6, direction = "dusk", POSIXct.out = TRUE) %>%
  rename(day_frac_dusk = day_frac,
         dusk_time = time)

complete <- clean_ids %>%
  cbind(x_lnlt = lnlt[,1],
        y_lnlt = lnlt[,2]) %>% 
  cbind(data_all_dawn, data_all_dusk)


### Tal code to determine time to dawn and time to dusk ----
### Time until dawn, dusk & time since dawn, dusk on whole dataset

dat.time.to.dusk.dawn <- complete %>% 
  #mutate(time_to_dawn = as.numeric(as.character(abs(difftime(dt, dawn_time, tz = "MST", units = "hours")))),
  # time_to_dusk = as.numeric(as.character(abs(difftime(dt, dusk_time, tz = "MST", units = "hours"))))) %>%
  #rowwise() %>%
  mutate(time_to_dawn = abs(as.numeric(dt - dawn_time)) / (60 * 60),
         time_to_dusk = abs(as.numeric(dusk_time - dt)) / (60 * 60)) %>%
  mutate(time_to_next_dawn = abs(as.numeric(dt - (dawn_time + (24*60*60)))),
         time_to_previous_dusk = abs(as.numeric((dusk_time - (24*60*60))  - dt))) %>%
  mutate(time_to_dawn = case_when(time_to_dawn > time_to_next_dawn ~ time_to_next_dawn,
                                  TRUE ~ time_to_dawn),
         time_to_dusk = case_when(time_to_dusk > time_to_previous_dusk ~ time_to_previous_dusk,
                                  TRUE ~ time_to_dusk)) %>%
  mutate(log_dawn = log(time_to_dawn + 1),
         log_dusk = log(time_to_dusk + 1)) %>%
  dplyr::select(-c(day_frac_dawn, day_frac_dusk)) %>%
  mutate(x = case_when(remove_coords == "y" ~ as.numeric(NA),
                       remove_coords == "n" ~ x),
         y = case_when(remove_coords == "y" ~ as.numeric(NA),
                       remove_coords == "n" ~ y),
         x_lnlt = case_when(remove_coords == "y" ~ as.numeric(NA),
                            remove_coords == "n" ~ x_lnlt),
         y_lnlt = case_when(remove_coords == "y" ~ as.numeric(NA),
                            remove_coords == "n" ~ y_lnlt)) %>%
  select(-remove_coords)


### Save data ----

saveRDS(dat.time.to.dusk.dawn, "updated_dawn_dusk_20221212.rds")




