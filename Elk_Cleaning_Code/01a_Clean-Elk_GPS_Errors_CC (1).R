#################################################X
# ---- CODE FOR CLEANING GPS TRACKING TABLES ----x
#################################################X
# ------- Functions updated: 05/24/2022 ---------X
#--------------------- CEC ----------------------x
#------------------ 05/24/2022 ------------------X
#################################################X
#------------ Last updated: 06-30-2022 ----------X
#################################################X

### Load Packages and Functions ----

library(amt)
library(dplyr)
library(lubridate)
library(tidyverse)

  # Most recent function bug fixes on git
remotes::install_github("jmsigner/amt", ref = "data-cleaning")

### Set Up ----

# EPSG code for WGS84 UTM 12N
utm <- 32612

# Alpha - tells how much time from capture to remove capture effect
alpha <- lubridate::hours(24) %>% # 5/13/22: changed from 10 to 24
  as.numeric() %>%
  lubridate::period() 

# Delta - How far it can move per unit time
speed <- 72 # Max speed of elk is 72 km/hr
time <- lubridate::minutes(10) # Length of time it can maintain the speed
delta <- calculate_sdr(speed, time, speed_unit = "km/h")

# Epsilon, zeta, eta, and theta
epsilon <- 20 # adjusts delta for flagging_round_trips
zeta <- 5
eta <- 5
theta <- lubridate::days(1) # if it hasn't moved in this time, remove for mortality

## Load Data ----

#elk <- readRDS("C:/Users/Avgar Lab/Box/elk_data_cleaning/raw_elk_data_from_UDWR/raw_elk.rds")
elk <- readRDS("/Users/courtney/Box/My_Code/Data/Raw/raw_elk.rds")

cap_dat <- read.csv("/Users/courtney/Box/My_Code/Data/dop_mort_data/capture_table.csv") %>%
  filter(Species == "elk") %>%
  select(Species, AnimalID, DOP)

mort_dat <- readRDS("/Users/courtney/Box/My_Code/Data/dop_mort_data/bigquery_20220810.rds") %>%
  #filter(species == "elk") %>%
  select(AnimalID, MortalityCause) #%>%
  
cap_mort = cap_dat %>%
  left_join(mort_dat, by = c("Species", "AnimalID"))
  # Which DOP to use? ...using cap for now

### Organize Data ----

pre.dat <- elk %>%
  mutate(datetime_UTC = as.POSIXct(datetime_UTC, 
                                   format= "%Y-%m-%d %H:%M:%S", 
                                   tz = "MST")) %>%
  rename(x = long_x,
         y = lat_y,
         datetime = datetime_UTC,
         AnimalID = animal_id,
         Species = species) %>%
  left_join(cap_dat, by = c("Species", "AnimalID")) %>%
  left_join(mort_dat, by = c("AnimalID")) %>%
  mutate(dop = ifelse(is.na(DOP) == FALSE, as.numeric(DOP), 1)) %>%
  # putting DOP of 1 for points with no DOP
   select(-DOP) %>% 
  arrange(datetime)

# Removing individuals with few points

few.pts <- pre.dat %>%
  group_by(AnimalID) %>%
  tally() %>%
  filter(n < 30)

dat <- pre.dat %>%
  filter(!AnimalID %in% few.pts$AnimalID)

### Clean Data  ----

# Grab IDs
id <- unique(dat$AnimalID)

# Get empty df
clean <- data.frame()

for(i in 1:length(id)){
  # print status
  print(paste(i, id[i]))
  
  df <- dat %>% 
  # Filter only the specific individual
    filter(AnimalID == id[i]) %>% 
  
  # Turn into a track_xyt
    mk_track(.x = x, .y = y, .t = datetime, 
                     all_cols = TRUE, 
                     crs = 4326) %>% 
    # project to given crs
    transform_coords(crs_to = utm) %>%
    
    ### Remove capture effect ----
    
    remove_capture_effect(start = alpha) %>%
  
    ### Remove low quality duplicated ----
    flag_duplicates(gamma = minutes(10)) %>% 
    filter(!duplicate_)  %>% 
   
    ### Remove fast steps ----
     flag_fast_steps(delta = delta) %>%
     filter(!fast_step_) %>% 
   
    ### Remove fast roundtrips ----
     flag_roundtrips(delta = delta, epsilon = epsilon, time_unit = "secs") %>% 
     filter(!fast_roundtrip_) %>% 
   
    ## Remove mortality/drop-off clusters ----
    flag_defunct_clusters(zeta = zeta, eta = eta, theta = theta) %>%
    filter(!defunct_cluster_)

  # combine together
  clean <- rbind(df, clean)
}

### Checks ----
# Check flags
table(clean$fast_roundtrip_)

### Rename and reorganize ----
dat_clean <- clean %>%
  # move ID column to front
  relocate(AnimalID, .before = x_) %>%
  # rename columns to original nomenclature
  rename(ID = AnimalID,
         dt = t_,
         x = x_,
         y = y_) %>% 
  # remove unnecessary col
  select(-defunct_cluster_,
         -duplicate_,
         -fast_step_,
         -fast_roundtrip_)

# Remove Zombie Points (i.e. points that occur after the animal's death date)

mort_dates = read.csv("/Users/courtney/Box/My_Code/Data/dop_mort_data/mort_table.csv") %>%
  filter(Species == "elk" & MortalityCause != "Collar Swapped") %>%
  select(AnimalID, UTC) %>%
  mutate(UTC = gsub("T", " ", UTC)) %>%
  rename(ID = "AnimalID",
         death_date = "UTC") %>%
  mutate(death_date = as.POSIXct(death_date, 
                                 format= "%Y-%m-%d %H:%M:%S", 
                                 tz = "MST"))

dat_clean.2 = dat_clean %>%
  left_join(mort_dates, by = "ID") %>%
  mutate(zombie_point = ifelse(!is.na(death_date) & dt > death_date, 
                               TRUE,
                               FALSE)) %>%
  filter(zombie_point == FALSE)

# Check rows
nrow(dat)
# [1] 3861585
nrow(clean)
# [1] 3441498
nrow(dat_clean)
# [1] 3441498
# How many indivs left?
x <- unique(dat_clean$ID)
id # 952

## Save outputs ----
# Save cleaned data
saveRDS(dat_clean.2, "/Users/courtney/Box/My_Code/Data/Cleaned/clean_elk_20220824.rds")

# DONE!

# Questions: 
# a) What happened with the sex/animalid thing?
# b) Ask Brian about the epsilon calculation
