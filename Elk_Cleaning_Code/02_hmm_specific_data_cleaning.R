# Clean env
rm(list = ls())
gc()

# ===============================================x
# ---- CODE FOR CLEANING GPS TRACKING TABLES ----x
# ===============================================x
# ------------------ 2022/02/08 -----------------x
# --------------------- RBH ---------------------x
# ===============================================x

# Johannes' workflow can be found here: 
#   Box/Avgar Lab on WILD/Code/Functions/GPS_datacleaning_guide.html
#
# ----------------------------x
# ---- JOHANNES' WORKFLOW: ----
# ----------------------------x
# 0)  Explore Data (skipping)
#     a)  Plot the data 
#     b)  What is the sampling rate?
#     c)  What is the sampling period?
#     d)  Summary of step lengths
#     e)  Summary of speeds
# 1)  Remove fixes before the tracking device was on the animal
#     (skipping, I'm going to assume the DWR already does this)
# 2)  Remove fixes influenced by the capture 
#     (Remove all fixes taken within the first alpha hours to reduce 
#     uncertainty with time of tagging and any capture effects)
# 3)  Remove locations in unreachable habitat 
# 4)  Remove low quality duplicated 
# 5)  Remove duplicate time stamps 
# 6)  Remove unreasonably fast steps
# 7)  Remove fast roundtrips
# 8)  Remove mortality/drop-off clusters
#     (The DWR probably also does this, but it won't hurt)
#
# ------------------------x
# -------- OUTPUT: --------
# ------------------------x
# For each cleaning method, there will be a new version of trk and a dataframe
#     called "clean"
# the new trk object is the cleaned version of the previous trk dataframe
#     after being run through that particular cleaning function.
# "clean" contains all the rows that were removed and the reason why they were 
#     removed
# "clean" is then returned along with the row ID and timestamp so you can 
#   join it with the original dataframe that you 

# -------------------------------------------------x
# ---- EXPLANATION ON THE FUNCTION'S ARGUMENTS: ----
# -------------------------------------------------x
# x = an individual's GPS tracking dataframe

# func_path = the path to where all of Johannes' functions live

# lon_col, lat_col, time_col, rowid_col, dop_col = columns in x for the lat/lon
#   coordinates, datetime, row ID, and DOP.
#     (if you don't have a column for a row ID, you can just add a column with 
#       sequential numbers. Row ID is important because you need it to join the 
#       points that were filtered out after cleaning with your original
#       dataframe, and I don't know if I would trust the OBJECTID column from
#       UDWR because I've seen some repeat OBJECTIDs. So if you haven't already,
#       I would make a new Row ID column)
#     (if you don't have a DOP column, leave it as NULL)

# epsg = the EPSG code for the CRS you are projecting to (such as UTM 12N)

# alpha, zeta, eta, theta = values to input in some of Johannes' functions

# delta_speed, delta_time = speed and time values to input into Johannes' 
#   function calculate_sdr() in order to calculate delta. 
clean_data <- try(function(x, func_path, lon_col, lat_col, time_col, rowid_col, 
                           dop_col = NULL, epsg, landuse = NULL, alpha, delta_speed,
                           delta_time, epsilon, zeta, eta, theta){
  # Load Packages and Functions ----
  library(amt)
  library(dplyr)
  library(lubridate)
  library(raster)
  library(tidyverse)
  library(data.table)
  library(sp)
  
  #  source("~/Box/Avgar Lab on WILD/Code/Functions/GPS_datacleaning_funs.R")
  
  # on pc
  source("C:/Users/A02362279/Box/Avgar Lab on WILD/Code/Functions/GPS_datacleaning_funs.R")
  
  
  # Turn dataset into a track object ----
  trk <- x %>%
    mk_track(.x = {{lon_col}}, .y = {{lat_col}}, .t = {{time_col}},
             all_cols = TRUE,
             # set to WGS84 crs
             
             crs = 4326) %>% 
    # project to given crs
    transform_coords(crs_to = epsg) %>%
    # rename the row ID column name to "row_id"
    rename(row_id = {{rowid_col}})
  
  if(is.null(dop_col)){
    # if dop_col is NULL (aka if the input dataframe doesn't have a DOP column)
    #   then add a column called dop but make it all NA
    trk <- trk %>%
      mutate(dop = NA)
  } else{
    # if there is a DOP column, rename it to dop
    trk <- trk %>%
      rename(dop = {{dop_col}})
  }
  # only select x, y, t, row ID, and DOP
  trk <- trk %>%
    dplyr::select(c(x_, y_, t_, row_id, dop))
  
  if(nrow(trk) <= 30){
    # 1) Check if the track has more than 30 records ----
    # if not, add a flag
    clean <- trk %>%
      dplyr::select(x_, y_, t_, row_id) %>%
      mutate(clean_why = "Only 30 records")
  } 
  # if so, continue
  else{
    # 2) Remove fixes influenced by the capture ----
    trk2 <- trk %>% 
      remove_capture_effect(start = alpha)
    clean <- trk %>%
      dplyr::select(x_, y_, t_, row_id) %>%
      mutate(clean_why = ifelse(!(row_id %in% trk2$row_id), 
                                "Capture Effect", NA)) %>%
      filter(!(is.na(clean_why)))
    
    # 3) Remove locations in unreachable habitat ----
    # check if a "landuse" raster was given
    if(!is.null(landuse)){
      names(landuse) <- "landuse"
      trk3 <- trk2 %>%
        # pull the values from the raster
        extract_covariates(landuse) %>%
        # filter out points that fall in water
        filter(!(landuse %in% c(577, 578, 579))) 
      # 577 = Open Water (Aquaculture)
      # 578 = Open Water (Brackish/Salt)
      # 579 = Open Water (Fresh) 
      clean <- trk2 %>%
        dplyr::select(x_, y_, t_, row_id) %>%
        mutate(clean_why = ifelse(!(row_id %in% trk3$row_id), 
                                  "Unreachable Habitat", NA)) %>%
        rbind(clean) %>%
        filter(!(is.na(clean_why)))
    } else{
      trk3 <- trk2 %>%
        mutate(landuse = NA)
    }
    
    # 4)  Remove low quality duplicated ----
    # check if all the values in the DOP column are NA or not
    if(!all(is.na(trk3$dop))){
      # if there are some values that are not NA, only filter those
      trk3_dop <- trk3 %>%
        filter(!is.na(dop))
      # we still want to keep the records that have NA for DOP, however
      trk3_no_dop <- trk3 %>%
        filter(is.na(dop))
      # perform the cleaning function and then bind the resulting dataframe
      # with the NA DOP records
      trk4 <- remove_low_quality_duplicates(trk3_dop, gamma = minutes(30)) %>%
        rbind(trk3_no_dop)
      clean <- trk3 %>%
        dplyr::select(x_, y_, t_, row_id) %>%
        mutate(clean_why = ifelse(!(row_id %in% trk4$row_id), 
                                  "Low Quality Dup", NA)) %>%
        rbind(clean) %>%
        filter(!(is.na(clean_why)))
      rm(trk3_dop, trk3_no_dop)
    } else{
      trk4 <- trk3
    }
    
    # 5)  Remove duplicate time stamps ----
    dup <- trk4 %>%
      group_by(t_) %>%
      filter(n() > 1) %>% 
      distinct(t_) %>% 
      pull()
    trk5 <- trk4 %>%
      filter(!t_ %in% dup)
    clean <- trk4 %>%
      dplyr::select(x_, y_, t_, row_id) %>%
      mutate(clean_why = ifelse(!(row_id %in% trk5$row_id), 
                                "Duplicate Time Stamp", NA)) %>%
      rbind(clean) %>%
      filter(!(is.na(clean_why)))
    
    # 6)  Remove unreasonably fast steps ----
    delta <- calculate_sdr(delta_speed, time = delta_time)
    trk6 <- remove_fast_steps(trk5, delta = delta)
    clean <- trk5 %>%
      dplyr::select(x_, y_, t_, row_id) %>%
      mutate(clean_why = ifelse(!(row_id %in% trk6$row_id), 
                                "Fast Step", NA)) %>%
      rbind(clean) %>%
      filter(!(is.na(clean_why)))
    
    # 7)  Remove fast roundtrips ----
    trk7 <- remove_roundtrips(trk6, delta = delta, epsilon = epsilon)
    clean <- trk6 %>%
      dplyr::select(x_, y_, t_, row_id) %>%
      mutate(clean_why = ifelse(!(row_id %in% trk7$row_id), 
                                "Fast Roundtrip", NA)) %>%
      rbind(clean) %>%
      filter(!(is.na(clean_why)))
    
    # 8)  Remove mortality/drop-off clusters ----
    trk8 <- remove_defunc_clusters(trk7, zeta, eta, theta)
    clean <- trk7 %>%
      dplyr::select(x_, y_, t_, row_id) %>%
      mutate(clean_why = ifelse(!(row_id %in% trk8$row_id), 
                                "Drop-Off Cluster", NA)) %>%
      rbind(clean) %>%
      filter(!(is.na(clean_why)))
  }
  return(clean)
})

# Set Up ----
## Load Packages ----
library(tidyverse)
library(lubridate)
library(raster)

## Set Inputs ----
# NOTE: YOU MIGHT NEED TO CHANGE INPUTS (SUCH AS PATHS TO DIRECTORIES, 
#   COLUMN NAMES, ETC) BASED ON YOUR DATASET AND WORKING DIRECTORY
# (the paths assume your working directory is in Box/Avgar Lab on WILD/)

# path to Johannes' cleaning functions (can be found in this box folder)
func_path_ <- "Code/Functions/GPS_datacleaning_funs.R"

# EPSG code for WGS84 UTM 12N
utm <- 32612

# landcover raster
landcover <- raster("../../../Avgar Lab on WILD/UtahEnvironmentalCovariates/Landcover/ut_landcover_utm.tif")
landcover
plot(landcover)

# for delta (different for different species)
# speed is in km/hr

# pr = pronghorn
# peed_pr <- 50
# time_pr <- lubridate::minutes(60)s

# md = mule deer
# speed_md <- 50
# time_md <- lubridate::minutes(5)

# elk
speed_elk <- 50
time_elk <- lubridate::minutes(10)

# All of these values are taken from Johannes' examples
alpha_ <- lubridate::hours(10) %>%
  as.numeric() %>%
  lubridate::period()

zeta_ <- 5
eta_ <- 5
theta_ <- lubridate::days(3)

# there could be an issue in remove_roundtrips() where epsilon is flagging too 
#   many points that are fine. This would mean needing to change epsilon 
#   depending on how frequent points are being flagged and removed
epsilon_ <- 10

# Load Data ----
# Load your data here, either from a .rds file, .csv file, or a database

# on mac
# elk <- readRDS("~/Box/MSc work/elk_data_cleaning/20220216_elk_data_BigQuery.rds")

# on desktop
elk_dat <- readRDS("C:/Users/A02362279/Box/MSc work/Elk_Cleaning_new/20220216_elk_data_BigQuery.rds")

# Changing column names, removing unnecessary columns, moving row id

elk <- elk_dat %>%
  mutate(row_id = as.character(seq(1:nrow(elk_dat)))) %>%
  rename(collar_id = CollarID,
         animal_id = AnimalID,
         lat_y = Latitude,
         lon_x = Longitude,
         datetime_UTC = DT_UTC,
         dop = Dop) %>% 
  dplyr::select(-c(OBJECTID, Species, Freq, CurrentCohort, ProjectName, 
                   CaptureUnit, CaptureSubunit, CaptureArea, BirthYear, DT_MST,
                   DateAndTime)) %>%   
  relocate(row_id, .before = "collar_id") %>% 
  # arrange by timestamp
  arrange(datetime_UTC)

# get a vector of every animal ID

id <- unique(elk$animal_id)

# Elk that had errors while running data
remove <- c("EL18F0133", "EL18M0021", "EL19F0064")


# Clean Data ----
# Loop through every ID
# REMEMBER TO CHANGE COLUMN NAMES IF NEEDED
clean <- lapply(1:length(id), function(i){
  # print status
  print(paste(i, id[i]))
  
  # filter only the specific individual
  df <- elk %>%
    # sort records chronologically
    arrange(datetime_UTC) %>%
    # filter out this specific individual
    filter(animal_id == id[i]) %>% 
    # removing the 3 elk that had errors
    filter(!(animal_id %in% remove))
  
  # apply full cleaning function (returns a dataframe of JUST the rows that
  # were removed post-clean and the reason why they were cleaned)
  clean <- df %>%
    clean_data(func_path = func_path_, lon_col = "lon_x", lat_col = "lat_y",
               time_col = "datetime_UTC", rowid_col = "row_id", dop_col = "dop",
               epsg = utm, alpha = alpha_, epsilon = epsilon_, zeta = zeta_, 
               eta = eta_, theta = theta_, landuse = landcover,
               # REMEMBER TO CHANGE THESE INPUTS BASED ON YOUR SPECIES
               delta_speed = speed_elk, delta_time = time_elk)
  
  # join the rows that were cleaned out with the original, full dataframe
  df_cleaned <- clean %>%
    # rename the datetime column in "clean" to match the datetime column in the 
    # original dataframe
    rename(datetime_UTC = t_) %>%
    # remove the UTM x and y columns
    dplyr::select(-c(x_, y_)) %>%
    # join the two together
    full_join(df, by = c("datetime_UTC", "row_id")) %>%
    # add a TRUE/FALSE column 
    # flagging if that row needs to be cleaned out or not
    mutate(to_be_cleaned = ifelse(is.na(clean_why), FALSE, TRUE)) %>%
    # move both columns to the end of the dataframe
    relocate(clean_why, .after = to_be_cleaned) %>%
    # order sequentially by row ID
    arrange(row_id)
  return(df_cleaned)
}) %>%
  # bind every individual's cleaned dataframe into one full dataframe
  bind_rows()
clean

# Save
# save your cleaned dataset where you need to

write_rds(clean, "clean_flagged_data_30.rds")

## In the future, when you want to filter out the points flagged with errors
## you would want to do something like this:
#  dat = your full dataset with the 2 cleaning columns

# elk_cleaned <- clean %>%
#    filter(!to_be_cleaned)


# look at numbers of flagged points
clean_flagged_data_30 %>% 
  group_by(clean_why) %>% 
  tally()

# clean_why                  n
# <chr>                  <int>
#   1 Capture Effect          4156
# 2 Drop-Off Cluster        1300
# 3 Duplicate Time Stamp       6
# 4 Fast Roundtrip            30
# 5 Low Quality Dup       354672
# 6 Only 30 records          221
# 7 Unreachable Habitat      760
# 8 NA                   4273380





