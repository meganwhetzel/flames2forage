library(tidyverse)
library(dplyr)
library(terra)
library(sp)

setwd("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Courtney_Cleaned_Data_20220927")

# Read in RDS
HMM_ready <- readRDS("C:/Users/A02362279/Downloads/HMM_ready_elk_data_20220926_hierarchical.rds")

# Filter all points before 12/30/21 due to MODIS data availability
HMM_filter <- HMM_ready %>%
  filter(dt < "2021-12-30 11:59:59")


# Create row ID

HMM_filter <- HMM_filter %>% 
  mutate(row_id = as.character(seq(1:nrow(HMM_filter)))) %>% 
  relocate(row_id, .before = "ID")

# Change UTM to lat/long so it works better in GEE


# Select just the UTM columns from the data ####
clean_latlon <- HMM_filter %>%
  select(y, x) %>%
  as.data.frame() %>% 
  relocate(x, .before = "y")

# Apparently there are NAs in the data so pulled those out (324,249 points??)
clean_na <- clean_latlon %>% 
  filter(is.na(x) == FALSE)


# For dataset ####

sp_points_all <- SpatialPoints(clean_na, proj4string = CRS("+proj=utm +zone=12 +datum=WGS84"))
sp_points_1 <- spTransform(sp_points_all, CRS("+proj=longlat +datum=WGS84"))
lnlt <- coordinates(sp_points_1)


# Remove points from main dataset that had NA's
HMM_filter_2 <- HMM_filter %>% 
  filter(is.na(x) == FALSE)

# Remove old UTM columns
HMM_filter_3 <- HMM_filter_2 %>% 
  select(-c(x,y))
  

# Bind lat lon data back to main dataset

complete <- cbind(HMM_filter_3, lnlt) %>% 
  rename(long = x,
         lat = y)



# Save RDS for future use
saveRDS(complete, "HMM_ready_elk_data_20220926_hierarchical_filtered.rds")

# Save CSV for import into GEE
setwd("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Exported_CSVs")
write.csv(complete, "HMM_ready_elk_data_20220926_hierarchical_filtered.csv", row.names = FALSE)

# Hist of data
hist(HMM_ready_elk_data_20220926_hierarchical_filtered$dt, breaks = 100)




## OLD CODE ##

# Pull elk ID's for subset
unique <- HMM_ready_elk_data_20220926_hierarchical_filtered %>% 
  group_by(ID) %>% 
  tally()


# Create subset of data for use in MTBS data

# subset_elk <- HMM_ready_elk_data_20220926_hierarchical_filtered %>% 
#   subset(
# 
# ID == "EL19F0060", "EL21M0002", "EL21M0005", "EL21F0071", "EL21F0051", "EL21F0052", "EL21F0053", "EL21F0057", "EL21F0058", "EL20M0001", "EL20M0002", "EL20F0075", "EL20F0060", "EL19F0117", "EL18M0009", "EL18M0007", "EL18F0123", "EL18F0074", "EL17F0124", "EL16F0141", "EL13F0008")

    