############### Isolating GEE-run elk data ################
##################### 09/28/22#############################\

gc()

# Load in libraries
library(tidyverse)
library(dplyr)

# Pull in csv's that Alex ran through GEE
# These should all have corresponding daymet and ndvi

gee_2011 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2011.csv")
gee_2012 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2012.csv")
gee_2013 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2013.csv")
gee_2014 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2014.csv")
gee_2015 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2015.csv")
gee_2016 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2016.csv")
gee_2017 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2017.csv")
gee_2018 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2018.csv")
gee_2019 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2019.csv")
gee_2020 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2020.csv")
gee_2021 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Alex_GEE_data/HMM_input_2021.csv") 

# Bind all csv's together into one dataframe using row ID

alex_elk <- rbind(gee_2011,gee_2012, gee_2013, gee_2014, gee_2015, gee_2016, gee_2017, gee_2018, gee_2019, gee_2020, gee_2021)

# Pull in cleaned and updated HMM data, part of which hasn't gone through GEE

HMM_burst <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Courtney_Cleaned_Data_20220927/HMM_ready_elk_data_20220926_hierarchical.rds")

# Pull in other clean HMM data (without bursts) - what was used to clean in GEE

clean <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_ready.rds")

# Convert row ID column in GEE dataset to character (because error was happening
# due to row_id in gee_elk being numeric)

alex_elk$row_id <- as.character(alex_elk$row_id)

# Join elk data Alex put through GEE with the main dataset (no bursts) by row ID

join_1 <- clean %>% 
  left_join(alex_elk, by = "row_id")


# Pull all data with NAs for covariate columns and export to dataframe

missing <- join_1 %>% 
  filter(is.na(tmax))

# 09/29/22 - all "missing" points occur on 12/31 of every year between 2011-2020








# Join with HMM burst data by ID to have the most up to date data

join_2 <- join_1 %>% 
  left_join(HMM_burst, by = c("ID", "dt", "x", "y")) 


# Save that dataframe as a csv

write.csv(, "nextround_GEE_data", row.names = FALSE)

