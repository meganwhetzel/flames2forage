### Joining multiple dataframes for analysis ###
              ### 11/28/22 ###

library(tidyverse)
library(dplyr)
library(TAF)

### 12/12/2022 ----
# Reran HMM, GEE, and dusk/dawn code - should be good to join data

# 1) Read in all data needed for joins

# HMM data results that was re-run on 01/15/2023

hmm_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/HMM_with_viterbi_prob_20230115.rds")

# removing dates after 12/31/2021 for data availability - this matches input as 
# it should
hmm_filter <- hmm_dat %>% 
  filter(is.na(x) == FALSE)

# for some reason this is not matching - has 3335080 instead of 3335834
hmm_filter_2 <- hmm_filter %>% 
  filter(dt < "2021-12-31 17:00:00") # matches when set to 5 pm

hmm_filter_2$row_id <- as.numeric(hmm_filter_2$row_id)

# # Took the HMM input data and filtered down - this date and time is where data
# # stops
# dec_dat <- december_hmm_dat %>%
#   filter(dt < "2021-12-31 10:00:00")
# 
# # # figuring out why 754 rows are missing
# # test <- dec_dat %>% 
# #   left_join(check_2, by = "row_id")
# # 
# # filter(test, is.na(x.y) == TRUE) %>% 
# #   View()
# 
# # they all appear to be rows on 12/31/21 AFTER 10:00 am (which should be
# # filtered out and are not)
# 
# # this binds nicely but is not accurate
# ugh <- dec_dat %>% 
#   left_join(gee_elk, by = "row_id")



# Elk points with fire perimeter data and veg info

elk_rdnbr <- read.csv("C:/Users/A02362279/Box/MeganResearch/MTBS data/fire_elk_veg_rerun/elk_fire_veg_rerun.csv")

# Completed GEE data

# elk_2013to2017 <- read_csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_covariates_scripts/December_GEE/HMM_2013to2017.csv")
# elk_2018 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_covariates_scripts/December_GEE/HMM_input_2018.csv")
# elk_2019 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_covariates_scripts/December_GEE/HMM_input_2019.csv")
# elk_2020 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_covariates_scripts/December_GEE/HMM_input_2020.csv")
# elk_2021 <- read.csv("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_covariates_scripts/December_GEE/HMM_input_2021.csv")
# 
# gee_elk <- rbind(elk_2013to2017, elk_2018, elk_2019, elk_2020, elk_2021)
# 
# rm(elk_2013to2017, elk_2018, elk_2019, elk_2020, elk_2021)
# saveRDS(gee_elk, "december2022_full_gee.rds")

gee_elk <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/december2022_full_gee.rds")

# Read in crepuscule data to get dawn time and dusk time
crepuscule <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/updated_dawn_dusk_20221212.rds")

# 1a) left join crepuscule data to HMM to get dawn and dusk columns

crepuscule_2 <- crepuscule %>% 
  rename(long = x_lnlt,
         lat = y_lnlt) %>% 
  select(row_id, long:log_dusk)

crepuscule_2$row_id <- as.numeric(crepuscule_2$row_id)

dawn_dusk_orig <- hmm_filter_2 %>% 
 left_join(crepuscule_2, by = "row_id") %>% 
  relocate(row_id, .before = ID)


# 2) Left joins ----

# Left join the HMM with the GEE covariate output by row_id

gee_elk$row_id <- as.numeric(gee_elk$row_id)
elk_rdnbr$row_id <- as.numeric(elk_rdnbr$row_id)


orig_and_gee <- dawn_dusk_orig %>% 
  left_join(gee_elk, by = "row_id")


# Left join that with fire perimeter data by row_id

rdnbr_2 <- elk_rdnbr %>% 
  dplyr::select(row_id, Evnt_ID:espgroup)
 

orig_gee_fire <- orig_and_gee %>% 
left_join(rdnbr_2, by = "row_id") #join fire data to HMM and GEE

orig_gee_fire %>% 
filter(is.na(espgroup) == TRUE) %>% 
  View()


# Dividing columns by values from GEE

full_data_div <- orig_gee_fire %>% 
  div(cols = c("pdsi", "tmin", "tmax", "prcp", "swe", 
               "slope", "aspect", "ruggedness"), by = 100) %>% 
  div(cols = c("NDVI_Previous", "NDVI_Current", "NDVI_Subsequent"), by = 10000) 

# Removing rows that have no step length

full_data_div_2 <- full_data_div %>% 
  filter(is.na(step) == FALSE)




# SAVE!!!!

saveRDS(full_data_div_2, "full_elk_dataset_20230115.rds")




# Pull data that has all values for fire data

full_data_fire <- full_data_div_2 %>% 
  filter(is.na(elkpn_6) == FALSE) 

full_data_fire_2 <- full_data_fire %>%
  rename(animalid = ID) %>% 
  mutate(elkpn_6 = as.factor(elkpn_6)) %>% 
  mutate(animalid = as.factor(animalid)) %>% 
  mutate(Evnt_ID = as.factor(Evnt_ID)) %>% 
  mutate(for_nf = as.numeric(case_when(fine_state == "foraging" ~ "1",
                                       fine_state == "commuting" ~ "0",
                                       fine_state == "resting" ~ "0"))) %>% 
  relocate(for_nf, .after = foraging) %>% 
  separate(dt, c("date", "time"), sep = " ", remove = FALSE) %>% 
  relocate(date:time, .after = dt) 

saveRDS(full_data_fire_2, "complete_fire_data_20230115.rds")














### 12/06/22 - need to re-run all of this due to issues with dusk and dawn
# and because of updated rdnbr points

###### OLD CODE FROM FIRST ATTEMPT AT JOINING DATA BEFORE RE-RUN ##############
# # 1) Read in data ----
# 
# # RDS of original (first run through) HMM input data
# 
# orig_HMM <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/HMM_ready_elk_data_20220926_hierarchical_filtered.rds")
# 
# # RDS of second run through HMM dataframe (results)
# 
# HMM_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Data_analysis/HMM_with_viterbi_prob_20221129.rds")
# 
# # MTBS fire perimeter data
# elk_rdnbr <- read.csv("C:/Users/A02362279/Box/MeganResearch/MTBS data/reducedelkpoints_20221206.csv") %>% 
#   mutate(level = case_when(as.character(level) == "2+0i" ~ "2",
#                            as.character(level) == "0+2i" ~ "2i",
#                            as.character(level) == "1+0i" ~ "1")) 
# class(elk_rdnbr$level)
# 
# # Completed GEE data
# gee_elk <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/GEE_covariates_scripts/gee_elk_full_data.rds")
# 
# # Read in crepuscule data to get dawn time and dusk time
# crepuscule <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/HMM_ready_elk_data_20220926_hierarchical_with_crespuscule.rds")


# # Joining reference data to newly run HMM output ----
# 
# # Renaming columns to allow for easy joins
# 
# orig_gee_fire_4 <- orig_gee_fire_3 %>% 
#   rename(animalid = ID,
#          ID = burstid) 
# 
# old_new_join <- orig_gee_fire_4 %>% 
#   left_join(HMM_dat, by = c("ID", "animalid", "dt"))
# 
# 
# # filter out all NA's in step length from the second run through
# orig_gee_fire_ref <- old_new_join %>% 
#   filter(is.na(step) == FALSE)


