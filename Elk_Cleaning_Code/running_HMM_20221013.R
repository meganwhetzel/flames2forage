### Running fine-scale HMM ###
### CEC code - sent 2022-10-13 #
### MMW adapt to run HMM ###

### Load Packages ---
library("dplyr")
library(amt)
library(tidyverse)
library(momentuHMM)
library(fitdistrplus)
library(circular)
library(lubridate)
# library(moveVis)
library(raster)
library(data.tree)

### Load Data ----

raw_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/HMM_ready_elk_data_20221207_nonhierarchical.rds")

# raw_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_RDS_Data/HMM_ready_elk_data_20220925_nonhierarchical.rds") %>%
# #  filter(level %in% c("2", "2i")) %>%
#   rename(animalid = ID,
#          ID = burstid)  %>%
#   as.data.frame() 
# #  mutate(type_twilight = case_when(type_twilight == "dawn" ~ 1,
# #                                    type_twilight == "dusk" ~ 0))

raw_dat <- raw_dat %>% 
  mutate(row_id = as.character(seq(1:nrow(raw_dat)))) %>%
  relocate(row_id, .before = ID)

### Remove Few Points ----
few_pts_real <- raw_dat %>%
  filter(is.na(x) == FALSE) %>%
  group_by(ID) %>%
  tally() %>%
  ungroup()

dat_start <- raw_dat %>%
  left_join(few_pts_real, by = "ID") %>%
  filter(n > 3) %>%
  dplyr::select(-n) %>%
  ungroup() %>%
  filter(is.na(x) == FALSE)
#filter(ID %in% c("EL13F0002.1", "EL22F0078.3948289", "EL20F0059.2829244"))

### Calculate step lengths ----

prep_dat <- prepData(data = dat_start,
                    type = "UTM",
                    coordNames = c("x", "y"))
saveRDS(prep_dat, "HMM_prepared_dat_20230113.rds")

# Write csv for use in GEE
write.csv(dat_start, "december_hmm_dat.csv", row.names = FALSE)

### Reading in prepped data for quicker calculations
prep_dat <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/HMM_prepared_dat_20230113.rds")

### Check Coarse-scale Distributions ----
# Convert gamma parameters from mean/sd to shape/scale
gamma_pars <- function(mean = NULL, sd = NULL,
                       scale = NULL, shape = NULL) {
  if(exists("mean") & exists("sd")) {
    
    scale_theta <- sqrt(sd^2/mean)
    shape_kappa <- mean/scale_theta
    
    pars <- c(scale_theta = scale_theta,
              shape_kappa = shape_kappa)
  } else if(exists("scale") & exists("shape")) {
    
    mean <- shape * scale
    sd <- sqrt(scale^2 * shape)
    
    pars <- c(mean = mean,
              sd = sd)
    
  }
  
  return(pars)
}

dist_fine <- prep_dat %>%
  filter(is.na(step) == FALSE) #%>%
#filter(step > 5000) #%>%
#group_by(month(dt)) %>%
#tally()

resting <- gamma_pars(mean = 10, sd = 20) 
# Long steps, a lot of variation
commuting <- gamma_pars(mean = 1000, sd = 1000)

foraging <- gamma_pars(mean = 300, sd = 1000)

# Plot a histogram of the observed steps
#hist(log(dist_fine$step), breaks = 200, freq = FALSE)

hist(dist_fine$step, breaks = 100, freq = FALSE)
# Overlay the two distributions. Iterate with new values until happy
lines(dgamma(x = c(1:3758885), shape = resting[2], scale = resting[1]), col = "red")
lines(dgamma(x = c(1:3758885), shape = commuting[2], scale = commuting[1]), col = "blue")
lines(dgamma(x = c(1:3758885), shape = foraging[2], scale = foraging[1]), col = "springgreen")

test <- prep_dat %>%
  filter(animalid %in% c("EL13F0006", "EL21M0005", "EL18F0105", "EL17F0124", "EL19M0003"))

### Fit fine-scale HMM ----

### Small test dataset
# # fine_HMM_test <- fitHMM(test, nbStates = 3,
#                    dist = list(step ='gamma', angle = 'vm'),
#                    formula = ~min_twilight+type_twilight+min_twilight*type_twilight,
#                    Par0 = list(step=c(10, 1000, 300, 20, 1000, 1000, 0, 0, 0), # need to fix this properly
#                                angle=c(0, 0, 0, 0.01, 0.9, 0.25)), # need to find out what this means and needed numbers
#                    estAngleMean = list(angle = TRUE),
#                    stateNames =  c("resting", "commuting", "foraging"))

# probs <- stateProbs(fine_HMM_test)

# Do I need to bind these probabilities back to main data?
# probs_test_full <- rbind()

# Get viterbi assignments (1-3) for most likely state at each step 
# fine_state_test <- viterbi(fine_HMM_test)
# 
# fine_dat_test <- test %>%
#   cbind(as.data.frame(fine_state_test)) %>%
#   cbind(as.data.frame(probs)) %>% 
#   mutate(fine_state_test = case_when(fine_state_test == 1 ~ "resting",
#                                 fine_state_test == 2 ~ "commuting",
#                                 fine_state_test == 3 ~ "foraging"))
#   
# num_states <- fine_dat_test %>% 
#   group_by(fine_state_test) %>%
#   tally()
# 


### Full dataset ----
fine_HMM <- fitHMM(prep_dat, nbStates = 3,
                     dist = list(step ='gamma', angle = 'vm'),
                     formula = ~1,
                     Par0 = list(step=c(10, 1000, 300, 20, 1000, 1000, 0.5, 0, 0), 
                                 angle=c(0, 0, 0, 0.01, 0.9, 0.25)), 
                     estAngleMean = list(angle = TRUE),
                     stateNames =  c("resting", "commuting", "foraging")) 

# Plot to see transition probabilities and maps
# not worth it on huge dataset
# plot(fine_HMM)

# SAVE THE ACTUAL HMM MODEL

saveRDS(fine_HMM, "fine_HMM_model_20230115.rds")


# State probabilities for the data
probs_HMM <- stateProbs(fine_HMM)

# Get viterbi assignments (1-3)
fine_state  <- viterbi(fine_HMM)


fine_dat_full  <-  prep_dat %>%
  cbind(as.data.frame(fine_state)) %>%
  cbind(as.data.frame(probs_HMM)) %>% 
  mutate(fine_state = case_when(fine_state == 1 ~ "resting",
                                  fine_state == 2 ~ "commuting",
                                fine_state == 3 ~ "foraging"))

num_states_full <- fine_dat_full %>%
  group_by(fine_state) %>% 
  tally()

# cfd <- complete_fire_data %>% 
#   group_by(fine_state) %>% 
#   summarise(n = n()) %>% 
#   mutate(freq = n / sum(n))


plotSpatialCov(fine_HMM_model, utah, states = HMM_with_viterbi_prob$fine_state)

saveRDS(fine_dat_full, "HMM_with_viterbi_prob_20230115.rds")
