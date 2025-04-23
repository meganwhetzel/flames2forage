### Running fine-scale HMM ###
### CEC code - sent 2022-10-13 #
### MMW adapt to run HMM ###
# Cleaned by B. J. Smith 2024-07-19 ####

### Load Packages ---
library(dplyr)
library(momentuHMM)

# Make sure output directory exists ----
dir.create("out", showWarnings = FALSE)

### Load Data ----
# Reading in prepped data
prep_dat <- readRDS("../data/HMM_prepared_dat_20230113.rds") 

### Fit model ----
fine_HMM <- fitHMM(prep_dat, nbStates = 3,
                   dist = list(step ='gamma', angle = 'vm'),
                   formula = ~ 1,
                   Par0 = list(step=c(10, 1000, 300, 20, 1000, 1000, 0.5, 0, 0), 
                               angle=c(0, 0, 0, 0.01, 0.9, 0.25)), 
                   estAngleMean = list(angle = TRUE),
                   stateNames =  c("resting", "commuting", "foraging")) 

# SAVE THE ACTUAL HMM MODEL
saveRDS(fine_HMM, "out/fine_HMM_model_20230115.rds")

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

saveRDS(fine_dat_full, "out/HMM_with_viterbi_prob_20230115.rds")
