library(lubridate)
library(dplyr)
library(tidyr)
library(mgcv)
library(MuMIn)

# functions Brian created for data prep
source("99_fun.R")

raw <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_Data/Full_fire_data/complete_fire_data_20230115.rds")
#### data prep ####
dat <- raw %>% 
  # General data prep
  data_prep() %>% 
  # Create interactions
  intxn()

# Scale and center
# Variables to scale and center
sc_vars <- c("foraging_prior", "foraging.prior", "snd",
             "cover.herbaceous_NDVI.norm.curr",
             "cover.herbaceous_NDVI.delta",
             "cover.shrub_NDVI.norm.curr",
             "cover.shrub_NDVI.delta",
             "cover.tree_NDVI.norm.curr",
             "cover.tree_NDVI.delta",
             "daydiff.log")

# Get mean and standard deviation for all variables to scale and center
sc_dat <- dat %>% 
  select(sc_vars) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>% 
  as.data.frame()

# Scale and center data
dat2 <- dat %>% 
  scale_data(sc_dat = sc_dat)

# dat2 %>%
# ggplot(aes(x = NDVI.norm.curr)) +
#   geom_histogram()

# Brian helped me set up this one
gam.null <- gam(foraging ~ 0 + 
                  # intercepts
                  is.other +
                  is.juniper +
                  is.conifer +
                  is.aspen +
                  # an autoregressive term
                  foraging_prior + 
                  # the effect of snow depth
                  snd + 
                  # effects of cover
                  is.other:cover.tree +
                  is.juniper:cover.tree +
                  is.conifer:cover.tree +
                  is.aspen:cover.tree +
                  # effects of greenness
                  cover.herbaceous_NDVI.norm.curr +
                  cover.herbaceous_NDVI.delta +
                  cover.shrub_NDVI.norm.curr +
                  cover.shrub_NDVI.delta +
                  is.aspen:cover.tree_NDVI.norm.curr +
                  is.aspen:cover.tree_NDVI.delta + 
                  # splines
                  s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
                  s(julian.day, bs = "cc", k = 13) + # a circular spline accounting for time of year
                  s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
                  s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
                family = betar, data = dat,
                knots = list(julian.day = seq(0, 366, length.out = 13)),
                method = "REML")

# Brian helped me with this one
gam.fire <- gam(foraging ~ 0 + 
                  # intercepts
                  unburned.out:is.other +
                  unburned.out:is.juniper +
                  unburned.out:is.conifer +
                  unburned.out:is.aspen +
                  unburned.in:is.other +
                  unburned.in:is.juniper +
                  unburned.in:is.conifer +
                  unburned.in:is.aspen +
                  burned.1:is.other +
                  burned.1:is.juniper +
                  burned.1:is.conifer +
                  burned.1:is.aspen +
                  burned.2:is.other +
                  burned.2:is.juniper +
                  burned.2:is.conifer +
                  burned.2:is.aspen +
                  burned.3:is.other +
                  burned.3:is.juniper +
                  burned.3:is.conifer +
                  burned.3:is.aspen +
                  # an autoregressive term
                  foraging_prior + 
                  # the effect of snow depth
                  snd + 
                  # effects of time since fire
                  unburned.in:is.other:daydiff.log +
                  unburned.in:is.juniper:daydiff.log +
                  unburned.in:is.conifer:daydiff.log +
                  unburned.in:is.aspen:daydiff.log +
                  burned.1:is.other:daydiff.log +
                  burned.1:is.juniper:daydiff.log +
                  burned.1:is.conifer:daydiff.log +
                  burned.1:is.aspen:daydiff.log +
                  burned.2:is.other:daydiff.log +
                  burned.2:is.juniper:daydiff.log +
                  burned.2:is.conifer:daydiff.log +
                  burned.2:is.aspen:daydiff.log +
                  burned.3:is.other:daydiff.log +
                  burned.3:is.juniper:daydiff.log +
                  burned.3:is.conifer:daydiff.log +
                  burned.3:is.aspen:daydiff.log +
                  unburned.in:is.other:I(daydiff.log ^ 2) +
                  unburned.in:is.juniper:I(daydiff.log ^ 2) +
                  unburned.in:is.conifer:I(daydiff.log ^ 2) +
                  unburned.in:is.aspen:I(daydiff.log ^ 2) +
                  burned.1:is.other:I(daydiff.log ^ 2) +
                  burned.1:is.juniper:I(daydiff.log ^ 2) +
                  burned.1:is.conifer:I(daydiff.log ^ 2) +
                  burned.1:is.aspen:I(daydiff.log ^ 2) +
                  burned.2:is.other:I(daydiff.log ^ 2) +
                  burned.2:is.juniper:I(daydiff.log ^ 2) +
                  burned.2:is.conifer:I(daydiff.log ^ 2) +
                  burned.2:is.aspen:I(daydiff.log ^ 2) +
                  burned.3:is.other:I(daydiff.log ^ 2) +
                  burned.3:is.juniper:I(daydiff.log ^ 2) +
                  burned.3:is.conifer:I(daydiff.log ^ 2) +
                  burned.3:is.aspen:I(daydiff.log ^ 2) +
                  s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
                  s(julian.day, bs = "cc", k = 13) + # a circular spline accounting for time of year
                  s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
                  s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
                family = betar, data = dat,
                knots = list(julian.day = seq(0, 366, length.out = 13)),
                method = "REML")


### This is my attempt at the combination between fire and null (NDVI and fire)
gam.combo <- gam(foraging ~ 0 + 
                  # intercepts
                  unburned.out:is.other +
                  unburned.out:is.juniper +
                  unburned.out:is.conifer +
                  unburned.out:is.aspen +
                  unburned.in:is.other +
                  unburned.in:is.juniper +
                  unburned.in:is.conifer +
                  unburned.in:is.aspen +
                  burned.1:is.other +
                  burned.1:is.juniper +
                  burned.1:is.conifer +
                  burned.1:is.aspen +
                  burned.2:is.other +
                  burned.2:is.juniper +
                  burned.2:is.conifer +
                  burned.2:is.aspen +
                  burned.3:is.other +
                  burned.3:is.juniper +
                  burned.3:is.conifer +
                  burned.3:is.aspen +
                  # an autoregressive term
                  foraging_prior + 
                  # the effect of snow depth
                  snd + 
                  # effects of cover
                  is.other:cover.tree +
                  is.juniper:cover.tree +
                  is.conifer:cover.tree +
                  is.aspen:cover.tree +
                  # effects of greenness
                  cover.herbaceous_NDVI.norm.curr +
                  cover.herbaceous_NDVI.delta +
                  cover.shrub_NDVI.norm.curr +
                  cover.shrub_NDVI.delta +
                  is.aspen:cover.tree_NDVI.norm.curr +
                  is.aspen:cover.tree_NDVI.delta + 
                  # effects of time since fire
                  unburned.in:is.other:daydiff.log +
                  unburned.in:is.juniper:daydiff.log +
                  unburned.in:is.conifer:daydiff.log +
                  unburned.in:is.aspen:daydiff.log +
                  burned.1:is.other:daydiff.log +
                  burned.1:is.juniper:daydiff.log +
                  burned.1:is.conifer:daydiff.log +
                  burned.1:is.aspen:daydiff.log +
                  burned.2:is.other:daydiff.log +
                  burned.2:is.juniper:daydiff.log +
                  burned.2:is.conifer:daydiff.log +
                  burned.2:is.aspen:daydiff.log +
                  burned.3:is.other:daydiff.log +
                  burned.3:is.juniper:daydiff.log +
                  burned.3:is.conifer:daydiff.log +
                  burned.3:is.aspen:daydiff.log +
                  unburned.in:is.other:I(daydiff.log ^ 2) +
                  unburned.in:is.juniper:I(daydiff.log ^ 2) +
                  unburned.in:is.conifer:I(daydiff.log ^ 2) +
                  unburned.in:is.aspen:I(daydiff.log ^ 2) +
                  burned.1:is.other:I(daydiff.log ^ 2) +
                  burned.1:is.juniper:I(daydiff.log ^ 2) +
                  burned.1:is.conifer:I(daydiff.log ^ 2) +
                  burned.1:is.aspen:I(daydiff.log ^ 2) +
                  burned.2:is.other:I(daydiff.log ^ 2) +
                  burned.2:is.juniper:I(daydiff.log ^ 2) +
                  burned.2:is.conifer:I(daydiff.log ^ 2) +
                  burned.2:is.aspen:I(daydiff.log ^ 2) +
                  burned.3:is.other:I(daydiff.log ^ 2) +
                  burned.3:is.juniper:I(daydiff.log ^ 2) +
                  burned.3:is.conifer:I(daydiff.log ^ 2) +
                  burned.3:is.aspen:I(daydiff.log ^ 2) +
                  s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
                  s(julian.day, bs = "cc", k = 13) + # a circular spline accounting for time of year
                  s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
                  s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
                family = betar, data = dat,
                knots = list(julian.day = seq(0, 366, length.out = 13)),
                method = "REML")
