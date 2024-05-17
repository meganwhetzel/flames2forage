

# Load packages ----
library(mgcv)
library(tidyverse)
library(ggplot2)
library(NatParksPalettes)

# Load custom scripts ----
source("99_fun.R")

# Load fitted models ----
# gam_null <- readRDS("Model_scripts_outputs/betar_gam_results/gam_null_20230215.rds")
gam_fire <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Model_scripts_outputs/betar_gam_results/gam_bam_fire_20230301.rds")

# Load scaling data ----
scale_df <- read.csv("scaling_data.csv")
# Use "name" column as row.names
row.names(scale_df) <- scale_df$name

# Model prediction ----

# ... Veg categories and burn severity ----
# Create a 16-panel figure, which crosses all the veg categories and the fire
# severity categories
# "newdata" argument for prediction
nd <- pred_data(scale_df, 
                daydiff.log = log(seq(0.1, 30, length.out = 500) * 365.25),
                veg_type = c("aspen", "conifer", "juniper"),
                burn_type = c("l", "m", "h", "o")
                ) %>% 
  # Create the dummy variables
  dummy_vars() %>% 
  # Create the interactions
  intxn() %>% 
  # Give each veg-type/burn-type combo a name
  mutate(type = case_when(
    veg_type == "aspen" & burn_type == "o" ~ "Unburned Aspen",
    veg_type == "aspen" & burn_type == "l" ~ "Low Severity Aspen",
    veg_type == "aspen" & burn_type == "m" ~ "Mod Severity Aspen",
    veg_type == "aspen" & burn_type == "h" ~ "High Severity Aspen",
    veg_type == "conifer" & burn_type == "o" ~ "Unburned Conifer",
    veg_type == "conifer" & burn_type == "l" ~ "Low Severity Conifer",
    veg_type == "conifer" & burn_type == "m" ~ "Mod Severity Conifer",
    veg_type == "conifer" & burn_type == "h" ~ "High Severity Conifer",
    veg_type == "juniper" & burn_type == "o" ~ "Unburned Juniper",
    veg_type == "juniper" & burn_type == "l" ~ "Low Severity Juniper",
    veg_type == "juniper" & burn_type == "m" ~ "Mod Severity Juniper",
    veg_type == "juniper" & burn_type == "h" ~ "High Severity Juniper",
    # veg_type == "other" & burn_type == "o" ~ "Unburned Other",
    # veg_type == "other" & burn_type == "l" ~ "Low Severity Other",
    # veg_type == "other" & burn_type == "m" ~ "Mod Severity Other",
    # veg_type == "other" & burn_type == "h" ~ "High Severity Other"
  ))

# Convert 'daydiff.log' to years
# Units are currently log(days)
nd$years <- exp(nd$daydiff.log) / 365.25


pred <- predict(gam_fire, newdata = nd, type = "response", se.fit = TRUE,
                exclude = c("s(animalid)", 
                            "s(log_dawn,log_dusk)",
                            "s(julian.day)",
                            "s(Evnt_ID)"))

nd$pred <- pred$fit
nd$lwr <- pred$fit - 1.96 * pred$se.fit
nd$upr <- pred$fit + 1.96 * pred$se.fit

nd %>% 
mutate(type = factor(type, levels = c("Unburned Aspen", "Low Severity Aspen",
                                      "Mod Severity Aspen", "High Severity Aspen",
                                      "Unburned Conifer", "Low Severity Conifer",
                                      "Mod Severity Conifer", "High Severity Conifer",
                                      "Unburned Juniper", "Low Severity Juniper",
                                      "Mod Severity Juniper", "High Severity Juniper"))) %>% 
ggplot(aes(x = years, y = pred, ymin = lwr, ymax = upr)) +
  facet_wrap(~ type) +
  geom_ribbon(fill = "darkseagreen") +
  geom_line() +
  #coord_cartesian(ylim = c(0.4, 0.52)) +
  xlab("Time Since Fire (years)") +
  ylab("Probability of Foraging") +
  theme_bw()

ggsave("9-panel_fig_wunburned.tiff", width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")

# ... NDVI predictions ----

dat %>% 
group_by(animalid) %>% 
tally() %>% 
  View()

