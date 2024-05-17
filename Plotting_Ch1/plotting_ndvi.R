# Load packages ----
library(mgcv)
library(tidyverse)
library(ggplot2)
library(NatParksPalettes)

# Load custom scripts ----
source("99_fun.R")

# Load fitted models ----
# gam_null <- readRDS("Model_scripts_outputs/betar_gam_results/gam_null_20230215.rds")
gam_fire <- readRDS("Model_scripts_outputs/betar_gam_results/gam_bam_fire_20230301.rds")
gam_combo <- readRDS("Model_scripts_outputs/betar_gam_results/gam_combo_20230314.rds")


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
                burn_type = c("l", "m", "h", "o"),
                cover.herbaceous_NDVI.norm.curr = ,
                cover.herbaceous_NDVI.delta = ,
                cover.shrub_NDVI.norm.curr = ,
                cover.shrub_NDVI.delta  = ,
 #need to put NDVI in nd but not sure how to denote that
 
) %>% 
  # Create the dummy variables
  dummy_vars() %>% 
  # Create the interactions
  intxn() %>% 
  # Give each veg-type/burn-type combo a name
  mutate(type = case_when(
    #  veg_type == "aspen" & burn_type == "u" ~ "Unburned Aspen",
    veg_type == "aspen" & burn_type == "o" ~ "Unburned Aspen",
    veg_type == "aspen" & burn_type == "l" ~ "Low Severity Aspen",
    veg_type == "aspen" & burn_type == "m" ~ "Moderate Severity Aspen",
    veg_type == "aspen" & burn_type == "h" ~ "High Severity Aspen",
    #  veg_type == "conifer" & burn_type == "u" ~ "Unburned Conifer",
    veg_type == "conifer" & burn_type == "o" ~ "Unburned Conifer",
    veg_type == "conifer" & burn_type == "l" ~ "Low Severity Conifer",
    veg_type == "conifer" & burn_type == "m" ~ "Moderate Severity Conifer",
    veg_type == "conifer" & burn_type == "h" ~ "High Severity Conifer",
    # veg_type == "juniper" & burn_type == "u" ~ "Unburned Juniper",
    veg_type == "juniper" & burn_type == "o" ~ "Unburned Juniper",
    veg_type == "juniper" & burn_type == "l" ~ "Low Severity Juniper",
    veg_type == "juniper" & burn_type == "m" ~ "Moderate Severity Juniper",
    veg_type == "juniper" & burn_type == "h" ~ "High Severity Juniper",
    # veg_type == "other" & burn_type == "u" ~ "Unburned Other",
    # veg_type == "other" & burn_type == "l" ~ "Low Severity Other",
    # veg_type == "other" & burn_type == "m" ~ "Mod Severity Other",
    # veg_type == "other" & burn_type == "h" ~ "High Severity Other"
  ))

# Convert 'daydiff.log' to years
# Units are currently log(days)
nd$years <- exp(nd$daydiff.log) / 365.25

# ... NDVI predictions ----
pred <- predict(gam_combo, newdata = nd, type = "response", se.fit = TRUE,
                exclude = c("s(animalid)", 
                            "s(log_dawn,log_dusk)",
                            "s(julian.day)",
                            "s(Evnt_ID)"))

nd$pred <- pred$fit
nd$lwr <- pred$fit - 1.96 * pred$se.fit
nd$upr <- pred$fit + 1.96 * pred$se.fit

#### Help from Soren to get unburned line as reference on all graphs ----
plot.data <- nd %>% 
  mutate(type = factor(type, levels = c("Unburned Aspen", 
                                        "Low Severity Aspen", 
                                        "Moderate Severity Aspen",
                                        "High Severity Aspen", 
                                        "Unburned Conifer",
                                        "Low Severity Conifer",
                                        "Moderate Severity Conifer", 
                                        "High Severity Conifer",
                                        "Unburned Juniper",
                                        "Low Severity Juniper", 
                                        "Moderate Severity Juniper",
                                        "High Severity Juniper"))) %>%
  select(years, pred, lwr, upr, type) %>% 
  mutate(habitat = case_when(str_detect(type, pattern='Aspen') ~ 'Aspen',
                             str_detect(type, pattern='Conifer') ~ 'Conifer',
                             str_detect(type, pattern='Juniper') ~ 'Juniper'),
         severity = case_when(str_detect(type, pattern='Unburned') ~ 'Unburned',
                              str_detect(type, pattern='Low') ~ 'Low',
                              str_detect(type, pattern='Moderate') ~ 'Moderate',
                              str_detect(type, pattern='High') ~ 'High'),
         UB = if_else(severity=='Unburned',T,F))

tmp <- plot.data %>% 
  filter(severity == "Unburned") %>% 
  select(-c(years, UB, type, severity)) %>%
  distinct() %>%
  rename(predUB = pred,
         lowerUB = lwr,
         upperUB = upr)

plot.data <- plot.data %>%
  filter(severity != "Unburned") %>%
  left_join(tmp, by = 'habitat')

plot.data %>%
  mutate(severity = factor(severity, levels = c("Low", 
                                                "Moderate", "High"))) %>% 
  ggplot(aes(x = years, y = pred)) +
  facet_grid(habitat ~ severity) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "slateblue1", alpha = 0.75) +
  geom_line(size = 0.7) +
  #  geom_ribbon(aes(ymin = lowerUB, ymax = upperUB), fill = 'gray64', alpha = 0.5) +
  geom_line(aes(y = predUB, years), linetype = "longdash", size = 0.7) +
  #  geom_line() +
  scale_x_continuous(limits = c(1,30)) +
  #scale_y_continuous(limits = c(0.4,0.55)) + 
  # coord_cartesian(ylim = c(0, 1)) +
  xlab("Time Since Fire (years)") +
  ylab("Probability of Foraging") +
  theme_bw()

ggsave("9-panel_fig_reference_20230320.tiff", width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")


# Brian's code for plotting ----
nd %>% 
  mutate(type = factor(type, levels = c("Unburned Aspen", "Low Severity Aspen",
                                        "Moderate Severity Aspen", "High Severity Aspen",
                                        "Unburned Conifer", "Low Severity Conifer",
                                        "Moderate Severity Conifer", "High Severity Conifer",
                                        "Unburned Juniper", "Low Severity Juniper",
                                        "Moderate Severity Juniper", "High Severity Juniper"))) %>% 
  ggplot(aes(x = years, y = pred, ymin = lwr, ymax = upr)) +
  facet_wrap(~ type) +
  geom_ribbon(fill = "darkseagreen") +
  geom_line() +
  #coord_cartesian(ylim = c(0.4, 0.52)) +
  xlab("Time Since Fire (years)") +
  ylab("Probability of Foraging") +
  theme_bw()



ggsave("9-panel_fig_reference.tiff", width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")


# plot.data = nd %>% 
#   mutate(type = factor(type, levels = c("Unburned Aspen", 
#                                         "Low Severity Aspen", 
#                                         "Moderate Severity Aspen",
#                                         "High Severity Aspen", 
#                                         "Unburned Conifer",
#                                         "Low Severity Conifer",
#                                         "Moderate Severity Conifer", 
#                                         "High Severity Conifer",
#                                         "Unburned Juniper",
#                                         "Low Severity Juniper", 
#                                         "Moderate Severity Juniper",
#                                         "High Severity Juniper"))) %>%
#   select(years, pred, lwr, upr, type) %>% 
#   mutate(habitat = case_when(str_detect(type, pattern='Aspen') ~ 'Aspen',
#                              str_detect(type, pattern='Conifer') ~ 'Conifer',
#                              str_detect(type, pattern='Juniper') ~ 'Juniper'),
#          severity = case_when(str_detect(type, pattern='Unburned') ~ 'Unburned',
#                               str_detect(type, pattern='Low') ~ 'Low',
#                               str_detect(type, pattern='Moderate') ~ 'Moderate',
#                               str_detect(type, pattern='High') ~ 'High'),
#          UB = if_else(severity=='Unburned',T,F))






