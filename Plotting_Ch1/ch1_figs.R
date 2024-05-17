# Chapter 1 Figures
# Megan Whetzel and Brian Smith
# 2023-03-29

# Load packages ----
library(dplyr)
library(ggplot2)
library(NatParksPalettes)
library(mgcv)
library(stringr)
library(lubridate)
library(suncalc)
library(patchwork)
library(broom)

# Load custom scripts ----
source("99_fun_update.R")

# Color palette ----
natparks.pals(name="BryceCanyon",n = 5, type = "discrete")
col <- as.vector(natparks.pals("BryceCanyon", 5))

# Load fitted models ----
# gam_null <- readRDS("Model_scripts_outputs/betar_gam_results/gam_null_20230215.rds")
gam_fire <- readRDS("Model_scripts_outputs/betar_gam_results/gam_bam_fire_20230301.rds")
gam_combo <- readRDS("gam_combo_20230314.rds")

# Load scaling data ----
scale_df <- read.csv("scaling_data.csv")
# Use "name" column as row.names
row.names(scale_df) <- scale_df$name

# ... Veg categories and burn severity ----
# Create a 16-panel figure, which crosses all the veg categories and the fire
# severity categories
# "newdata" argument for prediction
nd <- pred_data(scale_df, # add in all ndvi and cover variables
                daydiff.log = log(seq(0.1, 30, length.out = 500) * 365.25),
                veg_type = c("aspen", "conifer", "juniper"),
                burn_type = c("l", "m", "h", "o") # 4 dimensions
) %>% 
  # Convert 'daydiff.log' to years
  # Units are currently log(days)
  mutate(years = exp(daydiff.log) / 365.25) %>% 
  # Create the dummy variables
  dummy_vars() %>% 
  # Create the interactions
  intxn() %>% 
  
  ## need to have something here I think to add in NDVI effect on a plot
  # foraging probability is function of ndvi, severity, and veg type
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
    veg_type == "juniper" & burn_type == "h" ~ "High Severity Juniper"#,
    # veg_type == "other" & burn_type == "u" ~ "Unburned Other",
    # veg_type == "other" & burn_type == "l" ~ "Low Severity Other",
    # veg_type == "other" & burn_type == "m" ~ "Mod Severity Other",
    # veg_type == "other" & burn_type == "h" ~ "High Severity Other",
    
    
    
  ))  #%>% 
# scale_data(scale_df)


pred <- predict(gam_fire, newdata = nd, type = "link", se.fit = TRUE,
                exclude = c("s(animalid)", 
                            "s(log_dawn,log_dusk)",
                            "s(julian.day)",
                            "s(Evnt_ID)"))

nd$pred <- plogis(pred$fit)
nd$lwr <- plogis(pred$fit - 1.96 * pred$se.fit)
nd$upr <- plogis(pred$fit + 1.96 * pred$se.fit)

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

# Where is the maximum probability of foraging?
max_forage <- plot.data %>% 
  group_by(habitat, severity) %>% 
  filter(pred == max(pred)) %>% 
  select(habitat, severity, max = years, max_pred = pred)

plot.data <- plot.data %>% 
  left_join(max_forage)

plot9 <- plot.data %>%
  mutate(severity = factor(severity, levels = c("Low", 
                                                "Moderate", "High"))) %>% 
  ggplot(aes(x = years, y = pred)) +
  facet_grid(habitat ~ severity) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = col[4], alpha = 0.75) +
  geom_line(linewidth = 0.7) +
  #  geom_ribbon(aes(ymin = lowerUB, ymax = upperUB), fill = 'gray64', alpha = 0.5) +
  geom_line(aes(y = predUB, years), linetype = "longdash", size = 0.7) +
  # Draw point for max Pr(forage)
  geom_point(aes(x = max, y = max_pred), color = col[2]) +
  # Or instead, draw a vertical line
  geom_vline(aes(xintercept = max), color = col[2], 
             linetype = "dashed", linewidth = 0.7) +
  #  geom_line() +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
#  scale_y_continuous(limits = c(0.3,0.5)) + 
  # coord_cartesian(ylim = c(0, 1)) +
  xlab("Time Since Fire (years)") +
  ylab("Probability of Foraging") +
  theme_bw()

ggsave("9-panel_fig_reference_black.tiff", 
       width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")

# ... temporal patterns ----

# ... ... panel A -- Day of Year ----
nd_temp_a <- pred_data(scale_df, # add in all ndvi and cover variables
                       julian.day = 1:366
) %>% 
  # Convert 'julian.day' to a date
  # Use 2016 b/c it was a leap year
  mutate(date = jd_2_date(julian.day)) %>% 
  # Create the dummy variables
  dummy_vars() %>% 
  # Create the interactions
  intxn() %>% 
  
  ## need to have something here I think to add in NDVI effect on a plot
  # foraging probability is function of ndvi, severity, and veg type
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
    veg_type == "juniper" & burn_type == "h" ~ "High Severity Juniper"#,
    # veg_type == "other" & burn_type == "u" ~ "Unburned Other",
    # veg_type == "other" & burn_type == "l" ~ "Low Severity Other",
    # veg_type == "other" & burn_type == "m" ~ "Mod Severity Other",
    # veg_type == "other" & burn_type == "h" ~ "High Severity Other",
    
    
    
  ))  #%>% 
# scale_data(scale_df)

# Predict
pred_temp_a <- predict(gam_fire, newdata = nd_temp_a, type = "link",
                       se.fit = TRUE, exclude = c("s(animalid)", 
                                                  "s(log_dawn,log_dusk)",
                                                  "s(Evnt_ID)"))

nd_temp_a$pred <- plogis(pred_temp_a$fit)
nd_temp_a$lwr <- plogis(pred_temp_a$fit - 1.96 * pred_temp_a$se.fit)
nd_temp_a$upr <- plogis(pred_temp_a$fit + 1.96 * pred_temp_a$se.fit)

# Plot
temp_panel_a <- ggplot(nd_temp_a, aes(x = date, y = pred, 
                                      ymin = lwr, ymax =  upr)) +
  geom_ribbon(fill = col[1], alpha = 0.5) +
  geom_line(linewidth = 0.3, color = "black") +
  scale_x_date(date_breaks = "month", date_labels = "%B") +
  xlab("Month") +
  ylab("Probability of Foraging") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ... ... panel B -- Time of Day ----
# Start setup of newdata
nd_temp_b <- pred_data(scale_df, # add in all ndvi and cover variables
                       # This is just a placeholder so we have 100 values
                       log_dawn = 1:100
) %>% 
  # Create 100 times of day to determine log_dawn and log_dusk
  mutate(time = seq(from = ymd_hms("2016-04-25 00:00:00", tz = "US/Mountain"),
                    to = ymd_hms("2016-04-25 23:59:59", tz = "US/Mountain"),
                    length.out = 100))

# Determine dawn/dusk times
dd <- getSunlightTimes(date = as.Date("2016-04-25"),
                       lat = 39.8643545,
                       lon = -111.2755843,
                       keep = c("dawn", "dusk"),
                       tz = "US/Mountain")

# Attach time to dawn, time to dusk
nd_temp_b <- nd_temp_b %>% 
  mutate(tt_dawn = abs(as.numeric(time - dd$dawn, units = "hours")),
         tt_dusk = abs(as.numeric(time - dd$dusk, units = "hours"))) %>% 
  # Make sure we got the "closest" dawn or dusk
  # Subtract from 24 if > 12
  mutate(closest_dawn = case_when(
    tt_dawn > 12 ~ 24 - tt_dawn,
    TRUE ~ tt_dawn
  ),
  closest_dusk = case_when(
    tt_dusk > 12 ~ 24 - tt_dusk,
    TRUE ~ tt_dusk
  )
  ) %>% 
  # Take log
  mutate(log_dawn = log(closest_dawn),
         log_dusk = log(closest_dusk)) %>% 
  # Create the dummy variables
  dummy_vars() %>% 
  # Create the interactions
  intxn() %>% 
  
  ## need to have something here I think to add in NDVI effect on a plot
  # foraging probability is function of ndvi, severity, and veg type
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
    veg_type == "juniper" & burn_type == "h" ~ "High Severity Juniper"#,
    # veg_type == "other" & burn_type == "u" ~ "Unburned Other",
    # veg_type == "other" & burn_type == "l" ~ "Low Severity Other",
    # veg_type == "other" & burn_type == "m" ~ "Mod Severity Other",
    # veg_type == "other" & burn_type == "h" ~ "High Severity Other",
    
    
    
  ))  #%>% 
# scale_data(scale_df)

# Predict
pred_temp_b <- predict(gam_fire, newdata = nd_temp_b, type = "link",
                       se.fit = TRUE, exclude = c("s(animalid)", 
                                                  "s(julian.day)",
                                                  "s(Evnt_ID)"))

nd_temp_b$pred <- plogis(pred_temp_b$fit)
nd_temp_b$lwr <- plogis(pred_temp_b$fit - 1.96 * pred_temp_b$se.fit)
nd_temp_b$upr <- plogis(pred_temp_b$fit + 1.96 * pred_temp_b$se.fit)

# Plot
temp_panel_b <- ggplot(nd_temp_b, aes(x = time, y = pred, 
                                      ymin = lwr, ymax =  upr)) +
  geom_ribbon(fill = col[1], alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_x_datetime(date_labels = "%H:%M") +
  xlab("Time of Day") +
  ylab("Probability of Foraging") +
  theme_bw()

# ... ... combine ----
temp_plot <- (temp_panel_a +
                coord_cartesian(ylim = c(0.7, 0.85))) +
  (temp_panel_b +
     coord_cartesian(ylim = c(0.6, 0.85))) +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")

ggsave("temporal_pattern_update.tiff", plot = temp_plot,
       width = 200, height = 130, units = "mm",
       dpi = 600, compression = "lzw")

# ... betas ----
beta_data <- tidy(gam_fire, parametric = TRUE) %>% 
  mutate(lwr = estimate + qnorm(0.025) * std.error,
         upr = estimate + qnorm(0.975) * std.error)

key <- data.frame(term = beta_data$term) %>% 
  mutate(include = !(str_detect(term, "unburned.in") | 
                       str_detect(term, "is.other"))) %>% 
  # filter(include) %>% 
  mutate(category = case_when(
    str_detect(term, fixed("daydiff.log^2")) ~ "Time Since Fire (Quadratic)",
    str_detect(term, "daydiff.log") ~ "Time Since Fire (Linear)",
    TRUE ~ "Habitat"
  ))

parms <- key %>% 
  filter(include) %>% 
  select(term) %>% 
  mutate(habitat = case_when(str_detect(term, "aspen") ~ "Aspen",
                             str_detect(term, "conifer") ~ "Conifer",
                             str_detect(term, "juniper") ~ "Juniper"),
         severity = case_when(str_detect(term, "unburned") ~ "Unburned",
                              str_detect(term, fixed(".1")) ~ "Low",
                              str_detect(term, fixed(".2")) ~ "Moderate",
                              str_detect(term, fixed(".3")) ~ "High")) %>% 
  mutate(label = paste(severity, habitat))

# Fix top two
parms$label[1:2] <- c("Prior Foraging", "Snow Depth")

# Format labels as factor to control plotting order
levs <- expand.grid(b = c("Aspen", "Conifer", "Juniper"),
                    a = c("Unburned", "Low", "Moderate", "High")) %>% 
  mutate(level = paste(a, b)) %>% 
  pull(level)

# Add on Snow Depth and Prior Foraging
levs <- c(levs, "Prior Foraging", "Snow Depth")
    
parms$label <- factor(parms$label, levels = levs)     

beta_plot_a <- beta_data %>% 
  left_join(key) %>% 
  left_join(parms) %>% 
  filter(include) %>% 
  filter(category == "Habitat") %>% 
  ggplot(aes(x = estimate, y = label, xmin = lwr, xmax = upr)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_vline(xintercept = 0, color = col[2], linetype = "dashed") +
  xlab(expression(beta)) +
  ylab(NULL) +
  ggtitle("Habitat") +
  theme_bw()

beta_plot_b <- beta_data %>% 
  left_join(key) %>% 
  left_join(parms) %>% 
  filter(include) %>% 
  filter(category == "Time Since Fire (Linear)") %>% 
  ggplot(aes(x = estimate, y = label, xmin = lwr, xmax = upr)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_vline(xintercept = 0, color = col[2], linetype = "dashed") +
  xlab(expression(beta)) +
  ylab(NULL) +
  ggtitle("Time Since Fire (Linear)") +
  theme_bw()

beta_plot_c <- beta_data %>% 
  left_join(key) %>% 
  left_join(parms) %>% 
  filter(include) %>% 
  filter(category == "Time Since Fire (Quadratic)") %>% 
  ggplot(aes(x = estimate, y = label, xmin = lwr, xmax = upr)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_vline(xintercept = 0, color = col[2], linetype = "dashed") +
  xlab(expression(beta)) +
  ylab(NULL) +
  ggtitle("Time Since Fire (Quadratic)") +
  theme_bw()

beta_plot <- beta_plot_a +
  beta_plot_b +
  beta_plot_c +
  plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")

ggsave("betas.tiff", plot = beta_plot, 
       width = 180, height = 100, units = "mm",
       dpi = 300, compression = "lzw", scale = 1.6)
