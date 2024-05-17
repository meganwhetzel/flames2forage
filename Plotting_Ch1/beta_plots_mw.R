library(dplyr)
library(ggplot2)


gam_fire_betas <- data.frame(x = c("foraging_prior", 
                  "snd",                                    
                #  "unburned.out:is.other",                  
                  "unburned.out:is.juniper",                
                  "unburned.out:is.conifer",                
                  "unburned.out:is.aspen",                  
                  # "is.other:unburned.in",                   
                  # "is.juniper:unburned.in",                 
                  # "is.conifer:unburned.in",                 
                  # "is.aspen:unburned.in",                   
                 # "is.other:burned.1",                      
                  "is.juniper:burned.1",                    
                  "is.conifer:burned.1",                    
                  "is.aspen:burned.1",                      
              #    "is.other:burned.2",                      
                  "is.juniper:burned.2",                    
                  "is.conifer:burned.2",                    
                  "is.aspen:burned.2",                      
               #   "is.other:burned.3",                      
                  "is.juniper:burned.3",                    
                  "is.conifer:burned.3",                    
                  "is.aspen:burned.3",                      
               #   "is.other:unburned.in:daydiff.log",       
               #   "is.juniper:unburned.in:daydiff.log",     
                #  "is.conifer:unburned.in:daydiff.log",     
                #  "is.aspen:unburned.in:daydiff.log",       
                #  "is.other:burned.1:daydiff.log",          
                  "is.juniper:burned.1:daydiff.log",        
                  "is.conifer:burned.1:daydiff.log",       
                  "is.aspen:burned.1:daydiff.log",          
               #   "is.other:burned.2:daydiff.log",          
                  "is.juniper:burned.2:daydiff.log",        
                  "is.conifer:burned.2:daydiff.log",        
                  "is.aspen:burned.2:daydiff.log",          
               #   "is.other:burned.3:daydiff.log",          
                  "is.juniper:burned.3:daydiff.log",        
                  "is.conifer:burned.3:daydiff.log",        
                  "is.aspen:burned.3:daydiff.log",          
              #    "is.other:unburned.in:I(daydiff.log^2)",  
              #    "is.juniper:unburned.in:I(daydiff.log^2)",
              #    "is.conifer:unburned.in:I(daydiff.log^2)",
              #    "is.aspen:unburned.in:I(daydiff.log^2)",  
              #    "is.other:burned.1:I(daydiff.log^2)",     
                  "is.juniper:burned.1:I(daydiff.log^2)",   
                  "is.conifer:burned.1:I(daydiff.log^2)",   
                  "is.aspen:burned.1:I(daydiff.log^2)",     
              #    "is.other:burned.2:I(daydiff.log^2)",     
                  "is.juniper:burned.2:I(daydiff.log^2)",   
                  "is.conifer:burned.2:I(daydiff.log^2)",   
                  "is.aspen:burned.2:I(daydiff.log^2)",     
               #   "is.other:burned.3:I(daydiff.log^2)",     
                  "is.juniper:burned.3:I(daydiff.log^2)",   
                  "is.conifer:burned.3:I(daydiff.log^2)",   
                  "is.aspen:burned.3:I(daydiff.log^2)"), est = c( 2.330126,
                                                                  0.250588,
                                                                 # -1.577828,
                                                                  -1.531068,
                                                                  -1.453530,
                                                                  -1.343133,
                                                                  # -2.897611,
                                                                  # -0.736338,
                                                                  # -3.353267,
                                                                  # -1.232555,
                                                                 # -2.663418,
                                                                  -2.179834,
                                                                  -4.012780,
                                                                  -2.392053,
                                                                #  -4.115474,
                                                                  -4.347492,
                                                                  -4.356791,
                                                                  -2.251598,
                                                                #  -3.442367,
                                                                  -2.101048,
                                                                  -2.963524,
                                                                  -3.342887,
                                                                #  0.375051,
                                                                #  -0.236290,
                                                                #  0.543294,
                                                                #  -0.051339,
                                                                #  0.321703,
                                                                  0.167836,
                                                                  0.745299,
                                                                  0.262133,
                                                                #  0.741503,
                                                                  0.743219,
                                                                  0.819357,
                                                                  0.220474,
                                                                #  0.586610,
                                                                  0.184598,
                                                                  0.442936,
                                                                  0.509099,
                                                                #  -0.025761,
                                                                #  0.017469,
                                                                #  -0.037470,
                                                                #  0.004156,
                                                                #  -0.022614,
                                                                  -0.010425,
                                                                  -0.052293,
                                                                  -0.016608,
                                                                #  -0.051434,
                                                                  -0.048141,
                                                                  -0.056084,
                                                                  -0.013551,
                                                                 # -0.041883,
                                                                  -0.013488,
                                                                  -0.030674,
                                                                  -0.031027),
                  se = c(0.004383,
                  0.011659,
                #  0.026263,
                  0.026336,
                  0.026777,
                  0.026915,
                  #0.281287,
                  #0.243606,
                  #0.699919,
                  #0.590715,
                  #0.195956,
                  0.190805,
                  0.425376,
                  0.401142,
                  #0.260346,
                  0.244806,
                  0.439768,
                  0.438494,
                  #0.451822,
                  0.465166,
                  0.521388,
                  0.585963,
                  #0.080656,
                 # 0.071093,
                 # 0.189649,
                 # 0.158218,
                  #0.055737,
                  0.053803,
                  0.116921,
                  0.108479,
                  #0.072148,
                  0.066058,
                  0.120286,
                  0.118671,
                  #0.124661,
                  0.116877,
                  0.141686,
                  0.160501,
                  #0.005582,
                  #0.004960,
                  #0.012571,
                  #0.010471,
                  #0.003851,
                  0.003708,
                  0.007866,
                  0.007252,
                  #0.004870,
                  0.004391,
                  0.008053,
                  0.007881,
                  #0.008349,
                  0.007336,
                  0.009416,
                  0.010693)) %>% 
  mutate(lwr90 = est + qnorm(0.05) * se,
         upr90 = est + qnorm(0.95) * se,
         lwr95 = est + qnorm(0.025) * se,
         upr95 = est + qnorm(0.975) * se) %>% 
  ggplot(aes(x = est, y = x)) +
  geom_errorbar(aes(xmin = lwr95, xmax = upr95), col = "gray", 
                linewidth = 0.5, width = 0.2) +
  geom_errorbar(aes(xmin = lwr90, xmax = upr90), col = "black", 
                linewidth = 0.75, width = 0.0) +
  geom_point(color = "palegreen4") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  xlab(expression(beta)) +
  ylab("Parameter") +
#  scale_y_continuous(name = "Parameter") +
               #      breaks = c(1, 2, 3),
               #      labels = c("Var1", "Var2", "Var3")) +
  theme_bw()

ggsave("gam_fire_parameters.tiff", width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")   


library(ggmap)
# natparks.pals(name="BryceCanyon",n = 5, type = "discrete")
# col <- as.vector(natparks.pals("BryceCanyon", 5))



# Fixing up HMM tracks
HMM_prepared_dat_20230113 <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_Data/HMM_prepared_data/HMM_prepared_dat_20230113.rds")
HMM_with_viterbi_prob_20230115 <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/Cleaned_Data/HMM_results/HMM_with_viterbi_prob_20230115.rds")


plot <- HMM_with_viterbi_prob_20230115 %>% 
  select(x, y, row_id, fine_state) %>% 
  left_join(HMM_prepared_dat_20230113, by = c("row_id", "x", "y")) 

# Trying to convert utm to lat long, but not working - need for figure and 
# potential ggmap
plot2 <- plot %>% 
  select(x, y)
  
sp_points_utm <- SpatialPoints(plot2, 
                               proj4string = CRS("+proj=utm +zone=12 +datum=WGS84"))

sp_points_latlong <- spTransform(sp_points_utm, 
                                 CRS("+proj=longlat +datum=WGS84"))
lnlt <- as.data.frame(coordinates(sp_points_latlong)) %>% 
  rename(lat = y,
         long = x)

plot3 <- cbind.data.frame(plot, lnlt)


sample <- fine_dat_full %>% 
  filter(ID == "EL21F0002") %>% 
ggplot(aes(x = x, y = y, color = fine_state_new)) +
  geom_point() +
#  geom_path() +
#  scale_color_viridis_d(option = "cividis") +
  scale_color_manual(values = c("resting" = "mediumpurple1", "commuting" = "darkorange1", "foraging" = "mediumseagreen")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "right") +
  labs(x = "Easting", y = "Northing", color = "Behavioral State") +
  ggtitle("Elk EL21F002")



ggsave("HMM_tracks_elk_update.tiff", plot = sample,
       width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")

#elk_map <- get_googlemap(center = c(-111.0937, 39.3210))


# fine_dat_full  <-  HMM_with_viterbi_prob_20230115 %>%
#   select(c(1:11)) %>% 
#   cbind(as.data.frame(fine_state)) %>%
#   mutate(fine_state_new = case_when(fine_state == 1 ~ "resting",
#                                 fine_state == 2 ~ "commuting",
#                                 fine_state == 3 ~ "foraging"))
