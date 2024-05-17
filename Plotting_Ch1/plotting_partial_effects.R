library(tidyr)
library(ggplot2)

jd <- smooth_estimates(gam_fire, smooth = "s(julian.day)")

jd %>%
  ggplot(aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), fill = "deepskyblue2") + # or whatever color you want
  geom_line() +  labs(x = "Julian Day", y = "Effect on Foraging Probability")


ggsave("jd_partial_effect.tiff", width = 180, height = 180, units = "mm",
       dpi = 600, compression = "lzw")

ldd <- smooth_estimates(gam_fire, smooth = "s(log_dawn,log_dusk)")

ldd %>% 
  ggplot(aes(x = log_dawn, y = log_dusk)) +
  geom_hex(fill = est) #aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se)) +
  labs(x = "Log(dawn)", y = "Log(dusk)")

ldd_plot <- draw(ldd,
  continuous_fill = scale_fill_distiller(palette = "PuOr")) 

ggsave("ldd_partial_effect.tiff", width = 180, height = 180, units = "mm",
        dpi = 600, compression = "lzw")
  

  