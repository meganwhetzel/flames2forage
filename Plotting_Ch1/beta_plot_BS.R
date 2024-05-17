library(dplyr)
library(ggplot2)

data.frame(x = 1:3, est = c(0.8, 0.12, 0.5)) %>% 
  mutate(lwr90 = est - 0.1,
         upr90 = est + 0.1,
         lwr95 = est - 0.15,
         upr95 = est + 0.15) %>% 
  ggplot(aes(x = est, y = x)) +
  geom_errorbar(aes(xmin = lwr95, xmax = upr95), col = "gray", 
                linewidth = 0.5, width = 0.2) +
  geom_errorbar(aes(xmin = lwr90, xmax = upr90), col = "black", 
                linewidth = 0.75, width = 0.0) +
  geom_point() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  xlab(expression(beta)) +
  scale_y_continuous(name = "Parameter",
                     breaks = c(1, 2, 3),
                     labels = c("Var1", "Var2", "Var3")) +
  theme_bw()

