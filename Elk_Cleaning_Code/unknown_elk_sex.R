library(tidyverse)

missing_sex <- clean_flagged_data_30 %>% 
  filter(Sex == "U") %>% 
  group_by(animal_id) %>% 
  tally()



## Look into maptools = crepuscule, sunriset