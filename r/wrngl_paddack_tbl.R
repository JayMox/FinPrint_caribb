#merge script to combine spp lists
#both from paddack refs
# w/ trophic assignments (herbs, pisci, carns, etc)
# w/ habitat specializations (generalist v specialist)

##JHmoxley, Apr 22

library(tidyverse)

trophic <- read_csv(here('data', 'lkup_trophic_paddack.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(sci_name = str_trim(paste(genus, species, " ")))
hab <- read_csv(here('data', 'lkup_habUse_paddack.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(sci_name = str_trim(paste(genus, species, " "))) %>% 
  select(-fishing_status) #assumed same as other paddack study

paddack <- trophic %>% 
  merge(hab %>% 
          select(sci_name, habitat_use),
          by = "sci_name", all.x = T)

write_csv(paddack, here('data', 'lkup_paddack_all.csv'))

