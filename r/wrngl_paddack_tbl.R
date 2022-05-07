#merge script to combine spp lists
#both from paddack refs
# w/ trophic assignments (herbs, pisci, carns, etc)
# w/ habitat specializations (generalist v specialist)

##JHmoxley, Apr 22

library(tidyverse)
library(here)

#combine habitat specialization w/ trophic assingments
paddack <- read_csv(here('data', 'lkup_trophic_paddack.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(sci_name = str_trim(paste(genus, species, " "))) %>% 
  #get habitat specialization
  merge(read_csv(here('data', 'lkup_habUse_paddack.csv')) %>% 
          janitor::clean_names() %>% 
          mutate(sci_name = str_trim(paste(genus, species, " "))) %>% 
          select(sci_name, habitat_use), #assumed same as other paddack study %>%,
        by = "sci_name", all.x = T)
paddack %>% write_csv(here('data', 'lkup_paddack_all.csv'))

trophic_dB <- paddack %>%  
  #spread vars
  bind_cols(paddack %>% 
              spread(trophic_group, trophic_group) %>% 
              select(Carnivore, Herbivore, Invertivore, 
                     Omnivore, Piscivore, Planktivore)) %>% 
  bind_cols(paddack %>% 
              spread(fishing_status, fishing_status) %>% 
              select(fished = Fished, not_fished = Not)) %>%
  bind_cols(paddack %>% 
              spread(habitat_use, habitat_use) %>% 
              select(generalist = Generalist, 
                     specialist = Specialist)) %>% 
  #binarize values
  gather(var, val, -c(sci_name, family, genus, species, 
         trophic_group, max_length, fishing_status, habitat_use)) %>% 
  mutate(var = tolower(var),
    val = ifelse(!is.na(val), 1, NA)) %>% 
  #return to wide format
  spread(var, val) 


  # #get diaz fxn traits
  # merge(read_csv(here('data', 'lkup_fxntrait_diaz.csv')) %>% 
  #     janitor::clean_names() %>% 
  #     mutate(val = 1), 
  #   by.x = "sci_name", by.y = "species", all.x = T) %>% view

sc <- 
  paddack %>% mutate(sci_name = tolower(sci_name)) %>% 
  #2 missing matches
  merge(diaz %>% mutate(species = tolower(species)), 
        by.x = "sci_name", by.y = "species", all.x = T)
diaz <- read_csv(here('data', 'lkup_fxntrait_diaz.csv')) %>% 
      janitor::clean_names() %>%
      mutate(val = 1)

#troubleshooting speices that don't match to paddack dataset  
sc3 <- diaz %>% filter(!(tolower(species) %in% tolower(paddack$sci_name)))

sc <- read_csv(here('data', 'lkup_fxntrait_diaz.csv')) %>%
  janitor::clean_names() %>% 
  mutate(val = 1) %>% view

codes <- read_csv(here('data', 'lkup_trait_codes.csv')) %>% 
  janitor::clean_names()
#write_csv(paddack, here('data', 'lkup_paddack_all.csv'))

