#merge script to combine spp lists
#both from paddack refs
# w/ trophic assignments (herbs, pisci, carns, etc)
# w/ habitat specializations (generalist v specialist)

##JHmoxley, Apr 22

library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))

#########
##Trophic Groups & hab specialization
##from paddack refs
#########
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

  
#transform into [1, NA] matrix
trophic_db <- paddack %>%  
  #spread vars
  bind_cols(paddack %>% #adjust field names into info cols
              transform(trophic_group = 
                          codes$field[match(tolower(trophic_group), codes$val)]) %>%
              spread(trophic_group, trophic_group) %>% 
              select(contains("_fg"))) %>% 
  bind_cols(paddack %>% #adjust field names into info cols
              transform(fishing_status = 
                          codes$field[match(tolower(fishing_status), codes$val)]) %>% 
              spread(fishing_status, fishing_status) %>% 
              select(contains("_fished"))) %>%
  bind_cols(paddack %>% #adjust field names into info cols
              transform(habitat_use =
                          codes$field[match(tolower(habitat_use), codes$val)]) %>% 
              spread(habitat_use, habitat_use) %>% 
              select(contains("_habuse"))) %>% 
  #binarize values
  gather(var, val, -c(sci_name, family, genus, species, 
         trophic_group, max_length, fishing_status, habitat_use)) %>% 
  mutate(var = tolower(var),
    val = ifelse(!is.na(val), 1, NA)) %>% 
  #return to wide format
  spread(var, val)
  
  

##########
##Integrate with Fxn Traits
##from Diaz db
#########
diaz <- read_csv(here('data', 'lkup_fxntrait_diaz.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(species = tolower(species)) %>% 
  select(-family)
#make informative cols
colnames(diaz)[-1] <- codes$field[match(names(diaz)[-1], codes$val_code)]

traits_db <- trophic_db %>% 
  mutate(sci_name = tolower(sci_name)) %>% 
  merge(diaz, by.x = "sci_name", by.y = "species", all.x = T)


rm(list = c('diaz', 'paddack'))
return(list(trophic_db, traits_db))