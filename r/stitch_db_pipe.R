##pipeline script for stitching dB from wrngl_raw scripts
##JHMoxley, May 2022

library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))

#sweep-in ancillary data
#bruv <- read_csv(here('data/stitch', 'src_BRUV_westernAtl.csv'))
#srvy <- read_csv(here('data, 'lkup_srvy_params.csv'))

##raw input
blz <- source(here('r', 'wrngl_uvc_belize.R'))
cuba <- source(here('r', 'wrngl_uvc_cuba.R'))
colo <- source(here('r', 'wrngl_uvc_colombia.R'))
caicos <- source(here('r', 'wrngl_uvc_caicos.R'))
fla <- source(here('r', 'wrngl_uvc_florida.R'))
pr <- source(here('r', 'wrngl_uvc_pr.R'))
             

##stich'n'store
dF.f <- data.frame(NULL)
dF.f <- dF.f %>% 
  bind_rows(blz$value$fish.belize) %>% select(all_of(uvc.f.cols)) %>% 
  bind_rows(cuba$value$fish.cuba) %>% select(all_of(uvc.f.cols)) %>%
  bind_rows(colo$value$fish.colo) %>% select(all_of(uvc.f.cols)) %>% 
  bind_rows(caicos$value$fish.caicos) %>% select(all_of(uvc.f.cols)) %>% 
  bind_rows(fla$value$fish.fla) %>% select(all_of(uvc.f.cols)) %>% 
  bind_rows(pr$value$fish.pr) %>% select(all_of(uvc.f.cols)) %>% 
  #standardize fields
  mutate(sci.name = gsub("_", " ", tolower(sci.name))) %>% 
  janitor::clean_names()
  
#augment fields
dF.f <- dF.f %>% 
  #get paddack & standardize
  merge(read_csv(here('data', 'lkup_paddack_all.csv'))%>% 
          janitor::clean_names() %>% 
          mutate(sci_name = tolower(paste(genus,species, sep=" ")),
                 trophic_group = as.factor(trophic_group),
                 family = as.factor(family)) %>% 
          select(sci_name, 
                 family, max_length, 
                 tg_pdk = trophic_group, 
                 fished_pdk = fishing_status, 
                 habspec_pdk = habitat_use), 
        by = "sci_name", all.x = T) 
#10% NA's.. ~0.01% when only considering >0 counts
#387 spp have trophic groupngs

##DIAZ Designations
##165 spp, clustered in 3 groups
##all fields have multiple assignments
##complete info.. n = 95; 6 traits
##majority ifno = n = 38; 5+ traits
##incomplete = n = 32; <5 traits


  
sc <- read_csv(here('data', 'lkup_fxntrait_diaz.csv')) %>%
  janitor::clean_names()
  
traits <- read_csv(here('data', 'lkup_fxntrait_groupings.csv')) %>% 
  janitor::clean_names()

  mutate(tier = ben + dem+ mid + sur + sol + pair + agr + mix + diu + cre + noc + cor + rock + sea + alg + man +san + rub + ope + col + she + ver) %>% 
  gather(trait, bool, 
         -species, -family, -tl, -db, -tier) %>% 
  #trophic level & diet breadth
  #as well as:
  #social feeding behavior = 
  #feeding ground = 
  #time of feeding = 
  #water col feeding position = 
  filter(!is.na(bool))

  filter()
  head()
          
  
