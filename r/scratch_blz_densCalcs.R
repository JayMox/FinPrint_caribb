#data wrangling of belizean raw data
#detected various errors in fish_species_belize;
#identical density calcs for all spp in 2017
#calc'ed at transect, not site scale

#building new script to calc my own density estimates, 
#& replace prior trophic level summaries 
#JHMoxley Dec 2021

library(tidyverse)
library(janitor)
library(here)
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

##########
##READ IN
##########
blz <- read_csv(here('data', 'scratch_Belize_masterCounts.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(reserve = str_extract(src, "[:alpha:]{4,5}"), 
         scientific_name = str_replace(scientific_name, "_", " ")) %>% 
  #clean up site entry error (GU into GUZ)
  mutate(site = ifelse(!str_detect(site, "GU(?!Z)"), site, 
                       str_replace(site, "GU(?!Z)", "GUZ"))) %>% 
  filter(species != "N/A")

#read in ancillary data types for merges
tg.lkup <- read_csv(here('data', 'lkup_trophic_paddack.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(scispp = str_trim(paste(genus, species, " ")),
         tg = str_to_lower(trophic_group)) %>% 
  select(family, tg, fishing_status, scispp)
transects <- read_csv(here('data', 'lkup_belize_areaStandardization.csv')) %>%
  janitor::clean_names() %>% 
  mutate(reserve = str_extract(src, "[:alpha:]{4,5}"))
sites.assignment <- read_csv(here('data','lkup_belize_samplingSites.csv')) %>% 
  mutate(uvc_id = paste(reserve, loc, finprint_id, sep="_"))
##########
##########

##########
##APPEND
##########
blz <- blz %>% 
  #add tg designations & fishing status (misisng max size)
  merge(
    sites.assignment %>% 
      select(reserve, site, uvc_id, ruth), 
    by = c("reserve", "site"), all.x = T
) %>% 
  merge(
  tg.lkup %>% select(-family), 
  by.x = "scientific_name", by.y = "scispp", 
  all.x = T
) %>% 
  #add number of transects surveyed per site
  merge(
  transects %>% slice(-1) %>% select(-country, -src, -n_tr, -uvc_tube), 
  by = c('year', 'reserve', 'site'), all.x = T
) %>% 
  mutate(tr_surveyed = ifelse(!is.na(tr_surveyed), tr_surveyed, 8), 
         uvc_id = paste(uvc_id, year, sep="_"))

##########
##CALC
##########
##Get area surveyed by site/year
area <- sites.assignment %>% select(-ruth, -X10, -X11, -note) %>% 
  gather(yr, site_sampled, -reserve, -site, -loc, -finprint_id, -uvc_id) %>%
  mutate(yr = str_extract(yr, "[:digit:]{4}"), 
         uvc_id = paste(uvc_id, yr, sep="_")) %>%
  #remove unsampled sites
  filter(site_sampled!=F) %>% 
  #merge with tr per site
  merge(transects %>% select(year, site, reserve, tr_surveyed), 
        by.x = c("reserve", "site", "yr"), by.y= c("reserve", "site", "year"), 
        all.x = T) %>% 
  mutate(tr_surveyed = ifelse(!is.na(tr_surveyed), tr_surveyed, 8), 
         area_surveyed = tr_surveyed*60)

#store data for assimilation with other data streams


###############
##EXPLORATORY OF ESTS
###############
#at tg scale
dens.tg <- blz %>% group_by(uvc_id, tg) %>% 
  summarize(n.obs = sum(number_individuals, na.rm = T), 
            year = unique(year), reserve = unique(reserve)) %>% 
  merge(area %>% select(uvc_id, yr, area_surveyed) %>% 
          group_by(uvc_id) %>% 
          summarize(tot_area_surveyed = sum(area_surveyed, na.rm = T)), 
        by = "uvc_id") %>% 
  mutate(dens = n.obs/tot_area_surveyed)
  
dens.spp <- blz %>% group_by(uvc_id, scientific_name) %>% 
  summarize(n.obs = sum(number_individuals, na.rm = T), 
            year = unique(year), reserve = unique(reserve)) %>% 
  merge(area %>% select(uvc_id, yr, area_surveyed) %>% 
          group_by(uvc_id) %>% 
          summarize(tot_area_surveyed = sum(area_surveyed, na.rm = T)), 
        by = "uvc_id") %>% 
  mutate(dens = n.obs/tot_area_surveyed)

#####
##write out density estimates
#####
dens.tg %>% 
  write_csv(file = here("data", "Fish_TG_jhmDensEst_Belize.csv"))
dens.spp %>% 
  write_csv(file = here("data", "Fish_Species_SPP_jhmDensEst_Belize.csv"))

#compare w/ ruth estimates
dens.tg %>% filter(!is.na(tg)) %>% 
  select(uvc_id, tg, dens) %>% 
  #spread(tg, dens) %>% 
  mutate(obs = "jerr") %>% 
  bind_rows(read_csv("/Users/jhmoxley/Documents/Biologia & Animales/[[Consulting]]/Finprint_carib/data/summStats_tbl_3.csv") %>% 
  janitor::clean_names() %>% 
  filter(location_name == "Belize") %>% 
  bind_cols(reserve = c("GRMR", "GRMR", "GRMR", "GRMR", "SWCMR", "GRMR")) %>% 
  mutate(uvc_id = paste(reserve, reef_id, sep = "_"), 
         obs = "ruth") %>% 
  select(uvc_id,
         #reserve, trip_year, 
         contains('vore'), obs) %>% 
  gather(tg, dens, -uvc_id, -obs) %>% 
  select(uvc_id, tg, dens, obs)) %>% 
  arrange(uvc_id) %>% 
  ggplot() + geom_col(aes(x = uvc_id, y = dens, col = obs))+
  facet_wrap(~tg, scales = "free_x") + coord_flip() +
  labs(title = "underestimates of belizean data")
  
#write_csv(dens.tg, file = here("data", "blz_jhm_estimates.csv"))

#compare dens.tg to ruth ests
dens.tg %>% 
  filter(!is.na(tg)) %>% 
  mutate(reef_id = str_extract(uvc_id, "[:digit:]{2,3}_[:digit:]{4}"), 
         obs = "jerry", 
         tg = paste0(tg, "s")) %>% 
  mutate(reef_id = ifelse(reef_id == "43_2015", "43_2016", reef_id),
         reef_id = ifelse(reef_id == "50_2018", "50_2017", reef_id),
         reef_id = ifelse(reef_id == "510_2015", "510_2017", reef_id)) %>% 
  select(reef_id, tg, dens, obs) %>% 
  #merge w/ ruth ests
  bind_rows(read_csv(here("data", "summStats_tbl_3.csv")) %>% 
      janitor::clean_names() %>% 
      filter(location_name == "Belize") %>% 
      select(reef_id, contains('vore')) %>% 
      gather(tg, dens, -reef_id) %>% 
      mutate(obs = "ruth")
  ) %>% 
  spread(obs, dens) %>% 
  ggplot() + geom_point(aes(x = ruth, y = jerry, col = tg, shape = reef_id))+
    geom_abline(aes(intercept = 0, slope = 1)) + 
  labs(title = "belize underEsts of density")
ggsave(here("figs", "eda_pwBlz_ruthVjhm.png"))
