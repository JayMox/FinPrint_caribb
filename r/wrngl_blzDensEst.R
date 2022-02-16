#formal script for wrangling belizean data into database form
#raw data must be ingested, assinged to trophic groups (this is done in scratch_blz_densCalcs), 
#standardized for area sampled in each site, 
#aggregated to BRUV sampling sites, & then density estimated

#JHMoxley, Jan 2022

require(tidyverse)
require(janitor)
require(here)
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

blz <- read_csv(here('data', 'scratch_Belize_masterCounts.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(reserve = str_extract(src, "[:alpha:]{4,5}"), 
         scientific_name = str_replace(scientific_name, "_", " ")) %>% 
  #clean up site entry error (GU into GUZ)
  mutate(site = ifelse(!str_detect(site, "GU(?!Z)"), site, 
                       str_replace(site, "GU(?!Z)", "GUZ")))

