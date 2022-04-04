#wrngl pr data
#jhmoxley Mar 2022

library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))
ctry <- "Puerto Rico"
message(paste("sourcing raw uvc counts from", ctry))

src <- c('src_PR_fish_2016.csv'
       ##  'src_PR_fish_2019.csv',
         )
#Currently leaving 2019 on the table
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

#sweep in src data
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j])
           # date = lubridate::ymd(paste(year, month, day)),
           # site.reef = as.factor(primary_sample_unit),
           # site.reefcode = paste(primary_sample_unit, month, date, sep = "_"),
           # site.zone = paste(subregion_nr, tolower(habitat_cd), "_"),
           # n.obs = NA, eff.nsites = NA, eff.nsrveyed = NA, eff.pue = NA
    ) %>% 
    select(country, src, year, month, day,
           #site.reef, site.reefcode, site.zone,
           primary_sample_unit, 
           station_nr, #2 radials per primary sampling
           habitat_cd, 
           lat = lat_degrees, lon = lon_degrees,
           num,
           #sci.name = species_nr, #taken from lkup tbl outside loop
           species_nr, species_cd
           #n.obs, eff.nsites, eff.nsrveyed, eff.pue
    )
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}

#######
##effort
#######
effort <- raw %>% 
  mutate(date = lubridate::ymd(paste(year, month, day)),
         #SUBJECT TO CHANGE BASED ON SAMPLING HIERACHARHy
         site.reef = as.factor(primary_sample_unit),
         site.reefcode = paste(primary_sample_unit, date, sep = "_"),
         site.zone = paste("habCd", tolower(habitat_cd), sep = "_"),
         transect = paste(station_nr, site.reefcode, sep = "_"),
         n.obs = NA, 
         #transect = station_nr,
         #############
         #THESE NEED TO BE WRANGLED SENSIBLY
         ###############
         eff.nsites = NA, eff.nsrvyed = NA, eff.pue = NA,
         stitch.in = src) %>%
  group_by(site.zone, site.reef) %>% #APR22, Grouping by habitat code, NEEDS IMPROVEMENT
  summarize(
    srvy.type = "uvc.f", srvy.method = "radial", srvy.taxa = "fish", 
    #bruv assignment params
    d2bruv = NA,fpid = NA, 
    ####
    ##grouping on 'site.zone' SR/hab codes
    eff.nsites = length(unique(site.reefcode)), #PSU_YYYY-MM-DD
    eff.nsrvyed = length(unique(site.reefcode)) *1, #1 SSUs per site
    #############
    #THESE NEED TO BE WRANGLED SENSIBLY
    ###############
    site.reefcode = paste(primary_sample_unit, date, sep = "_"), 
    #effort
    eff.pue = 177, eff.unit = "m2",  #15 m cylinder 
    stitch.ed = NA, 
    stitch.out = paste("src", stitch.in, sep = "_"), 
    dat.partner = "NOAA"
  ) %>% 
  #mutate(site.reef = site.zone) %>% 
  distinct()

######
##data
######
df <- raw %>% ungroup() %>% 
  mutate(date = lubridate::ymd(paste(year, month, day)),
         #site.reef = as.factor(primary_sample_unit),
         #site.reefcode = paste(primary_sample_unit, date, sep = "_"),
         site.zone = paste("habCd", tolower(habitat_cd), sep = "_"),
         #transect = paste(station_nr, site.reefcode, sep = "_"),
         #n.obs = NA, eff.nsites = NA, eff.nsrvyed = NA, eff.pue = NA) %>% 
  ) %>% 
  merge(effort %>% 
          select(contains('site'), contains('eff')), 
        by = 'site.zone', all.x = T) %>% 
  merge(read_csv(here('data', 'lkup_fla_sppcodes.csv')) %>% 
          janitor::clean_names() %>% 
          select(species_cd, sciname), 
        all.x = T, by = "species_cd") %>% 
  mutate(transect = paste(station_nr, site.reefcode, sep = "_")) %>% 
  select(country, src, year, #date, 
         sci.name = sciname, spp = species_cd,
         count = num, 
         transect,
         starts_with("site."), 
         lat, lon,
         #n.obs, 
         starts_with("eff."))



out <- list('fish.pr' = df, 'uvc.f.effort.pr' = effort)
rm(list = c('dat', 'df', 'effort', 'raw'))
return(out)
