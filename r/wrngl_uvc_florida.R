#wrngl of florida data
#JHMoxley Feb 22 
library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))
ctry <- "Florida"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_flaFish_2016.csv')
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
           primary_sample_unit, subregion_nr, 
           station_nr, #2 radials per primary sampling
           habitat_cd, zone_nr, #zone is quasi depth (inshore, channel, fore, offshore)
           lat = lat_degrees, lon = lon_degrees,
           num,
           #sci.name = species_nr, #taken from lkup tbl outside loop
           species_nr, species_cd
           #n.obs, eff.nsites, eff.nsrveyed, eff.pue
           )
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}

######
##data
######
df <- raw %>% ungroup() %>% 
  mutate(date = lubridate::ymd(paste(year, month, day)),
     site.reef = as.factor(primary_sample_unit),
     site.reefcode = paste(primary_sample_unit, date, sep = "_"),
     site.zone = paste("subreg", subregion_nr, tolower(habitat_cd), sep = "_"),
     n.obs = NA, eff.nsites = NA, eff.nsrveyed = NA, eff.pue = NA) %>% 
  merge(read_csv(here('data', 'lkup_fla_sppcodes.csv')) %>% 
          janitor::clean_names() %>% 
          select(species_cd, sciname), 
        all.x = T, by = "species_cd") %>% 
  select(country, src, year, #date, 
         sci.name = sciname, spp = species_cd,
         count = num, 
         transect = station_nr,
         starts_with("site."), 
         lat, lon,
         n.obs, starts_with("eff."))

effort <- raw %>% 
  mutate(date = lubridate::ymd(paste(year, month, day)),
         #SUBJECT TO CHANGE BASED ON SAMPLING HIERACHARHy
         site.reef = as.factor(primary_sample_unit),
         site.reefcode = paste(primary_sample_unit, date, sep = "_"),
         site.zone = paste("sr", subregion_nr, tolower(habitat_cd), sep = "_"),
         n.obs = NA, 
         #############
         #THESE NEED TO BE WRANGLED SENSIBLY
         ###############
         eff.nsites = NA, eff.nsrveyed = NA, eff.pue = NA,
         stitch.in = src) %>%
  mutate(
    srvy.type = "uvc.f", srvy.method = "radial", srvy.taxa = "fish", 
    #bruv assignment params
    d2bruv = NA,fpid = NA, 
    #effort
    eff.pue = 177, eff.unit = "m2",  #15 m cylinder 
    stitch.ed = NA, 
    stitch.out = paste("src", stitch.in, sep = "_"), 
    dat.partner = "NOAA"
  ) %>% 
  distinct()
  
out <- list('fish.fla' = df, 'uvc.f.effort.fla' = effort)
rm(list = c('dat', 'df', 'effort', 'raw'))
return(out)
  
