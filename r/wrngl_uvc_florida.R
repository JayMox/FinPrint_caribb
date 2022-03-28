#wrngl of florida data
#JHMoxley Feb 22 
library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))
ctry <- "florida"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('x_flaFish_2016.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

#sweep in src data
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]),
           date = lubridate::ymd(paste(year, month, day)),
           site.reef = as.factor(primary_sample_unit),
           site.reefcode = paste(primary_sample_unit, month, date, sep = "_"),
           site.zone = paste(subregion_nr, tolower(habitat_cd), "_"),
           n.obs = NA, eff.nsites = NA, eff.nsrveyed = NA, eff.pue = NA
    ) %>% 
    select(country, src, date, year,
           site.reef, site.reefcode, site.zone,
           transect = station_nr, #2 radials per primary sampling
           habitat_cd, zone_nr, #zone is quasi depth (inshore, channel, fore, offshore)
           lat = lat_degrees, lon = lon_degrees,
           count = num, 
           sci.name = species_nr, spp = species_cd,
           n.obs, eff.nsites, eff.nsrveyed, eff.pue
           )
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}
