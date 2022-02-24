#scratch script exploring rvc tools
#see here: https://github.com/jeremiaheb/rvc

library(devtools)
devtools::install_github('jeremiaheb/rvc')
require(rvc)
#pkg dependency ages out macOS :(
#attempting to use docker images


ctry <- "florida"
src <- c('x_flaFish_2016.csv')

sc <- read_csv(here('data/stitch',  src[j])) %>%
   janitor::clean_names()

#look at structure of sampling units
sc %>% group_by(primary_sample_unit) %>% 
  select(primary_sample_unit, station_nr, zone_nr,
         subregion_nr, species_cd,
         month, day) %>% 
  summarize(n.station = length(unique(station_nr)),
            n.zone = length(unique(zone_nr)),
            n.subreg = length(unique(subregion_nr)),
            n.spp = length(unique(species_cd)),
            n.mos = length(unique(month)),
            n.day = length(unique(day)))
#2 secondary stations per priamry w/ a count of individuals
#