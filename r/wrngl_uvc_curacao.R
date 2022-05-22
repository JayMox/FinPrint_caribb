##wrangle curacao UVC fish spp counts
#JHMoxley, May 22

library(tidyverse)
library(here)

ctry <- "curacao"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_curacao2015_Fish.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>%
    gather(species, density, -station) %>%
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]), 
           yr = 2015,
    ) 
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}

##effort
effort <- raw %>% 
  select(-species, -density) %>% 
  mutate(country = "Curacao", 
         site.zone = NA, 
         site.reef = station, 
         site.reefcode = station) %>% 
  group_by(country, site.zone, site.reef, site.reefcode) %>%
  summarize() %>% 
  mutate(lat = NA, lon = NA, 
            year = 2015, 
            n.obs = NA, 
            eff.nsites = 5, #transects/station
            eff.nsrveyed = 5, 
            stitch.in = src) %>% 
  #merge w/ station info?
  #no station info provided by Vermj
  mutate(
    #wrngl into lkup form
    #survey params
    srvy.type = "uvc.f", srvy.method = "belt", srvy.taxa = "fish", 
    #bruv assignment params
    d2bruv = NA, fpid = NA, 
    #effort
    eff.pue = 30*2*2, eff.unit = "m2",
    stich.ed = NA, 
    stitch.out = paste("src", stitch.in, sep = "_"),
    dat.partner = "NOAA"
  ) %>% 
  distinct()


########
##DATA
#########
df <- raw %>% ungroup() %>% 
  mutate(date = NA, 
         site.reefcode = station, 
         n.obs = NA) %>% 
  #merge to get effort data
  merge(effort %>% ungroup() %>% 
          select(contains('site'), contains('eff.'), lat, lon),
        by = "site.reefcode", all.x = T) %>% 
  #get scientific names
  merge(read_csv(here('data', 'lkup_vermeij_sppcodes.csv')) %>% 
                   janitor::clean_names() %>% 
                   mutate(sci.name = tolower(str_replace(genus, "_", " ")),
                          spp = tolower(new_name)) %>% 
                   select( #get cols of interest
                     spp, family, sci.name), 
        by.x = "species", by.y = "spp", all.x = T) %>% 
  select(country, src, year = yr,
         sci.name, spp = species, 
         #count, #density,
         starts_with('site.'), lat, lon, 
         n.obs, starts_with('eff.')) %>% 
  mutate(transect = NA)
  #not sure what to do for transects
  #merge statement to get sci.names
  #select statement for cols of interest
  
out <- list('fish.cura' = df, 'uvc.f.effort.cura' = effort)
rm(list = c('dat', 'df', 'effort', 'raw'))
return(out)
