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
         site.reefcode = station) %>% colnames()
  group_by(country, site.zone, site.reef, site.reefcode) %>%
  summarize(lat = NA, lon = NA, 
            year = 2015, 
            n.obs = NA, 
            eff.nsites = 5, #transects/station
            eff.nsrveyed = 5, 
            stitch.in = src) %>% 
  #merge w/ station info?
  mutate(
    #wrngl into lkup form
    #survey params
    srvy.type = "uvc.f", srvy.method = "belt", srvy.taxa = "fish", 
    #bruv assignment params
    d2bruv = NA, fpid = NA, 
    #effort
    effort.pue = 30*2*2, eff.unit = "m2"
  )

