#wrangle cuba UVC fish spp counts
#JHMoxley, Feb 22

source(here('r', 'fxn_library.R'))
ctry <- "cuba"
message(paste("sourcing raw uvc counts from", ctry))
src <- "src_JardinesReina_fishDensity.csv"
n.src <- length(unique(src))
message(paste(n.src, "src files in stitch"))
message(paste(src, "frm", ctry))

#sweep in src data
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch', src[j])) %>% 
    janitor::clean_names() %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j])
           ) %>% 
    gather(key = "site.reef", value = "density.raw", 
           -species_sites, -country, -src) %>% 
    rename(sci.name = species_sites)
  
  raw <- bind_rows(raw, dat)
  message(paste(nrow(dat), "rows from ", src[j], " swept up"))
}
####
##effort
####
effort <- raw %>% ungroup() %>% 
  group_by(country, site.reef) %>% 
  summarize(n.obs = sum(density.raw > 0, na.rm = T)) %>% 
  mutate(site.zone = "Jardines de la Reina", 
         site.reefcode = "Transects 1 thr 10", 
         lat = NA, lon = NA, year = 2018, 
         eff.nsites = 10,   #transects per site; 
         eff.nsrvyed = 10, #density reported in aggregate
         srvy.type = "uvc.f", srvy.method = "belt", srvy.taxa = "fish",
         d2bruv = NA, fpid = NA,
         eff.pue = 50*4, eff.unit = "m2",
         stitch.ed = NA, stitch.out = NA, stitch.in = NA,
         dat.partner = NA
  )

#####
##data
#####
#back-calc spp counts & fill empty fields
df <- raw %>% ungroup() %>% 
  #10 belts at a site, 50x4 tubes
  mutate(count = as.integer(density.raw * 10 * 50 * 4), 
         spp = spp.ncodr(sci.name), 
         transect = "t1-10", country = ctry) %>% 
  merge(effort %>% ungroup() %>% 
          select(site.reef, site.reefcode,site.zone,
                 lat, lon, year, n.obs, 
                 eff.nsites, eff.nsrvyed, eff.pue), 
        by = "site.reef") %>% 
  #adjust a spp name
  mutate(sci.name = ifelse(sci.name == "Carcharhinus perezi", 
    "Carcharhinus perezii", sci.name)
  )

##push
out <- list('fish.cuba' = df, 'uvc.f.effort.cuba' = effort)
rm(list = c('dat', 'df', 'effort', 'raw'))
return(out)




