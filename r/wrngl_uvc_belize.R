#wrngl src blz data into database
#JHMoxley Feb 22

source(here::here('r', 'fxn_library.R'));
ctry <- "belize";
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_Belize_masterCounts.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

#use pre-summary tbl to wrangle into final form
raw <- NULL
for(j in 1:n.src){
  #create pre-summary of counts across size-classes
  dat <- read_csv(here('data/stitch', src[j])) %>% 
    janitor::clean_names() %>% 
    filter(scientific_name != "#N/A") %>% #Don't know?
    group_by(scientific_name, site, transect) %>% 
    summarize(
      #get data
      count = sum(number_individuals, na.rm = T),
      
      #everything else
      site.reefcode = first(site), #some confusion on appropriate site assignment
      site.reef = first(site), #look into deeper
      year = first(year), 
      spp = NA, 
      n.obs = NA #can't figure out how to make this reasonable
    ) 
  #bind up 
  raw <- bind_rows(raw, dat %>% arrange(site, transect, scientific_name))
  message(paste("dat from", src[j], " inputted"))
}

####
#effort
####
effort <- read_csv(here('data/stitch', src[1])) %>% 
  janitor::clean_names() %>% 
  group_by(site, year) %>% 
  summarize(
    #n.tr = max(transect, na.rm = T), 
    eff.nsrvyed = max(transect, na.rm = T), 
    eff.pue = 60, 
  ) %>% 
  mutate(
    country = ctry, 
    site.reefcode = site, 
    lat = NA, lon = NA, 
    n.obs = n(), eff.pue = 60, eff.unit = 'm2', 
    eff.nsites = #see note
      ifelse(eff.nsrvyed == 8, 
             8, eff.nsrvyed),
    srvy.type = 'uvc.f',srvy.method = 'belt', srvy.taxa = 'fish', 
    #bruv bits
    d2bruv = NA, fpid = NA,
    #integration
    stitch.in = src[1], stich.ed = NA, stitch.out = NA, dat.partner = NA, 
  ) %>% 
  rename(site.reef = site) %>% 
  #merge w/ lkup_belize_samplingSights for Reserve assignment
  merge(
    read_csv(here('data', 'lkup_belize_samplingSites.csv')) %>% 
      select(site.zone = reserve, site), 
    by.x = "site.reef", by.y = 'site', all.x =T
    #could get the habitat & finprint id here too
  ) 


######
##data
######
df <- raw %>% ungroup() %>% 
  rename(sci.name = scientific_name) %>% 
  mutate(spp = NA, n.obs = NA) %>% 
  #merge w/ effort
  ungroup() %>% merge(
    effort %>% ungroup() %>% 
      select(site.reefcode, n.obs, year, site.zone,
            eff.nsites, eff.nsrvyed, eff.pue), 
    by = c('year', 'site.reefcode')
  )

out <- list('fish.belize' = df, 'uvc.f.effort.belize' = effort)
rm(list = c('dat', 'df', 'raw', 'effort'))
return(out)

# ############
# ####
#   mutate(country = ctry, 
#          lat = NA, lon = NA, 
#          site_zone = site, site_reef = site, site_reefcode = site,
#          )
# #####
# #SCRATCH
#   #%>% 
#   #   mutate(
#   #     #try to get spp codes
#   #     spp = spp.ncodr(scientific_name, 3)
#   # ) %>% colnames()
#   
#   dat <- read_csv(here('data/stitch',  src[j])) %>%
#     janitor::clean_names()  %>% 
#     filter(scientific_name != "#N/A") %>% 
#     group_by(scientific_name, site, transect) %>% 
#     #agg data for sums
#     summarize(
#       count = sum(number_individuals, na.rm = T),
#       site.reefcode = first(site), #some confusion on appropriate site assignment
#       site.reef = first(site), #look into deeper
#       year = first(year), 
#       spp = spp.ncodr(scientific_name, 3),
#       #n.obs = n(), 
#     ) %>% colnames
#     mutate(country = as.factor(ctry), 
#            src = as.factor(src[j]),
#            n.obs = NA, #i don't think n.obs will be meaningful w/o pre-summarizing blz data
#            
#     )%>% 
#     select(
#       year, 
#       site.reef = site, #or zone
#       site.reefcode = site,
#       count = 
#     )
#   
# }