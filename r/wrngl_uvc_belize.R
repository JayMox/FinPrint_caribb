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

sc <- read_csv(here('data/stitch', src[1])) %>% 
  janitor::clean_names() %>% 
  group_by(site, year) %>% 
  summarize(
    #n.tr = max(transect, na.rm = T), 
    n.srvyed = max(transect, na.rm = T), 
    eff.pue = 60, 
  ) %>% 
  mutate(
    country = ctry, 
    src = src[1],
    n.transects = max(n.survyed)
  )


####
  mutate(country = ctry, 
         lat = NA, lon = NA, 
         site_zone = site, site_reef = site, site_reefcode = site,
         )
#####
#SCRATCH
  #%>% 
  #   mutate(
  #     #try to get spp codes
  #     spp = spp.ncodr(scientific_name, 3)
  # ) %>% colnames()
  
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names()  %>% 
    filter(scientific_name != "#N/A") %>% 
    group_by(scientific_name, site, transect) %>% 
    #agg data for sums
    summarize(
      count = sum(number_individuals, na.rm = T),
      site.reefcode = first(site), #some confusion on appropriate site assignment
      site.reef = first(site), #look into deeper
      year = first(year), 
      spp = spp.ncodr(scientific_name, 3),
      #n.obs = n(), 
    ) %>% colnames
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]),
           n.obs = NA, #i don't think n.obs will be meaningful w/o pre-summarizing blz data
           
    )%>% 
    select(
      year, 
      site.reef = site, #or zone
      site.reefcode = site,
      count = 
    )
  
}