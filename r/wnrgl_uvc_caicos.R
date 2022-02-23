#wrngl script for caicos data
#JHMoxley Feb 22


source(here('r', 'fxn_library.R'))
ctry <- "caicos"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_caicos_fishSurveys.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

#sweep in src
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>% 
    #remove size class cols
    select(-starts_with('x')) %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]), 
           date = lubridate::dmy(date)
    ) %>% 
    #minor data cleaning
    mutate(site = ifelse(tolower(site) == "spanish chain", 
                         "chain", tolower(site)))
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}

####
#effort
####
effort <- raw %>% 
  mutate(country = ctry, 
         site.zone = "South Caicos", 
         site.reef = tolower(site), 
         site.reefcode = paste(site, season, year, sep="_")
         ) %>%  
  #site.reef.code has site/season/year baked in
  group_by(country, site.zone, site.reef, 
           year, season, site.reefcode) %>% 
  summarize(n.obs = n(), #number species obs on a survey
            eff.nsites = NA, eff.nsrvyed = NA,
            stitch.in = src) %>% 
  merge(
    read_csv(here('data', 'lkup_caicos_samplingLocs.csv')) %>% 
      janitor::clean_names(), 
    by.x = "site.reef", by.y = "site", all.x = T,
  ) %>% 
  #add hard-coded fields
  mutate(
    eff.pue = NA, eff.unit = "m2", 
    #survey detals
    srvy.type = "uvc.f", srvy.method = "belt", srvy.taxa = "fish", 
    #finprint assignment
    d2bruv = NA, fpid = NA, 
    stitch.in = src[1], 
    stitch.out = NA, stitch.ed = NA, 
    dat.partner = NA, 
  )

#####
#data
#####
raw %>% #scrub away student obs?
  filter(observer = "staff") %>% 
  ungroup() %>% 
  group_by(year, season, )
  