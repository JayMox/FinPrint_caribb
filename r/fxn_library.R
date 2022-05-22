#fxn library for FinPrint Caribb
#JHMoxley, Winter22

require(tidyverse)
require(here)

###
#DATA CALLS
###
#this obj will become srvy or effort
uvc <- read_csv(here('data', 'lkup_surveyeffort.csv')) %>% 
  janitor::clean_names()
bruv.wa <- read_csv(here('data', 'BRUVmaxn_elasmobranch_observations_all.csv')) %>%
  janitor::clean_names() %>%
  filter(region_name == "Western Atlantic")
#%>% write_csv(here('data/pipe', 'src_BRUV_westernAtl.csv'))

#trait & trophic codes
codes <- read_csv(here('data', 'lkup_trait_codes.csv')) %>% 
  janitor::clean_names() %>% 
  #make informative field names
  mutate(field = paste(val_code, var_code, sep = "_")) %>% 
  mutate(field = ifelse(var_code %in% c('db', 'tl'), var_code, field))

#colnames for select() in data.stitch
uvc.f.cols <- c(
  "site.reef", "site.zone", "site.reefcode",
  "year", "country", "sci.name", "count", 
  "transect","eff.nsites", "eff.nsrvyed", "eff.pue")

###
#FXN
###

#scale [0,1] an array of numbers 
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

#spp shortner; shorten sci names to gen[0,3]spp[0,3]
spp.ncodr <- function(x, n = 3){
  paste(substr(str_split(x, " ", simplify = T)[,1], 0,n), 
        substr(str_split(x, " ", simplify = T)[,2], 0,n), 
        sep = "-")
}

