#fxn library for FinPrint Caribb
#JHMoxley, Winter22

require(tidyverse)
require(here)

###
#DATA CALLS
###
uvc <- read_csv(here('data', 'lkup_surveyeffort.csv')) %>% 
  janitor::clean_names()
# bruv.wa <- read_csv(here('data', 'BRUVmaxn_elasmobranch_observations_all.csv')) %>%
#   janitor::clean_names() %>%
#   filter(region_name == "Western Atlantic")
# #%>% write_csv(here('data/pipe', 'src_BRUV_westernAtl.csv'))



###
#FXN
###

#scale [0,1] an array of numbers 
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}