#fxn library for FinPrint Caribb
#JHMoxley, Winter22

require(tidyverse)
require(here)

###
#DATA
###
uvc <- read_csv(here('data', 'lkup_surveyMethods_uvc.csv')) %>% 
  janitor::clean_names()

###
#FXN
###

#scale [0,1] an array of numbers 
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}