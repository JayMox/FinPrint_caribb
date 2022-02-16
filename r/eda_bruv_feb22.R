#BRUV eda
#JHMoxley Feb 2022

library(here)
library(tidyverse)

source(here('r', 'pipeline_rawDataWeave.csv'))

dat <- read_csv(here('data', 'BRUVmaxn_elasmobranch_observations.csv')) %>% 
  janitor::clean_names() %>% 
  filter(region_name == "Western Atlantic") %>% 
  
