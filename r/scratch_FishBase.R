#exploratory script for linking database to FishBaseAPI

library(tidyverse)
library(here)
require(rfishbase)

source(here('r', 'pipeline_rawDataWeave.R'))

fish <- df$species %>% unique
#capitalize genus
fish <- paste0(toupper(substr(fish, 1,1)), substr(fish, 2, nchar(fish)))
validate_names(fish)
#non-validated species names
fish[is.na(validate_names(fish))]

#table of species w/ lengths
fb <- fb_tbl("species") %>% 
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% fish) %>% 
  select(sci_name, FBname, Length, SpecCode)

#field description: https://www.fishbase.in/manual/fishbasethe_ecology_table.htm
ecology(fb$SpecCode) %>% 
  select(dplyr::matches("Troph")) %>% 
  summary()
#from here: 
#https://github.com/ropensci/rfishbase/issues/167
#food troph is Monte Carlo estimate based on known food items
#diet troph is mean trophL derived from actual diet study
#food troph generally accepted as more broadly robust
#also avail: estimate.troph from species summary page

#whole bit on trophic level & querying FB: https://www.carlboettiger.info/2013/09/08/some-rfishbase-updates.html

#
