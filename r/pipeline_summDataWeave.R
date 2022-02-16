#data wrangling script
#purpose: to concatenate data sources & be single source of truth
#    for finPrint Caribb project
#JHMoxley, 11/2/2021

library(tidyverse)
library(janitor)
library(here) 
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}


dat <- read_csv(here("data", "summStats_tbl_4.csv")) %>% 
  janitor::clean_names() %>% 
  #create id field
  mutate(id = paste(reef_name, reef_id, sep = "_")) 

#get env data
env <- read_csv(here("data", "caribb_site_data.csv")) %>% 
  janitor::clean_names() %>% 
  #create id field
  mutate(id = paste(reef_name, reef_id, sep = "_")) %>% 
  #get rid of the cols i don't want
  select(-x1, -reef_id, -region_name, -location_name, 
         -site_name, -reef_name, -trip_year)

#combine & return
df <- merge(dat, env, by = "id") %>% 
      as_tibble()

#check if trasnformations are wanted
if(exists("sc01")){
  if(sc01 == T){
    df <- df %>% mutate(
      fishrich.lt = log(fish_richness),
      meso_sharks.sc = scale01(meso_sharks),
      all_sharks.sc = scale01(all_sharks),
      apex_sharks.sc = scale01(apex_sharks),
      rays.sc = scale01(rays),
      carnivores.sc = scale01(carnivores), 
      piscivores.sc = scale01(piscivores), 
      omnivores.sc = scale01(omnivores), 
      invertivores.sc = scale01(invertivores), 
      planktivores.sc = scale01(planktivores), 
      herbivores.sc = scale01(herbivores), 
      coral.sc = scale01(coral), 
      algae.sc = scale01(algae), 
      other.sc = scale01(other), 
      #fishrichness.sc = scale01(fishrichness), 
      fishrich.lt.sc = scale01(fishrich.lt),
      market_gravity.sc = scale01(market_gravity), 
      reef_complexity.sc = scale01(reef_complexity)
    )
  }
}



site_vars <- c("x1", "reef_id", "region_name", "location_name", "site_name",
               "min_lat", "min_lon", "max_lon")
rm(list = c("dat", "env"))

return(df)
return(site_vars)
