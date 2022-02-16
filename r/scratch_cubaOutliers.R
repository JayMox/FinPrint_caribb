##dealing w/ cuba outliers
##JHMoxley Jan 2022
source(here('r', 'pipeline_rawDataWeave.R'))
df %>% group_by(country, species) %>% 
  summarize(med_dens = median(density, na.rm = T), tg = unique(tg)) %>% 
  filter(med_dens > .1) %>% arrange(desc(med_dens)) %>%  view()


##go get raw cuba data
(raw <- read_csv(here('data', 'reef_benthos_rawdata', 'Cuba Docs',
                     'JardinesReina_fishDensity_raw.csv')) %>% 
  janitor::clean_names() %>% 
  gather(site, density, -species_sites) %>% 
  arrange(desc(density)))
ruth <- df %>% filter(country == "Cuba-JardinesdelaReina") %>% 
  arrange(desc(density))
#Ruth's density data does NOT account for the fact that Cuban data
#(according to the notes in their raw data .xlsx) define 
#density per 100 METERS SQUARED

#adjusted in pipeline_rawDataWeave 1/27/2022

