##Re-evaluation of density estimates of Colombia data
##notably.. columbia has a max density ~ 1.55 PER SQUARE METER.. it's a parrotfish
##JHMoxley Jan 2022

library(tidyverse)
library(here)

ruth <- read_csv(here("data", "Fish_Species_Colombia.csv"))
#get raw count data
raw <- read_csv(here("data", "reef_benthos_rawdata", 
                     "Mainland_Colombia", "Fish_Ab_San_Bernardo.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(site = "San Bernardo") %>% 
  bind_rows(
    read_csv(here("data", "reef_benthos_rawdata", 
                  "Mainland_Colombia", "Fish_Ab_Rosario.csv")) %>% 
      janitor::clean_names() %>% 
      mutate(site = "Rosario")
  ) %>% 
  #subset fields & rename w/ english colnames
  select(site, station = nom_estacion, station_code = cod_estacion, 
         lat = latitud, lon = longitud, date = fecha, transect = censo, 
         spp_code = especie, species = especie_des, count = cantidad)

#collapse spp counts per transect
dat <- raw %>% 
  group_by(site, date, transect, species, station) %>% 
  summarize(
    count = sum(count, na.rm = T),
    station_code = unique(station_code),
    lat = unique(lat), 
    lon = unique(lon), 
    spp_code = unique(spp_code)
  )
#10 50x2m2 (???OR 30x2m2??) transects per site/station, except Tintipan 2 Medio (8 here)
area.surveyed = raw %>% group_by(site, station) %>% 
  summarize(n = length(unique(transect))) %>% 
  mutate(area_surveyed = 100 * n) #sq meters surveyed 
  
# num of transects/station does NOT match the "Transectos por Estaciones de Monitoreo.xlsx"

#up-scaled density ests
dat <- dat %>% 
  merge(area.surveyed %>% select(station, area_surveyed), 
        by = c("site", "station"), all.x = T) %>% 
  mutate(dens_transect = count/area_surveyed) %>% 
  group_by(site, station, species, transect) %>% 
  summarize(n = sum(count),
            survey_id = paste0(station_code, "_", transect),
            area_surveyed = unique(area_surveyed), 
            density_transect = n/area_surveyed)

sc <- dat %>% 
  merge(ruth %>% select(-reef_id) %>% janitor::clean_names(), 
        by = c("survey_id", "species"))
#plot jhm vs. ruth
sc %>% ggplot() + 
  geom_point(aes(x = density, y = density_transect)) + 
  geom_abline(aes(intercept = 0, slope = 1))
