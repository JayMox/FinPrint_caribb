#wrngl for Belize density estimates
#JHMoxley, Dec '21

require(tidyverse)
require(here)

paddack <- read_csv(here('data', 'lkup_trophic_paddack.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(scispp = str_trim(paste(genus, species, " ")),
         trophic_group = str_to_lower(trophic_group))
sites <- read_csv(here('data','lkup_glovers_samplingSites.csv')) %>% 
  select(-note)
dat <- read_csv(here('data', 'scratch_Belize_masterCounts.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(reserve = str_extract(src, "[:alpha:]{4,5}"), 
         scientific_name = str_replace(scientific_name, "_", " "))
transects <- read_csv(here('data', 'lkup_belize_areaStandardization.csv')) %>%
  janitor::clean_names() %>% 
  mutate(reserve = str_extract(src, "[:alpha:]{4,5}"))

#append standardization metrics
dat <- merge(dat, area, by = c('year', 'site', 'reserve'), 
            all.x = T) %>% 
  mutate(tr_surveyed = ifelse(is.na(tr_surveyed), 8, tr_surveyed))
  #ie, default assumption is 8 transects per site
#append trophic groups
dat <- dat %>% merge(paddack %>% select(scispp, trophic_group, fishing_status), 
                     by.x="scientific_name", by.y="scispp", all.x = T)
#append site designations
dat <- dat %>% merge(sites, by = c("reserve", "site"), all.x = T) %>% 
  mutate(id = paste(reserve, year, loc, sep = "_"))

##density estimation
dens <- dat %>% group_by(year, site, id, species) %>% 
  summarize(family = unique(family), scispp = unique(scientific_name),
            tg = unique(trophic_group),
            n.inds = sum(number_individuals, na.rm = T), 
            tr_surveyed = unique(tr_surveyed)) %>% 
  mutate(density = n.inds / (tr_surveyed*60))

##explore pairwise of new & RD-derived density estimates
ruth <- read_csv(here('data', 'Fish_Species_Belize.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(site = str_split(survey_id, "_", simplify=T)[,1], 
         year = str_split(survey_id, "_", simplify=T)[,2]) %>% 
  merge(area %>% 
          select(year, site, tr_surveyed), 
        by = c('year', 'site'), all.x = T) %>% 
  mutate(tr_surveyed = ifelse(is.na(tr_surveyed), 8, tr_surveyed))

#ruth data has repeated obs of same spp/same site/same yr w/ different density calcs
#trying to collapse these to # of spp observed in a site, standardized for area sampled
ruth2 <- read_csv(here('data', 'Fish_Species_Belize.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(site = str_split(survey_id, "_", simplify=T)[,1], 
         year = str_split(survey_id, "_", simplify=T)[,2]) %>% 
  #collapse repeated obs of spp on a given site
  group_by(year, site, species) %>% 
  summarize(reef_id = unique(reef_id), depth = unique(depth_m),
            n_obs = n()) %>% 
  merge(area %>% 
          select(year, site, tr_surveyed), 
        by = c('year', 'site'), all.x = T) %>% 
  mutate(tr_surveyed = ifelse(is.na(tr_surveyed), 8, tr_surveyed),
         density = n_obs/(tr_surveyed * 60)) %>% 
  merge(paddack %>% select(family, scispp, tg = trophic_group),
        by.x = c('species'), by.y = c('scispp'))
  
#why is this so much smaller in dimension
#105 spp in dens, 63 in ruth2
#ruth data exhibits far fewer sites

#pointwise comparison w/ ruth
merge(dens %>% ungroup() %>% select(year, site, scispp, density, tg, id) %>% 
        filter(site %in% unique(ruth$site)), 
      ruth %>% ungroup() %>% select(year, site, species, density), 
      by.x = c('year', 'site', 'scispp'), by.y = c('year', 'site', 'species'), 
      all.x = T, all.y = T) %>% 
  #NOT SURE WHERE ALL THE NAs are coming from?
  filter(!is.na(density.y)) %>% 
  ggplot() + geom_point(aes(x = density.y, y = density.x)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-0.0, 0.1)) + ylim(c(-0.0,0.1)) + 
  labs(title = "jm (y-ax) X ruth (x-ax) density calcs") + 
  facet_grid(year~tg)

#pointwise comparison of ruth & ruth2
merge(ruth2 %>% ungroup() %>% select(year, site, species, density, tg),
  ruth %>% ungroup() %>% select(year, site, species, density, ),
  by = c('year', 'site', 'species')) %>% 
  ggplot() + geom_point(aes(x = density.y, y = density.x)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-0.0, 0.055)) + ylim(c(-0.0,0.055)) + 
  labs(title = "ruth2 (y-ax, ie calc per site by jm) X ruth2 (x-ax, ie calced per transect (?) by rd) density calcs") + 
  facet_grid(year~tg)

#pointwise comparison of ruth2 & jhm
merge(dens %>% ungroup() %>% select(year, site, scispp, density, tg, id) %>% 
        filter(site %in% unique(ruth2$site)),
      ruth2 %>% ungroup() %>% select(year, site, species, density), 
      by.x = c('year', 'site', 'scispp'), by.y = c('year', 'site', 'species'), 
      all.x = T, all.y = T) %>% 
  #NOT SURE WHERE ALL THE NAs are coming from?
  filter(!is.na(density.y)) %>% 
  ggplot() + geom_point(aes(x = density.y, y = density.x)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-0.0, 0.1)) + ylim(c(-0.0,0.1)) + 
  labs(title = "jm (y-ax) X ruth2 (x-ax) density calcs") + 
  facet_grid(year~tg)

#histogram
merge(dens %>% ungroup() %>% select(year, site, scispp, density, tg, id) %>% 
        filter(site %in% unique(ruth2$site)), 
      ruth2 %>% select(year, site, species, density), 
      by.x = c('year', 'site', 'scispp'), by.y = c('year', 'site', 'species'), 
      all.x = T, all.y = T, suffixes = c(".jerr", ".ruth")) %>% 
  gather(obs, density, -year, -site, -scispp, -tg, -id) %>% 
  ggplot() + geom_histogram(aes(x = density, fill = obs), alpha = 0.5)

#summary tbl
#source(here('r', 'pipeline_dataWeave.R'))
df <- df %>% filter(location_name == "Belize")

#tally transects surveyed for each Tgroup
site.areas <- dat %>% ungroup() %>% 
  group_by(id) %>% 
  select(id, site, year) %>% distinct() %>% 
  merge(area %>% select(year, site, tr_surveyed), 
        by = c("site", "year"), all.x = T) %>% 
  mutate(tr_surveyed = ifelse(is.na(tr_surveyed), 8, tr_surveyed))
(area.surveyed <- site.areas %>% group_by(id) %>% 
  summarize(area.sampled = sum(tr_surveyed) * 60))
#twice a

#compare trophic estimates
dens.tl <- dat %>% ungroup() %>% 
  filter(site %in% unique(ruth2$site)) %>% 
  group_by(id, trophic_group) %>% 
  summarize(n.inds = sum(number_individuals, na.rm = T),
            year = unique(year)) %>% 
  merge(area.surveyed, by = "id", all.x = T) %>% 
  mutate(density = n.inds / area.sampled)

#compare with original estimates
bind_rows(dens.tl %>% 
          select(year, id, trophic_group, density),
                df %>% 
                  select(year = trip_year, id, 
                    contains("vore")
                  ) %>%
                  gather(trophic_group, density, -year, -id)) %>% 
  ungroup() %>% 
  mutate(obs = ifelse(str_detect(id, "[:upper:]{4,5}"), "jhm", "ruth"), 
         reserve = ifelse(str_detect(id, "^S"), "SWCMR", "GRMR"), 
         id = fct_relevel(id,"GRMR_2015_NA", "GRMR_2015_west", "West_510_2017", "GRMR_2019_west",
                         "GRMR_2015_east", "East_43_2016",  "East_43_2017",
                         "GRMR_2017_east", "GRMR_2019_east", "East_43_2019",
                         "GRMR_2015_lagoon", "GRMR_2017_lagoon", "GRMR_2019_lagoon", "Glovers Lagoon_158_2019", 
                         "SWCMR_2016_NA", "Southwater Cay_50_2017", "SWCMR_2018_NA")) %>% 
  ggplot() + geom_col(aes(x = id, 
                          y = density, col = obs), 
                      position = "dodge") + facet_grid(reserve~trophic_group, scales = 'free') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) + 
  coord_flip()

##compare with raw ruth data to original summary vals
ruth %>% 
  merge(site.areas %>% select(-tr_surveyed), by = c('site', 'year')) %>% 
  merge(area.surveyed, by = "id") %>% 
  merge(paddack %>% select(family, tg = trophic_group, scispp), 
               by.x = "species", by.y = "scispp") %>% 
  #backcalc spp counts, assuming she calced by transect
  mutate(n_ind = round(density*8*60),
         year = as.numeric(year)) %>% ungroup() %>%
  select(-density, -depth_m) %>% 
  group_by(id, tg) %>%
  summarize(year = unique(year),
    area.sampled = unique(area.sampled), 
    n = n(),
    tg = unique(tg),
    n_obs = sum(n_ind, na.rm = T)) %>% 
  mutate(density = as.numeric(n_obs/area.sampled), 
         obs = "ruthraw") %>% 
  distinct() %>% 
  #filter(id == "GRMR_2019_east") %>% view
  ungroup() %>% 
  select(year, id, tg, density, obs) %>% 
  bind_rows(df %>% select(year = trip_year, id, 
                     contains("vore")) %>% 
                       mutate(obs = "ruthderived") %>%
              gather(tg, density, -year, -id, -obs)) %>% 
  ggplot() + geom_col(aes(x = id, 
                          y = density, col = obs), 
                      position = "dodge") + facet_grid(id~tg, scales = 'free') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) + 
  coord_flip()
  


