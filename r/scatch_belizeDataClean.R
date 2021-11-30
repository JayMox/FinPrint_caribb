###checking belize 2017 data
###also attempting to deduce how Ruth calc's her densities
###belize is 8 transects a site, 30X2m UVC tubes

#get aggregated data for balize
dat <- read_csv(here('data', 'scratch_Belize_masterScratch.csv')) %>% 
  janitor::clean_names()



dat <- read_csv(here('data', 'scratch_Belize2016.17.19_dataCleaningScratch.csv'),
                col_types = "nccccdncccccdd_") %>% 
  janitor::clean_names()
dat %>% group_by(year, site) %>% 
  summarize(n.transect = max(transect))

dat %>% group_by(year, site, scientific_name) %>% 
  summarize(n.obs = sum(number_individuals, na.rm = T), 
            n.transect = max(transect))

dat %>% group_by(site, species) %>% 
  summarize(n_inds = sum(number_individuals, na.rm = T),
            density = n_inds/(60*8)) %>% 
  ggplot() + geom_col(aes(x = site, y = n_inds)) +
  #theme(axis.text.x = element_text(angle=90)) + 
  facet_wrap(~species, scales = "free")


dat <- read_csv(here('data', 'scratch_Belize2015_dataCleaningScratch.csv')) %>% 
  janitor::clean_names()

dat %>% group_by(site, scientific_name) %>% 
  summarize(n_inds = sum(number_individuals, na.rm = T),
            density = n_inds/(60*8))

belize <- read_csv(here('data', str_subset(surveys, "Belize"))) %>% 
  janitor::clean_names() %>% 
  mutate(survey_code = str_extract(survey_id, str_split(survey_id, "_", simplify = T)[,1]),
         survey_site = str_extract(survey_id, "[:alpha:]{2,}(?=FR[:digit:]{1}|PR[:digit:]{1})"),
         survey_hab = str_extract(survey_id, "FR|PR"),
         survey_num = str_extract(survey_id, "[:digit:](?=_)"),
         survey_yr = str_extract(survey_id, "(?<=_)[:digit:]{4}"))

merge(dat %>% group_by(site, scientific_name) %>% 
        summarize(n_inds = sum(number_individuals, na.rm = T),
                  density = n_inds/(60*8)),
      belize %>% filter(survey_yr == 2015) %>% select(survey_code, species, density), 
      by.x = c("site", "scientific_name"), by.y = c("survey_code", "species"), all = TRUE) %>% 
ggplot + geom_point(aes(x = density.x, y = density.y)) + facet_wrap(~site, scales = "free")
