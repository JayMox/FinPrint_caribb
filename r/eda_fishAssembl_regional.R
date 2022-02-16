##eda plotting of fish assemblage regionally
##aim to identify subset of spp represented across sites
##JHMoxley Jan 2022

source(here('r', 'pipeline_rawDataWeave.R'))
df <- df %>% 
  filter(density > 0)
df %>% group_by(country) %>% 
  summarize(n = n(), 
            n_survey = length(unique(survey_id)), 
            n_spp = length(unique(species)), 
            dens_max = max(density, na.rm = T),
            dens_min = min(density, na.rm = T))
#radial transect sites are SIGNIFICANTLY oversampled


df %>% group_by(country, species) %>% 
  summarize(med_dens = median(density, na.rm = T), 
            tg = unique(tg)) %>% 
  ggplot() + geom_histogram(aes(x = med_dens, fill = tg)) + theme_classic() + 
  facet_wrap(~country)

#double chk these outlier vals
df %>% group_by(country, species) %>% 
  summarize(med_dens = median(density, na.rm = T), 
            tg = unique(tg)) %>% filter(med_dens > .1) %>% view

########
##scratch eval of assemblage
##uses lists of family taxa eval'ed in Colombia
########
families <- c("Acanthuridae", "Balistidae", "Carangidae", 
              "Chaetodontidae", "Haemulidae", "Labridae", 
              "Lutjanidae", "Pomacanthidae", "Pomacentridae", 
              "Scaridae", "Serranidae", "Sphyraenidae", "Tetraodontidae")

sc <- df %>% 
  filter(Family %in% families)
#summary table
sc %>% group_by(country, Family, )