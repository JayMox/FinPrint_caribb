#pipeline script for assembling fish UVC data spp scale
#into database of site, country, species, tg, density
#n.b.: belize data was reported at #-inviduals, 
#and density estimated by JHM, in scaratch_blz_densCalcs

#JHMoxley, Jan 2022

library(tidyverse)
library(janitor)
library(here) 
require(rfishbase)
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}


files <- list.files(here("data"), 
                    pattern = "Fish_Species*")
#belize needs specific handling
files <- files[!str_detect(files, "Belize")]
print("reading in data from")
print(files)

df <- data.frame(NULL)
for(j in 1:length(files)){
  dat <- read_csv(here("data", files[j]), 
                  col_types = "cccdcd") %>%
    janitor::clean_names() %>% 
    mutate(
      species = tolower(species),
      fname = files[j], #get full dir name or use "home" pkg
      country = str_extract(files[j], pattern = "(?<=_| )[^_ ]+(?=\\.csv)")
    )
  
  df <- df %>% bind_rows(dat)
  print(paste(nrow(dat), "from ", files[j], "ingested"))
}
#explicit handling of belizean data
blz <- read_csv(here("data", "Fish_Species_SPP_jhmDensEst_Belize.csv")) %>%
  mutate(reef_id = str_extract(uvc_id, "[:digit:]{2}_[:digit:]{4}"),
         site_name = "Belize",
         survey_id = str_extract(uvc_id, "[:alpha:]{4,5}_[:alpha:]{2,6}"),
         depth = NA,
         species = tolower(scientific_name),
         fname = "Fish_Species_SPP_jhmDensEst_Belize.csv",
         country = "Belize"
         ) %>%
    select(reef_id, site_name, survey_id, depth, species, density = dens,
           fname, country)
df <- df %>% bind_rows(blz)
df <- df %>% 
  mutate(density = ifelse(country == "Cuba-JardinesdelaReina", 
                          density / 100, 
                          density )) #standardize densities per sqMeter
 
#assign trophic groups
tg.lkup <- read_csv(here('data', 'lkup_trophic_paddack.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(scispp = tolower(str_trim(paste(genus, species, " "))),
         tg = as.factor(str_to_lower(trophic_group))) %>% 
  select(family, tg, fishing_status, scispp)
df <- df %>% 
  merge(
    tg.lkup %>% select(-family), 
    by.x = "species", by.y = "scispp", 
    all.x = T)

###############
##fishbase query
##############
fish <- df$species %>% unique()
fish <- paste0(toupper(substr(fish, 1,1)), substr(fish, 2, nchar(fish)))
nofb <- fish[is.na(validate_names(fish))]

print(paste0(nrow(nofb), 
             " of ", length(fish), " unique spp validated in Fishbase"))
print("spp not validated agaisnt fishbase: ")
print(nofb)

########
## FB query
########
fb <- fb_tbl("species") %>%
  mutate(sci_name = paste(Genus, Species)) %>%
  filter(sci_name %in% fish) %>%    #231 rows
  select(sci_name, FBname, Length, SpecCode, FamCode)  %>% 
  mutate(species = tolower(sci_name))
fb <- fb %>% 
  merge(
    #get taxonomic family
    fb_tbl("families") %>% select(FamCode, Family, CommonName) %>% 
    #could order if desired
      filter(FamCode %in% fb$FamCode), 
    by = "FamCode", all.x = T
  ) %>% 
  merge(
    #get trophic data
    ecology(fb$SpecCode) %>% select(SpecCode, dplyr::matches("Troph")), 
    by = "SpecCode", all.x = T
  )

#query non-matches
#dealing w/ no FBval
nofb <- data.frame(name = fish[is.na(validate_names(fish))]) %>% 
  mutate(genus = str_split(name, " ", simplify = T)[,1], 
         species = str_split(name, " ", simplify = T)[,2]
  ) %>% 
  merge(
    fb_tbl("species") %>% 
      filter(Genus %in% str_split(nofb," ", simplify = T)[,1]) %>% 
      select(FamCode, Genus) %>% distinct(), 
    by.x = "genus", by.y = "Genus", all.x = T
  )

#write out FBquery once a month
fb %>% write_csv(file = here("data", 
                             paste0("fishBaseQuery_", 
                                    substr(Sys.Date(), 1, 7),
                                    ".csv")))
nofb %>% write_csv(file = here("data", 
                               paste0("fishBaseQuery_noMatches", 
                                      substr(Sys.Date(), 1, 7),
                                      ".csv")))

#merge
df <- df %>% 
  merge(fb, by = "species", all.x = T) %>% 
  merge(nofb %>% mutate(species = tolower(name)) %>% select(-name, -genus), 
        by = c("species", "FamCode"), all.x = T)

rm(list = c("files", "dat", "tg.lkup", "fb", "nofb", "fish", "blz"))
return(df)

########
##NAs adjusted by jhm
########
## corrected in raw data (usually against FishBase)
# a. polygonia => polygonius (FL)
# carangoides bartholomaei => caranx... (Cuba)
# cephalopholis cruentatus => c. cruentata (Caicos)
# chaetodon aculeatus => prognathodes aculeatus (Caicos)
# chilomycterus atinga => c. reticulatus (Florida)
# hemiemblemaria simula => simulus (Florida)
# hypoplectrus hybrid => h. sp (Florida, PR)
# hypoplectrus tann => h. sp (Florida, PR)
# Needlefish sp. => Tylosurus sp (Florida)
# paradiplogrammus bairdi => callionymus bairdi
# porgy sp => sparid sp. (Florida)
# razorfish sp. => Xyrichtys sp. (Florida)
# snapper sp => Lutjanus sp.
# c. enchrysura => c. enchrysurus (Florida)
# prognothodes aculeatus => prognathodes
#####
## added to tg.lkup
#####
# apogon aurolineatus
# bodianus pulchellus
# calamus nodulus
# carcharhinus leucas
# carcharhinus perezii (perezi corrected to perezii)
# chloroscombus chrysurus (no max L, not in FB)
# chromis enchrysura
# coryphopterus punctipectophorus
# echeneis neucratoides
# elacatinus macrodon
# emblemariopsis bahamensis
# halichoeres caudalis
# harengula jaguana
# Pomacanthidae	sp.
# menidia sp. (under Atherinospidae; )
# microgobius microlepis
# Needlefish sp. (under Belonidae)
# Negaprion brevirostris
# nicholsina utsa
# opistognathus whitehursti (misplled, no a in genus)
# paralichthys albigutta (no flounders at all interesting?)
# pareques umbrosus
# phaeoptyx xenus
# ptereleotris helenae
# rypticus maculatus
# seriola dumerili
# synodus foetens
#hemiramphus brasiliensis
##ignored
# blenny sp.
# unknown sp. 
# holocanthus sp. (don't know if angelfish/Pomacanthidae or puffer/Tetradontidae)


