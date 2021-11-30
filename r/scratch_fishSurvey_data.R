#data stitching script for reef data
#JHMoxley 11/29/21
require(tidyverse)
library(here)

#look for reef fish data
surveys <- list.files(here('data'), pattern = "Fish_Species*")
df <- tibble(reef_id = NULL, site_name = NULL, survey_id = NULL, 
                depth_m = NULL, species = NULL, 
                density = NULL)
for(i in 1:length(surveys)){
  dat <- read_csv(here('data', str_subset(surveys, surveys[i]))) %>% 
    janitor::clean_names()
  colnames()
  
  
  df <- rbind(df, dat)
}

#using an excel wrangled incomplete dataset



#SCRATCH CODE
#xplore belize data
belize <- read_csv(here('data', str_subset(surveys, "Belize"))) %>% 
  janitor::clean_names() %>% 
  mutate(survey_code = str_extract(survey_id, str_split(survey_id, "_", simplify = T)[,1]),
         survey_site = str_extract(survey_id, "[:alpha:]{2,}(?=FR[:digit:]{1}|PR[:digit:]{1})"),
         survey_hab = str_extract(survey_id, "FR|PR"),
         survey_num = str_extract(survey_id, "[:digit:](?=_)"),
         survey_yr = str_extract(survey_id, "(?<=_)[:digit:]{4}"))

colo <- read_csv(here('data', str_subset(surveys, "Colombia"))) %>% 
  janitor::clean_names() %>% 
  mutate(survey_code = str_split(survey_id, "_", simplify=T)[,1],
         survey_site = str_split(survey_id, "-", simplify=T)[,1],
         survey_num = str_split(survey_id, "_", simplify=T)[,2])

cuba <- read_csv(here('data', str_subset(surveys, "Cuba"))) %>% 
  janitor::clean_names()

florida <- read_csv(here('data', str_subset(surveys, "Florida"))) %>% 
  janitor::clean_names()

pr <- read_csv(here('data', str_subset(surveys, "PuertoRico"))) %>% 
  janitor::clean_names()

