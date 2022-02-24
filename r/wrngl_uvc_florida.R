#wrngl of florida data
#JHMoxley Feb 22
library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))
ctry <- "florida"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('x_flaFish_2016.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

#sweep in src data
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]), 
    ) %>% 
    select(transect = station_nr, #or should this 
           year, )
    mutate(
      especie_des = 
        ifelse(especie == "Ccru", 
               "Cephalopholis cruentata",
               especie_des)
      #deal w/ species w/ funky encoding error
    ) %>% 
    select(-longitud_des)
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}
