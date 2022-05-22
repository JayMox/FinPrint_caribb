##wrangle curacao UVC fish spp counts
#JHMoxley, May 22

library(tidyverse)
library(here)

ctry <- "curacao"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_curacao2015_Fish.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]), 
           fecha = lubridate::mdy_hm(fecha),
           yr = lubridate::year(fecha),
    ) %>% 
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
