#wrngl script for caicos data
#JHMoxley Feb 22


source(here('r', 'fxn_library.R'))
ctry <- "caicos"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_caicos_fishSurveys.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))

#sweep in src
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/stitch',  src[j])) %>%
    janitor::clean_names() %>% 
    #remove size class cols
    select(-starts_with('x')) %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]), 
           date = lubridate::dmy(date)
    )
  
  raw <- bind_rows(raw, dat)
  message(paste("dat from", src[j], " inputted"))
}