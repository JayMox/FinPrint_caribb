#ingest colombia UVC fish spp counts
#JHMoxley, Feb 22

source(here('r', 'fxn_library.R'))

srvy <- uvc %>% filter(country == ctry)
print(paste("sourcing raw uvc counts from", ctry))

src <- unique(srvy$src)
n.src <- length(unique(src))
print(paste(n.src,"src files found in pipe"))
print(paste(src, "frm", ctry))

#src sweep
raw <- NULL
for(j in 1:n.src){
  dat <- read_csv(here('data/pipe',  paste0('src_', src[j]))) %>%
    janitor::clean_names() %>% 
    mutate(country = as.factor(ctry), 
           src = as.factor(src[j]), 
           fecha = mdy_hm(fecha),
           yr = year(fecha),
          )
  
  raw <- bind_rows(raw, dat)
}

raw %>% 
  group_by(area_des, nom_estacion, cod_estacion) %>% 
  summarize(lat = mean(latitud, na.rm = T),
            lon = mean(longitud, na.rm = T),
            n_row = n(), 
            n_tr = max(unique(censo)), 
            n_srvyed = length(unique(censo))) 

#%>% write_csv(here('data', paste('x_uvc', ctry, "srvys.csv", sep = '_')))
