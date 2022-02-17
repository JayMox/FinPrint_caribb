#wrangle colombia UVC fish spp counts
#JHMoxley, Feb 22

source(here('r', 'fxn_library.R'))
ctry <- "colombia"
message(paste("sourcing raw uvc counts from", ctry))
src <- c('src_Fish_Ab_Rosario.csv',
         'src_Fish_Ab_San_Bernardo.csv')
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

####
#effort
####
effort <- raw %>% 
    mutate(country = "Colombia", site.zone = area_des, 
      site.reef = nom_estacion, site.reefcode = cod_estacion) %>% 
    group_by(country, site.zone, site.reef, site.reefcode) %>% 
    summarize(lat = mean(latitud, na.rm = T),
              lon = mean(longitud, na.rm = T),
              year = unique(yr),
              n.obs = n(), #number species on a transect
              eff.nsites = max(unique(censo)), 
              eff.nsrvyed = length(unique(censo)),
              stitch.in = src) %>% 
    mutate(
      #wrngl into lkup form
      #survey params
      srvy.type = "uvc.f", srvy.method = "belt", srvy.taxa = "fish", 
      #bruv assignment params
      d2bruv = NA,fpid = NA, 
      #effort
      eff.pue = 60, eff.unit = "m2",  #uvc tubes
      stitch.ed = NA, 
      stitch.out = paste("src", stitch.in, sep = "_"), 
      dat.partner = "SIMAC"
    ) %>% 
  distinct() #it's getting dupes for some stupid reason
#message(paste("effort data from " ))

######
##data
######
df <- raw %>% ungroup() %>% 
  select(-ubicacion_especifica, -fecha) %>% 
  ungroup() %>% 
  group_by(cod_estacion, especie, censo) %>% 
  summarize(sci.name = unique(especie_des), 
            site.reef = unique(nom_estacion),
            count = sum(cantidad, na.rm = T),
            year = 2016, #lubridate::year(fecha), 
            ) %>% 
  mutate(site.reefcode = cod_estacion, spp.code = especie, transect = censo) %>% 
  ungroup() %>% select(-cod_estacion, -especie, -censo) %>% 
  merge(
    effort %>% ungroup() %>% select(site.reefcode, n.obs, 
                                    eff.nsites, eff.nsrvyed, eff.pue), 
    by = 'site.reefcode', all.x = T
  ) #%>% nrow

out <- list('fish.colo' = df, 'uvc.f.effort.colo' = effort)
rm(list = c('dat', 'df', 'effort', 'raw'))
return(out)
