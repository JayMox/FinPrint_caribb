#wrngl of bruv westAtl data
#JHMoxley Feb 22

bruv <- read_csv(here('data/stitch', 'src_BRUV_westernAtl.csv'))
sc <- bruv%>% 
  janitor::clean_names() %>% 
  #stamp FPid
  mutate(site_reefcode = paste(reef_id, trip_year, sep = "_")) %>% 
  select(country = location_name, 
         site_name, reef_name, 
         site_reefcode, 
         #detections
         maxn, common_name, animal_id, event_time_mil,
        #id data
         reef_id, set_id, master_set_id, set_code,
         ) %>% 
  mutate(
    #effort stats are kinda warped
    eff_nsamples = NA, #this comes from # of bruv deployments incl ZEROS
    eff_srveyed = length(unique(set_id)) 
    ) %>% 
  filter(site_reefcode %in% unique(srvy$site.reefcode))


#####THEN WHAT? NEED EFFORT DATA
  
sc %>% group_by(site_reefcode) %>% 
  summarize(n = n(),
            n.reef = length(unique(reef_id)),
            n.setid = length(unique(set_id)),
            n.master = length(unique(master_set_id)),
            n.setcode = length(unique(set_code)), 
            d.setid = sum(duplicated(set_id)),
            d.master = sum(duplicated(master_set_id)),
            d.setcode = sum(duplicated(set_code))
            
            )
#often more setcode dupes, than others, setid and master_setid have identical dupes


  bruv %>% select(set_code, set_id, master_set_id) %>% 
    gather(type, val) %>% 
    ggplot() + geom_bar(aes(x = val)) + facet_wrap(~type)
  
  bruv %>% 
    filter(set_code %in% idx$set_code) %>% 
    arrange(set_code) %>% 
    select(trip_code, maxn, set_code, reef_id, set_id, 
           master_set_id, animal_id, event_time_mins, set_lat, 
           set_long, depth, ) %>%  view
  