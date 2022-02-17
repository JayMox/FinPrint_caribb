

  
  #initalize lkup_surveyeffort from BRUV data
   read_csv(here('data', 'summStats_tbl_4.csv')) %>% 
    janitor::clean_names() %>%
    select(reef_id, trip_year, location_name, site_name, reef_name) %>% 
    mutate(
           ##sampling site meta data
           country = location_name,
           site = site_name, 
           reef_name = paste(site_name, reef_name, trip_year, sep = "_"),
           reef_code = reef_id, 
           lat = NA, lon = NA, 
           
           ##srvy Params
           srvy.method = "bruv", #OR uvc.f[ish] uvc.b[enthic]
           srvy.taxa = "elasmo", #elasmo, fish, benthos
           d2bruv = NA, #dist to bruv [unit]
           fpid = NA, #assigned bruv id based on proximity
           n.obs = NA, #n of samples in src data
           
           ##effort params
           eff.sites = NA, #number of transect samples
           eff.srvyed = NA, #number of srveyed 
           eff.pue = NA, #area or mins PerUnitEffort
           eff.unit = NA, #m2 or perMin(??)
           
           ##file tracking mgmt
           #is it stitched in[t/f], raw src [file], raw out [file]
           stitch.ed = NA,
           stitch.in =  "BRUVmaxn_elasmobranch_observations_all.csv", 
           stitch.out = "src_BRUV_westernAtl.csv",
           
           ) %>% 
    select(-location_name, -site_name) %>% 
     write_csv(here('data', 'lkup_srvyparams.csv')
  

   