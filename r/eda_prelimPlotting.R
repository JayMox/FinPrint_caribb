#wrangle script for data ingestion of finprint_Caribb data

library(tidyverse)
library(GGally)
library(gridExtra)
library(here)
scale01 <- function(x){
  #scaling data [0,1]
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

dat <- read_csv(here("data", "summStats_tbl_4.csv")) %>% 
  janitor::clean_names()


#EDA plots
#corr plot shows strong collinearity in functional groups 
ggcorr(dat %>% 
         select_if(is.double) %>% 
         select(-c(x1, trip_year, depth, contains(c("lat", "lon")))),
       method = c("everything", "pearson")) +
  labs(title = "corr matrix of summary statistics, Dec 21")
ggsave("r/plots/eda_corr.pdf")

#scatter plot
dat %>% 
  #select_if(is.double) %>% 
  select(-c(x1, reef_id, region_name, site_name, reef_name, 
            trip_year, depth, contains(c("lat", "lon")))) %>% 
  ggpairs()  
ggsave(here('figs', 'eda_scatter_grouped.pdf')); dev.off()
#collinearity amongst functional groups 

#bar plots
dat %>% 
  mutate(id = paste(reef_name, reef_id, sep = "_")) %>% 
  mutate(id = fct_reorder(id, meso_sharks, .desc = T)) %>% 
  select(-x1, -reef_id, -region_name, -location_name,
         -reef_name, -trip_year, -depth, -contains(c("lat", "lon")),
         -fish_richness) %>% 
  gather(key = "group", "density", -coral, -algae, -other, -id, -site_name) %>% 
  #arrange up some re-ordering
  mutate(group = fct_relevel(group, unique(group))) %>% 
  mutate(shk = ifelse(str_detect(tolower(group), "sharks|rays"), "elasmo", "other"), 
         #use a dodgey vert dodge for viz
         density = ifelse(shk == "elasmo", density, 0-density)) %>% 
 ggplot() + geom_col(aes(x = id, y = density, fill = group, group = id), position = "dodge2") + 
  scale_fill_manual(values = c('#d73027','#f46d43','#fdae61','#d9ef8b','#a6d96a','#e0f3f8','#abd9e9','#74add1','#4575b4', '#111111')) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "relative density of group by site") + 
  coord_flip()
  

##site by site
dat %>% 
  mutate(id = paste(reef_name, reef_id, sep = "_")) %>% 
  mutate(id = fct_reorder(id, meso_sharks, .desc = T)) %>% 
  select(-x1, -reef_id, -region_name, -location_name,
         -reef_name, -trip_year, -depth, -contains(c("lat", "lon")),
         -fish_richness) %>% 
  gather(key = "group", "density", -coral, -algae, -other, -id, -site_name) %>% 
  #arrange up some re-ordering
  mutate(group = fct_relevel(group, unique(group))) %>% 
  mutate(shk = ifelse(str_detect(tolower(group), c("sharks|rays")), "elasmo", "other"), 
         #use a dodgey vert dodge for viz
         density = ifelse(shk == "elasmo", density, 0-density)) %>% 
  ggplot() + geom_col(aes(x = group, y = density, fill = group), position = "dodge2") + 
  scale_fill_manual(values = c('#d73027','#f46d43','#fdae61','#d9ef8b','#a6d96a','#e0f3f8','#abd9e9','#74add1','#4575b4', "#000000")) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  #coord_flip() + 
  facet_wrap(~id, scales = "fixed")

#histogram & density plots
dat %>% 
  select(-x1, -reef_id, -region_name, -location_name,
         -reef_name, -trip_year, -depth, -contains(c("lat", "lon")),
         -fish_richness) %>% 
  gather(key = "group", "density", -coral, -algae, -other, -site_name) %>% 
  mutate(group = fct_relevel(group, unique(group))) %>% 
  ggplot() + geom_histogram(aes(x = density)) + facet_wrap(~group) + 
  theme_bw()

#heatmap
p <- dat %>% 
  mutate(id = paste(reef_id, site_name, reef_name)) %>% 
  mutate(id = fct_reorder(id, meso_sharks)) %>% 
  select(-x1, -reef_id, -region_name, -location_name,
         -reef_name, -trip_year, -depth, -contains(c('lat','lon')),
         -fish_richness) %>% 
  gather(key = "group", "density", 
         -coral, -algae, -other, -site_name, -id) %>% 
  ggplot() + 
  geom_tile(aes(x = fct_relevel(group, unique(group)), 
                y = id, fill = sqrt(density))) + 
  labs(x="", y="") + 
  scale_fill_gradient2(low = '#ffeda0',high = '#f03b20') +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  labs(title = "heat map of fxn group density by site")
dat %>%
  mutate(id = paste(reef_id, site_name, reef_name)) %>% 
  mutate(id = fct_reorder(id, meso_sharks)) %>% 
  ggplot() + 
  geom_point(aes(x = coral, y = algae, 
                 size = sqrt(meso_sharks), col = sqrt(meso_sharks))) + 
  scale_color_gradient2(low = '#ffeda0',high = '#f03b20') + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_bw()

#lollipop approach?
#maybe scale these?
dat %>% 
  mutate(id = paste(reef_id, site_name, reef_name)) %>% 
  mutate(id = fct_reorder(id, meso_sharks)) %>% 
  select(id, algae, coral, fish_richness) %>% 
  gather(key = "group", "density", -id, -fish_richness) %>% 
  ggplot() + 
  geom_point(aes(x = id, y = ifelse(group == "algae", 0-density, density),
                 size = log10(fish_richness), color = log10(sqrt(fish_richness)))) +
  geom_line(aes(x = id, y = ifelse(group == "algae", 0-density, density))) +
  scale_color_gradient2(low = '#ffeda0',high = '#f03b20') + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_flip() + guides(size = F, color = F) + 
  labs(title = "algae:coral ratios, size = Fish Richness, desc Sharks", 
       y = "algae:coral ratio")
ggsave(here("figs", "eda_lollipop_coralAlgae.png"))


###########3
#pyramid assemblage approach, ranked by all sharks

dat %>% 
  mutate(id = paste(reef_id, site_name, reef_name)) %>% 
  mutate(id = fct_reorder(id, all_sharks)) %>% 
  select(-x1, -reef_id, -region_name, -location_name,
         -reef_name, -trip_year, -depth, -contains(c('lat','lon'))) %>% 
  select(-contains(c('ray', 'coral', 'algae', 'other')), -fish_richness, -apex_sharks, -all_sharks) %>% 
  gather(key = 'group', 'density', -id, -site_name) %>% 
  ggplot() + 
  geom_col(aes(x =  fct_relevel(group, 
                               "meso_sharks", "planktivores", "invertivores", "herbivores",
                               "omnivores", "piscivores", "carnivores"), 
               y = density, fill = group)) +
  scale_fill_manual(values = c('#4d4d4d', '#d53e4f','#fc8d59',
                               '#fee08b','#e6f598',
                               '#99d594','#3288bd'))+
  guides(fill = "none") + 
  facet_wrap(~id) +
  labs(title = "assemblage by site, ranked by mesoShk", x = "trophic group") +
  theme_classic() + coord_flip()
ggsave(here('figs', 'eda_Assmblg_bySite.png'))

#study map
#trying this: https://semba-blog.netlify.app/06/13/2020/plots-in-interactive-maps-with-r/
library(leaflet)
library(leaflet.minicharts)
library(sf)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dat <- st_as_sf(x = dat, coords = c("min_lon", "min_lat"), crs = projcrs)

#make a basemap
tilesURL = "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
(basemap = leaflet(width = "100%", height = "800px") %>%
  addTiles(tilesURL))
#???
sdat = dat %>% 
  janitor::clean_names() %>%  
  select(x1, apex_sharks, meso_sharks, rays) %>% 
  st_transform(4326)
dat.tb = sdat %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename(lon = 1, lat = 2) %>% 
  bind_cols(sdat %>% st_drop_geometry()) %>%
  mutate(total = apex_sharks+meso_sharks+rays)
#palette brew
colors <- c("#edf8b1", "#7fcdbb", "#2c7fb8")
#map 'er up
basemap %>%
  addMinicharts(lng = sdat.tb$lon, 
                lat = sdat.tb$lat, 
                type = "pie", 
                chartdata = sdat.tb[, c("apex_sharks", "meso_sharks", "rays")], 
                colorPalette = colors, 
                width = 60 * sqrt(sdat.tb$total) / sqrt(max(sdat.tb$total)), 
                transitionTime = 0)


#other ecosystem components
sdat = dat %>% 
  janitor::clean_names() %>%  
  select(x1, carnivores, piscivores, omnivores, invertivores, planktivores,
         herbivores) %>% 
  st_transform(4326)
dat.tb = sdat %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename(lon = 1, lat = 2) %>% 
  bind_cols(sdat %>% st_drop_geometry()) %>%
  mutate(total = carnivores + piscivores + omnivores + 
                 invertivores + planktivores + herbivores)
#palette brew
colors <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850")
#map 'er up
basemap %>%
  addMinicharts(lng = dat.tb$lon, 
                lat = dat.tb$lat, 
                type = "pie", 
                #type = "polar-area",
                chartdata = dat.tb[, c("carnivores", "piscivores", "omnivores",
                                        "invertivores", "planktivores", "herbivores")], 
                colorPalette = colors, 
                width = 60 * sqrt(dat.tb$total) / sqrt(max(dat.tb$total)), 
                transitionTime = 0)

#coral v algae
sdat = dat %>% 
  janitor::clean_names() %>%  
  select(x1, coral, algae, other) %>% 
  st_transform(4326)
dat.tb = sdat %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename(lon = 1, lat = 2) %>% 
  bind_cols(sdat %>% st_drop_geometry()) %>%
  mutate(total = coral + algae + other)
#palette brew
colors <- c("#d73027", "#d9ef8b", "#fee08b")
#map 'er up
basemap %>%
  addMinicharts(lng = dat.tb$lon, 
                lat = dat.tb$lat, 
                #type = "pie", 
                type = "polar-area",
                chartdata = dat.tb[, c("coral", "algae", "other")], 
                colorPalette = colors, 
                width = 60 * sqrt(dat.tb$total) / sqrt(max(dat.tb$total)), 
                transitionTime = 0)

