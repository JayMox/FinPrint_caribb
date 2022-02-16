#ordination work thru
#based on this https://www.youtube.com/watch?v=OMrtxobDhrM
#intention is to test a ord w/ & w/o sharks
#THIS IS DUMB APPROACH CAN"T COMPARE TWO PCAs

library(tidyverse)
library(reshape2)
library(vegan)
library(vegan3d)
library(ggvegan)
setwd("/Users/jhmoxley/Documents/Biologia & Animales/[[Consulting]]/Finprint_carib/")
sc01 = T; source("./r/pipeline_dataWeave.R")
#replace some na's w/ guesses
df <- df %>% mutate(protection_status = ifelse(is.na(protection_status), c("open", "closed"), protection_status),
                    market_gravity = ifelse(is.na(market_gravity), c(55, 83, 35), market_gravity),
                    reef_complexity = ifelse(is.na(reef_complexity), 1.5, reef_complexity))
#rescale those
df <- df %>% mutate(market_gravity.sc = scale01(market_gravity), 
                    reef_complexity.sc = scale01(reef_complexity),
                    max_lat.sc = scale01(max_lat))

#########UNCONSTRAINED, FINFISH & MESO_SHARKS ONLY
#pca of scaled data
pca <- rda(df %>% select(max_lat.sc, meso_sharks.sc, contains('vores.sc')))
data.frame(dim = 1:8,
           eig = pca$CA$eig) %>% 
  mutate(percEx = eig/pca$tot.chi) %>% 
  ggplot() + 
  geom_point(aes(x = dim, y = eig)) + 
  geom_line(aes(x = dim, y = eig)) + 
  geom_col(aes(x = dim, y = percEx), alpha = 0.3) + 
  theme_classic() + labs(title = "kruskal elbow of finfish/elasmo pca (cols are %explained") + 
  guides(fill = 'none')
#88% of unconstrained variance explained in first 3 

