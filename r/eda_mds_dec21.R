#this is a preliminary script for arranging a quantitative analysis
#aim is to explore a logical process of ordination, 
#design is ordination of fish assemblage, correlate axis of env (incl sharks)
#& be transferred into a markdown doc
#see this overview of multivar analysis: https://dmcglinn.github.io/quant_methods/lessons/multivariate_models.html

library(tidyverse)
library(reshape2)
library(vegan)
library(vegan3d)
library(ggvegan)
library(here)

# sc01 = T; source("./r/pipeline_dataWeave.R")
sc01 = T; source(here("r", "pipeline_dataWeave.R"))

df <- df %>% select(-site_vars, 
                    -contains(c('all_sharks', 'apex_sharks', 'rays')),
                    )
#replace some na's w/ guesses
df <- df %>% mutate(protection_status = ifelse(is.na(protection_status), c("open", "closed"), protection_status),
                    market_gravity = ifelse(is.na(market_gravity), c(55, 83, 35), market_gravity),
                    reef_complexity = ifelse(is.na(reef_complexity), 1.5, reef_complexity))
#rescale those
df <- df %>% mutate(market_gravity.sc = scale01(market_gravity), 
                    reef_complexity = scale01(reef_complexity))


#rank index suggests gower when using derived mean assemblage data
rankindex(
  #teleost data
  df %>% select(max_lat, contains("vore"), fishrich.lt),
  #sharks, benthic data
  df %>% select(max_lat, contains("shark"), coral, algae, other)
)
rankindex(df %>% select(max_lat, contains("vores.sc"), fishrich.lt),
          df %>% select(max_lat, meso_sharks.sc, fishrich.lt.sc))
#scaled data shows much better rank correlation

#choose a dimensionality, using euclidean b/c of means
ord1 <- metaMDS(df %>% select(max_lat, meso_sharks, contains("vore"), fish_richness),
                 distance = "euc", k = 1, try = 30, 
                 autotransform = TRUE, 
                 plot = TRUE)
ord2 <- metaMDS(df %>% select(max_lat, meso_sharks, contains("vore"), fish_richness),
                distance = "euc", k = 2, try = 30, 
                autotransform = TRUE, 
                plot = TRUE)
ord3 <- metaMDS(df %>% select(max_lat, meso_sharks, contains("vore"), fishrich.lt),
                distance = "euc", k = 3, try = 30, 
                autotransform = FALSE, 
                plot = TRUE)
ord3.sc <- metaMDS(df %>% select(max_lat, meso_sharks.sc, contains("vores.sc"), fishrich.lt.sc),
                distance = "euc", k = 3, try = 30, 
                autotransform = FALSE, 
                plot = TRUE)

ord4 <- metaMDS(df %>% select(max_lat, meso_sharks, contains("vore"), fish_richness),
                distance = "euc", k = 4, try = 30, 
                autotransform = TRUE, 
                plot = TRUE)
ord5 <- metaMDS(df %>% select(max_lat, meso_sharks, contains("vore"), fish_richness),
                distance = "euc", k = 5, try = 30, 
                autotransform = TRUE, 
                plot = TRUE)
ord6 <- metaMDS(df %>% select(max_lat, meso_sharks, contains("vore"), fish_richness),
                distance = "euc", k = 6, try = 30, 
                autotransform = TRUE, 
                plot = TRUE)
data.frame(k = 1:6, stress = c(ord1$stress, ord2$stress, ord3$stress,
                               ord4$stress, ord5$stress, ord6$stress)) %>% 
  ggplot(aes(x = k, y = stress)) + geom_point() + geom_line() + 
  scale_x_continuous(limits = c(0, 6), breaks = seq(0,6,1)) + 
  theme_classic() + labs(title="nMDS stress across dimensions")
ggsave("eda_nMDS_stressVdims.png")


#GGVEGAN AUTO PLOT



######
##UPDATE THESE W/ NEW INSIGHTS FROM STRESS PlotxDims
##AND OTHER TIPS HERE: https://mb3is.megx.net/gustame/dissimilarity-based-methods/nmds
##e.g., Minimising overlap between points in an ordination is helpful in 
#interpretation, thus a principal components analysis  may be applied to 
#NMDS axis scores. This effectively rotates an NMDS solution to ensure the 
#first NMDS axis lies along the direction of maximum scatter.
#in "Post-analysis section"
#also has primers on Procrustes (compare diff nMDS) & anosim (test if groups have diff dissimilarity)
#

#NMDS (w/ monoMDS as default)
ord.euc <- metaMDS(df %>% select(max_lat, contains("vore"), fish_richness),
                 distance = "euc", k = 3, try = 50, 
                 autotransform = TRUE, 
                 plot = TRUE)
#stress 0.08.. too high
ord.bray <- metaMDS(df %>% select(max_lat, contains("vore"), fish_richness),
               distance = "bray", k = 3, try = 50,
               autotransform = TRUE, 
               plot = TRUE)
#lower stress w/ euc
ord.gow <- metaMDS(df %>% select(max_lat, contains("vore"), fish_richness),
               distance = "gow", k = 3, try = 50,
               autotransform = TRUE, 
               plot = TRUE)
#0.28 stress after 50
stressplot(ord.euc)
ord.euc$species #variable scores
df %>% bind_cols(as_tibble(ord.euc$points)) #add MDS coords to df



#apply env fit
(bio <- envfit(ord.euc, 
              env = df %>% select(meso_sharks, 
                                  coral, algae, other, 
                                  market_gravity), 
              na.rm=T))
(anthro <- envfit(ord.euc, 
                 env = df %>% select(meso_sharks, 
                                     protection_status, market_gravity,
                                     reef_complexity, reef_type_guess),
                 na.rm = T))
#IMPT: from gustame blow
#Avoid relating environmental variables (or other explanatory variables) to 
#the distances in an NMDS ordination. As an NMDS ordination is typically only
#an approximate representation of a distance matrix, consider using the 
#distance matrix itself. Consider distance-based redundancy analysis or the 
#Mantel test.
#another lengthy how to page: https://archetypalecology.wordpress.com/2018/02/18/non-metric-multidimensional-scaling-nmds-what-how/

#3d plot
ordiplot3d(ord.euc, display = c("sites"), 
           scaling = "symetric", angle = 65,
           ax.col = "blue")


####RDA 
#replace NAs in market gravity with guesses.
#just gunna use generic guesstimate from glovers reef data
df <- df %>% mutate(market_gravity = 
                      ifelse(is.na(market_gravity),
                           83.5, 
                           market_gravity))
rda_tree <- rda(
  ##SHARKS as environment
  # df %>% select(contains('vores.sc')),
  # df %>% select(meso_sharks.sc, coral, algae, other, market_gravity, max_lat)
  ##SHARKS AS Community
  df %>% select(contains('vores.sc'), meso_sharks.sc),
  df %>% select(coral, algae, other, market_gravity, max_lat)
)
RsquareAdj(rda_tree)
#Inertia is another name for variation or variance in this case. 
#“Total” refers to total variance, “Constrained” refers to the amount of 
#variance explained by the explanatory variables, “Unconstrained” refers to 
#the residual variance. Constrained + Unconstrained = Total. An R2 statistic 
#can be derived simply as Constrained / Total. The function RsquareAdj 
#computes R2 and R2-adjusted. The variable “Rank” indicates the number of 
#variables included. The eigenvalues are displayed for both the constrained 
#and unconstrained axes. In this context these eigenvalues indicate how 
#much variance each of the axes contribute to.
plot(rda_tree, type='n', scaling=1)
orditorp(rda_tree, display='sp', cex=0.5, scaling=1, col='blue')
text(rda_tree, display='bp', col='red')
title(paste0('rda biplot, fish comm + sharks ~ env, r2= ', 
             round(RsquareAdj(rda_tree)$adj.r.squared, 4)))


#######CCA
#maybe use a CCA or PCA as starting condition of nMDS??
##appropriate for humpshaped, whereas RDA is linear
comm <- df %>% select(contains('vores.sc'))
env <- df %>% 
  select(meso_sharks.sc, coral, algae, other, market_gravity, max_lat) #%>% 
  #mutate(market_gravity = ifelse(is.na(market_gravity), 55, market_gravity))
cca_tree <- cca(comm ~ ., data = env)
RsquareAdj(cca_tree)

#hypothesis testing
anova(cca_tree, permutations = 999)
#tests overall model fit against ranodmized data
anova(cca_tree, by='margin', permutations = 999)
#tests partial effects of vars
#LATITUDE & ALGAE FIND SIGNIFICANCE; 
#NEAR SIGNIFICANCE FOR ALL BENTHIC VARS

plot(cca_tree, type='n', scaling=1)
orditorp(cca_tree, display='sp', cex=0.5, scaling=1, col='blue')
text(cca_tree, display='bp', col='red')
title(paste0('cca biplot, fish comm ~ env + sharks, r2= ', 
             round(RsquareAdj(cca_tree)$adj.r.squared, 4)))




