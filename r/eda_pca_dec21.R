#ordination work thru
#based on this https://www.youtube.com/watch?v=OMrtxobDhrM
#intention is to test a ord w/ & w/o sharks
#THIS IS DUMB APPROACH CAN"T COMPARE TWO PCAs

library(tidyverse)
library(reshape2)
library(vegan)
library(vegan3d)
library(ggvegan)
library(here)

sc01 = T; source(here('r', 'pipeline_dataWeave.R'))
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

#extract results
pca.f <- fortify(pca, 1:3)
p12 <- pca.f %>% 
  ggplot() + geom_point(data = subset(pca.f, Score == "sites"), 
                        aes(x = PC1, y = PC2), colour = "black", alpha = 0.5) +
  geom_segment(data = subset(pca.f, Score == "species"),
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow=arrow(length=unit(0.03, "npc"), type = "closed"),
               colour = "darkgray", size = 0.8) + 
  geom_text(data = subset(pca.f, Score == "species"),
            aes(label = Label, x = PC1*1.1, y = PC2*1.1)) +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  xlab(paste0("PC1 (", 
              round(pca$CA$eig[1]/pca$tot.chi, 4)*100, "%var)")) + 
  ylab(paste0("PC2 (", 
              round(pca$CA$eig[2]/pca$tot.chi,4)*100, "%var)")) +
  labs(title = "PCA w sharks, Ax1&2 (above) Ax1&3 (below)") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
p13 <- pca.f %>% 
  ggplot() + geom_point(data = subset(pca.f, Score == "sites"), 
                        aes(x = PC1, y = PC3), colour = "black", alpha = 0.5) +
  geom_segment(data = subset(pca.f, Score == "species"),
               aes(x = 0, y = 0, xend = PC1, yend = PC3), 
               arrow=arrow(length=unit(0.03, "npc"), type = "closed"),
               colour = "darkgray", size = 0.8) + 
  geom_text(data = subset(pca.f, Score == "species"),
            aes(label = Label, x = PC1*1.1, y = PC3*1.1)) +
  geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) + 
  xlab(paste0("PC1 (", 
              round(pca$CA$eig[1]/pca$tot.chi, 4)*100, "%var)")) + 
  ylab(paste0("PC3 (", 
              round(pca$CA$eig[3]/pca$tot.chi,4)*100, "%var)")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
grid.arrange(p12, p12, ncol = 1)
ggsave("eda_PCAax12_ax13.png")

