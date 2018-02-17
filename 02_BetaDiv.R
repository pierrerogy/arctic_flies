#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Diversity indices and analyses
library(vegan)
library(betapart)
##Modelling
library(lme4)
library(nlme)


# B nestedness and turnover components------------------------------------------------------------------
##Not ideal because not with chao, but still interesting for interpretation
##Getting dataframe ready
betaflies <- 
  cleanflies %>% 
  ungroup() %>% 
  select(Locality, Moisture_Regime, Replicate, Species, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate, Species) %>% 
  summarise_all(funs(sum))
###Making presence/absence
betaflies$Abundance <- 1
betaflies <- 
  betaflies %>% 
  spread(key = Species, Abundance, fill =0)
###row names
betaflies <- 
  betaflies %>% 
  unite(site, Locality, Moisture_Regime, Replicate, sep ="_", remove = F)
betaflies <-  data.frame(betaflies[,2:381], row.names = betaflies$site)

###Compute with jaccard
jaccard <- beta.pair(betaflies[,4:380], index.family = "jaccard")
###NMDS it
jtu_nmds <- 
  metaMDS(jaccard$beta.jtu, k = 2)
stressplot(jtu_nmds)
jne_nmds <- 
  metaMDS(jaccard$beta.jne, k = 2)
stressplot(jne_nmds) ##Stress >0.3, not significant
jac_nmds <- 
  metaMDS(jaccard$beta.jac, k = 2)
stressplot(jac_nmds)
###Not necessary to plot, as nestedness component not significant
###Hence better to stick with chao

# Chao NMDS ---------------------------------------------------------------
##not with presence/absence, so turning back to repliflies
repliflies <- 
  repliflies %>% 
  unite(site, Locality, Moisture_Regime, Replicate, sep ="_", remove = F)
repliflies <-  data.frame(repliflies, row.names = repliflies$site)
##chao
chao <- vegdist(repliflies[,5:381], method = "chao")
##NMDS
chao_nmds <- metaMDS(chao, k=2, autotransform =F,vplot = TRUE)
stressplot(chao_nmds) ###Not that good either, but could be worse I guess


##Plot it
pdf("chao_NMDS.pdf",height = 20,width = 20)
par(mfrow =c(2,1))
plot(chao_nmds, type ="n", xlim =c(-1,1), ylim =c(-0.5,0.5))
orditorp(chao_nmds,display="sites",cex=1, pch = NA)
###Cannot do style things with vegan option, so extracting and plotting
datascores <- as.data.frame(scores(chao_nmds))
datascores$moisture <- repliflies$Moisture_Regime
datascores$zone <- repliflies$Ecozone
plot(datascores$NMDS2 ~ datascores$NMDS1, pch = c(16,17)[datascores$moisture], cex =2, 
     col= c('blue','orange','red')[datascores$zone], xlim =c(-1,1), ylim =c(-0.5,0.5),
     xlab = "NMDS1", ylab = "NMDS2")
legend ("topleft", legend = c("Northen Boreal", "Subarctic", "High Arctic"), pch = 15, pt.bg = c('orange','red', 'blue'), col = c('orange','red', 'blue'))
legend("topright", legend = c("Mesic", "Wet"), pch = c(1, 2))
dev.off()
par(mfrow =c(1,1))

##Beta avec jaccard, correle ca avec dist() sur les envt variables
##NMDS


# Mantel Test -------------------------------------------------------------
###Distance matrix for longitude and PCA axes
####Remove empty samples (not accepted in NMDS)
dist_variables <- 
  replicate_climate %>% 
  unite(site, Locality, Moisture_Regime, Replicate, sep ="_", remove = F) %>% 
  select(site, Longitude, Latitude, bio11, bio7)
dist_variables <-  dist_variables[1:65,]
dist_variables <-  data.frame(dist_variables, row.names = dist_variables$site)
dist_variable_matrix <- dist(dist_variables[,2:5])

##vegan
mantel(chao, dist_variable_matrix, permutations = 999)
##ade4
mantel.rtest(chao, dist_variable_matrix, nrepet = 999)
###Significant positive correlation!
