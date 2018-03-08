#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Diversity indices and analyses
library(vegan)
library(betapart)



#B nestedness and turnover components------------------------------------------------------------------
##Not ideal because not with chao, but still interesting for interpretation
##Getting dataframe ready
betaflies <- 
  cleanflies %>% 
  ungroup() %>% 
  dplyr::select(Locality, Moisture_Regime, Replicate, Species, Abundance) %>% 
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

#Adonis ---------------------------------------------------------
##Dataframe
replicadonis <- 
  replicate_analysis %>% 
  left_join(repliflies)
replicadonis <- 
  replicadonis %>% 
  filter(Richness != 0)
replicadonis$Locality <- as.factor(replicadonis$Locality)
str(replicadonis)
##Names pretty long, hardly visible on plots, so have to shorten them
replicadonis <- 
  cbind(plant_dat[1:65,1], replicadonis[,4:394])
colnames(replicadonis)[which(names(replicadonis) == "plant_dat[1:65, 1]")] <- 
  "Locality"
levels(replicadonis$Moisture_Regime) <- 
  c(levels(replicadonis$Moisture_Regime),
    "M",
    "W")
replicadonis$Moisture_Regime[replicadonis$Moisture_Regime=="Mesic"] <- 
  "M"
replicadonis$Moisture_Regime[replicadonis$Moisture_Regime=="Wet"] <- 
  "W"
levels(replicadonis$Moisture_Regime) <- 
  droplevels(replicadonis$Moisture_Regime)
##Uniting
replicadonis <- 
  replicadonis %>% 
  unite(moisture, Locality, Moisture_Regime, sep =".", remove = F) %>% 
  unite(plot, Locality, Moisture_Regime, Replicate, sep =".", remove = F)
replicadonis$moisture <- 
  as.factor(replicadonis$moisture)
replicadonis$plot <- 
  as.factor(replicadonis$plot)
replicadonis <- 
  data.frame(replicadonis, row.names = replicadonis$plot)

#Adonis with latitude
perm <- 
  how(nperm = 999)
setBlocks(perm) <- 
  with(replicadonis, Locality, moisture, nested.blocks = T)
adonis_latitude <-
  adonis(replicadonis[,18:394]~ 
           Latitude,
          method = "chao",
          permutations= perm,
          data = replicadonis)

#Adonis with envt variables
perm <- 
  how(nperm = 999)
setBlocks(perm) <- 
  with(replicadonis, Locality, moisture, nested.blocks = T)
adonis_envt <-
  adonis(replicadonis[,18:394]~ 
           log(abs(coldest_quarter))*log(t_range)*Moisture_Regime,
         method = "chao",
         permutations= perm,
         data = replicadonis)



#NMDS--------------------------------------------------------------
##NMDS
chao_nmds <- 
  metaMDS(replicadonis[,18:394], 
          k=2, 
          autotransform =T, 
          distance = "chao",
          engine = c("monoMDS", "isoMDS"))
stressplot(chao_nmds)


#Plotting NMDS----------------------------------------------------------------
#Plot together
pdf("chao_NMDS.pdf",
    height = 10,width = 7)
###Using layout
##first part of matrix to say where each plot is
layout(matrix(c(1,1,
                2,3,
                4,5), nrow=3, byrow =T),
       widths=c(5,0.6))
par(mar =c(4, 4, 4, 1))
##making data frame with evferything, hard with vegan
datascores <- 
  as.data.frame(scores(chao_nmds))
datascores$moisture <- 
  replicadonis$Moisture_Regime
datascores$coldest_quarter <- 
  replicadonis$coldest_quarter
datascores$t_range <- 
  replicadonis$t_range
##With sites
plot(NULL, 
     type ="n", 
     xlim =c(-0.6,0.6), 
     ylim =c(-0.4,0.4),
     xlab ="NMDS1",
     ylab ="NMDS2")
sites <- 
  replicadonis$Locality
ordihull(chao_nmds,
         groups=sites,
         draw="polygon",
         lty = 1,
         col="grey60",
         label=T,
         cex=0.5,
         xlim =c(-0.6,0.6), 
         ylim =c(-0.4,0.4))


##Min Winter T
###Use n equally spaced breaks to assign each value 
###to n-1 equal sized bins 
colcol <- 
  cut(sort(datascores$coldest_quarter, decreasing = F), 
      breaks = seq(min(datascores$coldest_quarter),
                   max(datascores$coldest_quarter), 
                   len = 100), 
      include.lowest = TRUE)
###Use indices to select color from vector of n-1 equally spaced colors
colfunc <- 
  colorRampPalette(c("blue", "red"))(100)[colcol]
##Plot it
plot(datascores$NMDS2 ~ 
       datascores$NMDS1, 
     pch = c(16,17)[datascores$moisture], 
     cex =1, 
     col= colfunc,
     xlim =c(-0.6,0.6), 
     ylim =c(-0.4,0.4),
     xlab = "NMDS1", 
     ylab = "NMDS2")
legend("topright", 
       legend = c("Mesic", "Wet"), 
       pch = c(1, 2))
###Color legend
fitcold <- 
  unique(datascores$coldest_quarter)
graphics::image(1,
                sort(fitcold, decreasing = F), 
                t(seq_along(fitcold)), 
                col=colfunc, 
                xlab ="",
                ylab="",
                main ="",
                axes = F)
##Using title to make axis name closer to plot
title(main="", 
      ylab="Mean T of Coldest Quarter(°C)",
      line =0.1)
axis(4)
##With T range
###Colors
colcol <- 
  cut(sort(datascores$t_range, decreasing = F), 
      breaks = seq(min(datascores$t_range),
                   max(datascores$t_range), 
                   len = 50), 
      include.lowest = TRUE)
###Use indices to select color from vector of n-1 equally spaced colors
colfunc <- 
  colorRampPalette(c("blue", "red"))(50)[colcol]
plot(datascores$NMDS2 ~ datascores$NMDS1, 
     pch = c(16,17)[datascores$moisture], 
     cex =1, 
     col= colfunc, 
     xlim =c(-0.6,0.6), 
     ylim =c(-0.4,0.4),
     xlab = "NMDS1", 
     ylab = "NMDS2")
###color legend
rangecold <- 
  unique(datascores$t_range)
graphics::image(1,
                sort(rangecold , decreasing = F), 
                t(seq_along(rangecold )), 
                col=colfunc, 
                xlab ="",
                ylab="",
                axes = F)
title(main="", 
      ylab="Annual T Range(°C)",
      line =0.1)
axis(4)
##closing
dev.off()
.pardefault <- par()









