#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Diversity indices and analyses
library(vegan)
library(betapart)



#Adonis ---------------------------------------------------------
##Dataframe
replicadonis <- 
  replicate_analysis %>% 
  left_join(sitemap[,1:2], by = "Locality") %>% 
  left_join(repliflies) %>% 
  filter(Richness != 0)
str(replicadonis)


##Uniting
replicadonis <- 
  data.frame(replicadonis, row.names = replicadonis$plot)

#Adonis with envt variables
adonis_envt <-
  adonis(replicadonis[,19:349]~ 
           coldest_quarter*t_range + Moisture_Regime,
         method = "chao",
         permutations= 2000,
         strata = replicadonis$Locality,
         data = replicadonis)



#NMDS--------------------------------------------------------------
##NMDS
chao_nmds <- 
  metaMDS(replicadonis[,19:394], 
          k=2, 
          trymax = 100,
          autotransform =T, 
          distance = "chao",
          engine = c("monoMDS", "isoMDS"))
stressplot(chao_nmds)
##making data frame with everything for plotting, hard with vegan
datascores <- 
  as.data.frame(scores(chao_nmds))
datascores$moisture <- 
  replicadonis$Moisture_Regime
datascores$coldest_quarter <- 
  replicadonis$coldest_quarter
datascores$t_range <- 
  replicadonis$t_range

#Plotting NMDS----------------------------------------------------------------
#Plot together
pdf("chao_NMDS.pdf",
    height = 10,width = 8)
###Using layout
##first part of matrix to say where each plot is
layout(matrix(c(1,1,
                2,3,
                4,5), nrow=3, byrow =T),
       widths=c(5,0.6))
par(mar =c(4, 3, 3, 2))


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
  colorRampPalette(c("grey85", "grey13"))(100)[colcol]
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
  colorRampPalette(c("grey85", "grey13"))(50)[colcol]
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
par(mfrow=c(1,1))











#Second NMDS -------------------------------------------------------------
##With sites
tiff("NMDS_sites.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
plot(NULL, 
     type ="n", 
     xlim =c(-0.6,0.6), 
     ylim =c(-0.4,0.4),
     xlab ="NMDS1",
     ylab ="NMDS2")
sites <- 
  replicadonis$LOC
ordihull(chao_nmds,
         groups=sites,
         draw="polygon",
         lty = 1,
         col="grey60",
         label=T,
         cex=0.3,
         xlim =c(-0.6,0.6), 
         ylim =c(-0.4,0.4))

dev.off()
##With replicates
tiff("NMDS_replicates.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
plot(datascores$NMDS2 ~ 
       datascores$NMDS1, 
     pch = 16, 
     col = c("grey30", "grey80")[datascores$moisture],
     cex =1, 
     xlim =c(-0.6,0.6), 
     ylim =c(-0.4,0.4),
     xlab = "NMDS1", 
     ylab = "NMDS2")
legend("topright", 
       legend = c("Mesic", "Wet"), 
       pch = 16,
       col = c("grey30", "grey80"),
       cex =0.7)
climarrows <- 
  envfit(chao_nmds, 
       replicadonis[,14:15],
       permutations = 2000,
       display= "site")
ordiArrowMul(scores(climarrows, display ="vectors"), rescale = F)
par(mfrow=c(1,1))
plot(climarrows, 
col ="black",
labels = c("Min. T of coldest quarter", 
           "T annual range"),
cex = 0.5)
dev.off()
#B nestedness and turnover components for incidence------------------------------------------------------------------
#Getting dataframe ready
betaflies <- 
  cleanflies %>% 
  ungroup() %>% 
  dplyr::select(Locality, Moisture_Regime, Replicate, Species, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate, Species) %>% 
  summarise_all(funs(sum))
###Making incidence
betaflies$Abundance <- 
  1
betaflies <- 
  betaflies %>% 
  spread(key = Species, Abundance, fill =0)
###row names
betaflies <- 
  betaflies %>% 
  unite(plot, Locality, Moisture_Regime, Replicate, sep ="_", remove = F)
betaflies <-  
  data.frame(betaflies, row.names = betaflies$plot)




###Compute with jaccard, because not nterested in dominance
jaccard <- 
  beta.pair(betaflies[,5:381], index.family = "jaccard")

###Plot histograms
tiff("hist_turnover.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
hist(jaccard$beta.jtu, 
     xlab= "Turnover",
     main = NULL,
     col = "grey50",
     ylim = c(0,2000))
dev.off()
tiff("hist_nestedness.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
hist(jaccard$beta.jne,
     xlab= "Nestedness",
     main = NULL,
     col = "grey50",
     ylim = c(0,2000))
dev.off()
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
###nestedness component not significant


#Plot it
##setup
pdf("jacincidence_NMDS.pdf",
    height = 10,width = 8)
par(mfrow =c(3,2))

##Turnover
plot(NULL, 
     type ="n", 
     xlab ="NMDS1",
     ylab ="Turnover",         
     main ="With localities",
     xlim =c(-1,1), 
     ylim =c(-0.5,0.5))
sites <- 
  replicadonis$Locality
ordihull(jtu_nmds,
         groups=sites,
         draw="polygon",
         lty = 1,
         col="grey60",
         label=T,
         cex=0.5)
###making data frame to show wet/mesic and plot
datascores <- 
  as.data.frame(scores(jtu_nmds))
datascores$moisture <- 
  betaflies$Moisture_Regime
plot(datascores$NMDS2 ~ 
       datascores$NMDS1, 
     pch = c(1,2)[datascores$moisture], 
     cex =1, 
     xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "NMDS1", 
     ylab = "NMDS2",
     main = "With replicates")
legend("topright", 
       legend = c("Mesic", "Wet"), 
       pch = c(1, 2))

##Nestedness
plot(NULL, 
     type ="n", 
     xlab ="NMDS1",
     ylab ="Nestedness",
     xlim =c(-1,1), 
     ylim =c(-0.5,0.5))
ordihull(jne_nmds,
         groups=sites,
         draw="polygon",
         lty = 1,
         col="grey60",
         label=T,
         cex=0.5)

###making data frame to show wet/mesic and plot
datascores <- 
  as.data.frame(scores(jne_nmds))
datascores$moisture <- 
  betaflies$Moisture_Regime
plot(datascores$NMDS2 ~ 
       datascores$NMDS1, 
     pch = c(1,2)[datascores$moisture], 
     cex =1, 
     xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "NMDS1", 
     ylab = "NMDS2")

##Total
plot(NULL, 
     type ="n", 
     xlab ="NMDS1",
     ylab ="Nestedness and Turnover",
     xlim =c(-1,1), 
     ylim =c(-0.5,0.5))
ordihull(jac_nmds,
         groups=sites,
         draw="polygon",
         lty = 1,
         col="grey60",
         label=T,
         cex=0.5)

###making data frame to show wet/mesic and plot
datascores <- 
  as.data.frame(scores(jac_nmds))
datascores$moisture <- 
  betaflies$Moisture_Regime
plot(datascores$NMDS2 ~ 
       datascores$NMDS1, 
     pch = c(1,2)[datascores$moisture], 
     cex =1, 
     xlim =c(-1,1), 
     ylim =c(-0.5,0.5),
     xlab = "NMDS1", 
     ylab = "NMDS2")

##closing
dev.off()
par(mfrow =c(1,1))



