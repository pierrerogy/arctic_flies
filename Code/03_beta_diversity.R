#Beta diversity analyses

#Adonis ---------------------------------------------------------
#Data
replicadonis <- 
  replicate_analysis[1:65,1:11] %>% 
  left_join(sitemap[,2:3]) %>% 
  left_join(repliflies[,c(1, 6:338)])
##Double check
str(replicadonis)

#Make plot row names
replicadonis <- 
  data.frame(replicadonis, 
             row.names = replicadonis$plot)

#Adonis
adonis_envt <-
  adonis(replicadonis[,13:345]~ 
           coldest_quarter*t_range + Moisture_Regime,
         method = "chao",
         permutations= 2000,
         strata = replicadonis$Locality,
         data = replicadonis)



#NMDS--------------------------------------------------------------
##Scaling
chao_nmds <- 
  metaMDS(replicadonis[,13:345], 
          k=2, 
          trymax = 100,
          autotransform =T, 
          distance = "chao",
          engine = c("monoMDS", "isoMDS"))
##Stress
chao_nmds$stress
stressplot(chao_nmds)


#Plotting NMDS, and saving in tiff format -------------------------------------------------------------
#With replicates as dots and environmental vectors
tiff("NMDS_replicates.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
##Create Data for manual plotting (easier to do than vegan options)
###Extract NMDS scores
datascores <- 
  as.data.frame(scores(chao_nmds))
###Add variables of interest
datascores$moisture <- 
  replicadonis$Moisture_Regime
datascores$coldest_quarter <- 
  replicadonis$coldest_quarter
datascores$t_range <- 
  replicadonis$t_range
datascores$Ecoclimatic_zone <- 
  replicadonis$Ecoclimatic_zone
##Plot points
plot(datascores$NMDS2 ~ 
       datascores$NMDS1, 
     pch = c(15,16,17)[datascores$Ecoclimatic_zone], 
     col = c("grey30", "grey80")[datascores$moisture],
     cex =1, 
     xlim =c(-0.2,0.2), 
     ylim =c(-0.12,0.12),
     xlab = "NMDS1", 
     ylab = "NMDS2")
legend("topright", 
       legend = c("Mesic", "Wet"), 
       pch = 16,
       col = c("grey30", "grey80"),
       cex =0.7)
legend("topleft",
       pch = c(16,17,15),
       col = "grey34",
       legend = c("Northern Boreal", "Subarctic", "High Arctic"),
       cex = 0.7)
title("(a)",
      adj = 0)
##Add climate arrows
###Compute arrow values
climarrows <- 
  envfit(chao_nmds, 
       replicadonis[,10:11],
       permutations = 2000,
       display= "site")
###Add to ordination plot
plot(climarrows, 
     col ="black",
     labels = c("", ""))
text(c(-0.16, -0.19),
     c(-0.04, -0.06), 
     c("Mean T ", "T range"),
     cex=0.8)
dev.off()

#With sites as hulls
tiff("NMDS_sites.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)
##Create plot
plot(NULL, 
     type ="n", 
     xlim =c(-0.2,0.2), 
     ylim =c(-0.12,0.12),
     xlab ="NMDS1",
     ylab ="NMDS2")
title("(b)",
      adj = 0)
##Add site for hull names
sites <- 
  replicadonis$LOC
##Add hulls
ordihull(chao_nmds,
         groups=sites,
         draw="polygon",
         lty = 1,
         col="grey60",
         label=T,
         cex=0.45,
         xlim =c(-0.15,0.15), 
         ylim =c(-0.1,0.1))
dev.off()

#Turnover analysis ----------------------------------------
#Data
locaflies <- 
  cleanflies %>% 
  ##Pool all species per locality
  dplyr::select(Ecoclimatic_zone, Locality, Species, Abundance) %>% 
  group_by(Ecoclimatic_zone, Locality, Species) %>% 
  summarise_all(funs(sum)) %>% 
  spread(key=Species, Abundance, fill =0)

#Compute distance
locadist <- 
  vegdist(locaflies[,3:335], 
          method = "chao")

#Analysis of multivariate homogeneity
turnosite <- 
  betadisper(locadist,
             locaflies$Ecoclimatic_zone,
             type = "centroid",
             bias.adjust = T)
##Test the output
anova(turnosite)

#Plot in tiff format
tiff("ecozone_turnover.tiff",
     width = 4800,
     height = 3200, 
     units = "px", 
     res = 800)

plot(turnosite,
     groups = locaflies$Ecoclimatic_zone,
     cex = 1,
     label = F,
     ellipse = T,
     hull = F,
     sub = "",
     main = "",
     col= "grey34",
     pch =c(15,16,17),
     xlim = c(-0.6, 0.6))
legend("topleft",
       pch = c(16,17,15),
       col = "grey34",
       legend = c("Northern Boreal", "Subarctic", "High Arctic"),
       cex = 0.7)

dev.off()



