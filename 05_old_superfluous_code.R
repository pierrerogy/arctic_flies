#Old, superfluous code
devtools::install_github("ropensci/EML", build_vignettes = TRUE)

# Bringing in Crystal's climate variables -----------------------------------

##Making wordlclim variables pooled by site
locality_climate <- 
  replicate_climate %>% 
  dplyr::select(-Ecozone, -Side, -Moisture_Regime, -Replicate) %>% 
  group_by(Locality) %>% 
  summarise_all(funs(mean))
###Extract vector with site names
localist <- 
  locality_climate[,1]

##Adding crystal's dataset and checking it
cry <- 
  read.csv("C:/Users/pierr/OneDrive/Projects/arctic_flies/Data/Crystal_weather.csv", sep = ";")
str(cry)
###Localities in the same order

##Make a new frame with common variables (mean T, and total precipitation)
climate_corrtest <- cbind(locality_climate$bio1, locality_climate$bio13, 
                          cry$Mean.Annual.Temp, cry$Total.Precipitation)
##Check correlation
##Cutoff point set at .95
cor(climate_corrtest, 
    method = c("pearson", "kendall", "spearman"))
##interestingly, cry variables less correlated than worldclim

##Now blend the two climate datasets together
locaclim <- 
  data.frame(c(locality_climate, cry))
###Tidy it a bit, and remove redundant variables, and wind speed because missing values
locaclim <- 
  locaclim %>% 
  dplyr::select(-Locality, -Average.Wind.Speed, -site, -Mean.Annual.Temp, -Total.Precipitation,
                -Latitude, -Longitude, -Total.Sun.Hours, -Total.Sun.Days)
locaclim <- 
  as.data.frame(locaclim, 
                row.names = as.character(locality_climate$Locality))



# PCA with Crystal's variables --------------------------------------------


##BUT what if adding Crystal's values blurred the worldclim axes
###especially since her min T is the only variable on the second axis
###and all precipitation variables (12-19) are grouped together in the top-right
###so just to be sure
crypca_dat <- locality_climate[4:20]
crypca <- dudi.pca(crypca_dat, scannf = FALSE, nf = 2)
##quick visualisation
par(mfrow = c(3,2))
s.arrow(locapca$c1, clabel = 0.5, boxes =  F)
s.arrow(crypca$c1, clabel = 0.5, boxes =  F)
s.corcircle(crypca$co, clabel = 0.5)
s.class(locapca$li, as.factor(row.names(locaclim)), cpoint = 1)
s.class(crypca$li, as.factor(row.names(locaclim)), cpoint = 1)
par(mfrow = c(1,1))
###slight but not excessive differences


#With Plant Data
plant_dat <- 
  read.csv("C:/Users/pierr/OneDrive/Projects/arctic_flies/Data/plant_dat.csv", sep = ";")
View(plant_dat)
plot(plant_dat[,6:11])
##PCA
testplant_pca <- 
  plant_dat[,6:11]
testplant_pca <- 
  dudi.pca(df = testplant_pca, scannf = FALSE, nf = 3)
testplant_pca
testplant_pca$eig/sum(testplant_pca$eig)
s.corcircle(testplant_pca$co, clabel = 0.5)
s.class(testplant_pca$li, as.factor(plant_dat$site), cpoint = 1)                                                               
##Attaching selected climate variables to see correlation
plant_dat <-
  cbind(plant_dat, replicate_analysis$coldest_quarter, replicate_analysis$t_range)
testplant_pca <- 
  plant_dat[,6:13]
testplant_pca <- 
  dudi.pca(df = testplant_pca, scannf = FALSE, nf = 3)
testplant_pca
testplant_pca$eig/sum(testplant_pca$eig)
s.corcircle(testplant_pca$co, clabel = 0.5)
s.class(testplant_pca$li, as.factor(plant_dat$site), cpoint = 1)

##Checkingvegetation measurements vs. habitat
hist(plant_dat$max.veg.height)
hist(plant_dat$graminoid.cover)
hist(plant_dat$moss.cover)
hist(plant_dat$lichen.cover)
hist(plant_dat$forb.cover)
hist(plant_dat$shrub.cover)
plot(plant_dat$habitat, plant_dat$shrub.cover)

# Diversity ---------------------------------------------------------------

##Evenness
Simpson <- 
  diversity(repliflies[,6:338], index = "simpson", MARGIN = 1, base = exp(1))
Frequency <- 
  specnumber(repliflies[,6:338], MARGIN = 2)
###Margin =1 is number of specie per replicate, Margin = 2 is frequency of species

##Adding Pielou's evenness
Pielou <- 
  replicate_analysis$Shannon/log(replicate_analysis$Richness)
replicate_analysis <- 
  cbind(replicate_analysis, Pielou)
plot(replicate_analysis$Pielou ~ replicate_analysis$Richness)
#Funky NMDS with gradient----------------------------------------------------------------
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











