#Load data and check if it's alright
flies <- 
  read.csv("C:/Users/pierr/OneDrive/Projects/arctic_flies/Data/Inventory.csv", 
           sep = ",")
View(flies)
str(flies)

#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
##Maps
library(rworldmap)
##WorldClim
library(raster)
library(sp)
##PCA
library(ade4)
##Diversity indices and analyses
library(vegan)
library(iNEXT)
##To plot iNEXT results
library(ggplot2)
library(gridExtra)
##Modelling
library(MASS)
library(MuMIn)
library(effects)
##Model analysis and visualization
library(car)
library(visreg)



# Data_Tidying ------------------------------------------------------------
##Remove extra columns
cleanflies <- flies %>% 
  dplyr::select(-Genus, -Province.Territory, -Trapping_Method, -Date) %>% 
  ##Remove Mcgill Station
  filter(Moisture_Regime != "McGill_Subarctic_Research_Station")
##drop levels
cleanflies$Moisture_Regime <- 
  droplevels(cleanflies$Moisture_Regime)
cleanflies$Replicate <- 
  as.factor(cleanflies$Replicate)
##Make sure it works
str(cleanflies)


# Sample-rarefaction curve per site------------------------------------------------
pdf("rarecurve_site.pdf", height =30, width = 30)
par(mfrow =c(4,3))
#Moosonee NB
replifliesMO <- 
  cleanflies %>% 
  filter(Locality =="Moosonee") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesMO[,3:164]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "Northern Boreal", cex.main =2, xlab = "Moosonee", cex.lab = 2,
     ylab ="")
#Churchill SA
replifliesCH <- 
  cleanflies %>% 
  filter(Locality =="Churchill") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesCH[,3:84]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "Subarctic", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Churchill")
#Aulvik HA
replifliesAN <- 
  cleanflies %>% 
  filter(Locality =="Aulavik_National_Park") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesAN[,3:14]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "High Arctic", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Aulavik Nat Park")
#Goose Bay NB
replifliesGB <- 
  cleanflies %>% 
  filter(Locality =="Goose_Bay") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesGB[,3:22]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Goose Bay")
#Kugluktuk SA
replifliesKG <- 
  cleanflies %>% 
  filter(Locality =="Kugluktuk") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesKG[,3:29]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Kugluktuk")
#Cambridge Bay HA
replifliesCB <- 
  cleanflies %>% 
  filter(Locality =="Cambridge_Bay") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesCB[,3:14]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Cambridge Bay")
#Norman Wells NB
replifliesNW <- 
  cleanflies %>% 
  filter(Locality =="Norman_Wells") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesNW[,3:98]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Norman Wells")
#Schefferville SA
replifliesSC <- 
  cleanflies %>% 
  filter(Locality =="Schefferville") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesSC[,3:41]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Schefferville")
#Iqaluit HA
replifliesIQ <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesIQ[,3:13]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Iqaluit")
#Yellowknife NB
replifliesYK <- 
  cleanflies %>% 
  filter(Locality =="Yellowknife") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesYK[,3:73]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Yellowknife")
#Tombstone SA
replifliesTM <- 
  cleanflies %>% 
  filter(Locality =="Tombstone_Mountains") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesTM[,3:67]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Tombstone Mountains")
#Lake Hazen HA
plot(NA, xlim = c(1,7), ylim=c(0,200), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Lake Hazen")
dev.off()
par(mfrow =c(1,1))

# Sample-rarefaction curve per moisture regime------------------------------------------------
pdf("rarecurve_moisture.pdf", height =30, width = 30)
par(mfrow =c(4,6))
#Moosonee NB
replifliesMOm <- 
  cleanflies %>% 
  filter(Locality =="Moosonee" & Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesMOm[,3:100]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "Northern Boreal Mesic", cex.main =2, xlab = "Moosonee", cex.lab = 2,
     ylab ="")
replifliesMOw <- 
  cleanflies %>% 
  filter(Locality =="Moosonee" & Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesMOw[,3:119]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "Northern Boreal Wet", cex.main =2, xlab = "Moosonee", cex.lab = 2,
     ylab ="")
#Churchill SA
replifliesCHm <- 
  cleanflies %>% 
  filter(Locality =="Churchill"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesCHm[,3:41]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "Subarctic Mesic", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Churchill")
replifliesCHw <- 
  cleanflies %>% 
  filter(Locality =="Churchill"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesCHw[,3:63]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "Subarctic Wet", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Churchill")
#Aulvik HA
replifliesANm <- 
  cleanflies %>% 
  filter(Locality =="Aulavik_National_Park"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesANm[,3:9]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "High Arctic Mesic", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Aulavik Nat Park")
replifliesANw <- 
  cleanflies %>% 
  filter(Locality =="Aulavik_National_Park"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesANw[,3:12]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "High Arctic Wet", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Aulavik Nat Park")
#Goose Bay NB
replifliesGBm <- 
  cleanflies %>% 
  filter(Locality =="Goose_Bay"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesGBm[,3:17]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Goose Bay")
replifliesGBw <- 
  cleanflies %>% 
  filter(Locality =="Goose_Bay"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesGBw[,3:10]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Goose Bay")
#Kugluktuk SA
replifliesKGm <- 
  cleanflies %>% 
  filter(Locality =="Kugluktuk"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesKGm[,3:15]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Kugluktuk")
replifliesKGw <- 
  cleanflies %>% 
  filter(Locality =="Kugluktuk"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesKGw[,3:19]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Kugluktuk")
#Cambridge Bay HA
replifliesCBm <- 
  cleanflies %>% 
  filter(Locality =="Cambridge_Bay" & Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesCBm[,3:5]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Cambridge Bay")
replifliesCBw <- 
  cleanflies %>% 
  filter(Locality =="Cambridge_Bay" & Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesCBw[,3:13]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Cambridge Bay")
#Norman Wells NB
replifliesNWm <- 
  cleanflies %>% 
  filter(Locality =="Norman_Wells"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesNWm[,3:69]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Norman Wells")
replifliesNWw <- 
  cleanflies %>% 
  filter(Locality =="Norman_Wells"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesNWw[,3:57]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Norman Wells")
#Schefferville SA
replifliesSCm <- 
  cleanflies %>% 
  filter(Locality =="Schefferville"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesSCm[,3:19]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Schefferville")
replifliesSCw <- 
  cleanflies %>% 
  filter(Locality =="Schefferville"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesSCw[,3:26]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Schefferville")
#Iqaluit HA
replifliesIQm <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesIQm[,3:7]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Iqaluit")
replifliesIQw <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesIQw[,3:12]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Iqaluit")
#Yellowknife NB
replifliesYKm <- 
  cleanflies %>% 
  filter(Locality =="Yellowknife" & Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesYKm[,3:26]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Yellowknife")
replifliesYKw <- 
  cleanflies %>% 
  filter(Locality =="Yellowknife" & Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesYKw[,3:60]), ci.type = "poly", ci.col = "lightblue",
     col ="blue",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Yellowknife")
#Tombstone SA
replifliesTMm <- 
  cleanflies %>% 
  filter(Locality =="Tombstone_Mountains"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesTMm[,3:40]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Tombstone Mountains")
replifliesTMw <- 
  cleanflies %>% 
  filter(Locality =="Tombstone_Mountains"& Moisture_Regime == "Wet") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesTMw[,3:53]), ci.type = "poly", ci.col = "lightcoral",
     col ="red",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Tombstone Mountains")
#Lake Hazen HA
plot(NA, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Lake Hazen")
plot(NA, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Lake Hazen")
dev.off()
par(mfrow =c(1,1))




# Rarefaction and sample completeness per site-----------------------------------------------------
pdf("completeness_site.pdf", height = 8, width = 15)
#Per site
repliflies <- 
  cleanflies %>% 
  dplyr::select(Locality,Species, Abundance) %>% 
  group_by(Locality,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
repliflies <- 
  data.frame(repliflies[,2:334], row.names = repliflies$Locality)
completeness <- 
  iNEXT(t(repliflies), 
      q=0, 
      datatype = "abundance")
ggiNEXT(completeness)
dev.off()
##values
DataInfo(t(repliflies), 
         datatype = "abundance")
# Rarefaction per moisture regime-----------------------------------------------------
#Moosonee
replifliesMO <- 
  cleanflies %>% 
  filter(Locality== "Moosonee") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesMO <- 
  data.frame(replifliesMO[,2:163], row.names = replifliesMO$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesMO), 
        q=0, 
        datatype = "abundance")
plot1 <- 
  ggiNEXT(completeness) + ggtitle("Moosonee")+ xlim(0, 1400) + ylim(0,175)

#Churchill
replifliesCH <- 
  cleanflies %>% 
  filter(Locality== "Churchill") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesCH <- 
  data.frame(replifliesCH[,2:83], row.names = replifliesCH$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesCH), 
        q=0, 
        datatype = "abundance")
plot2 <- 
  ggiNEXT(completeness) + ggtitle("Churchill")+ xlim(0, 1400) + ylim(0,175)

#Aulavik
replifliesAU <- 
  cleanflies %>% 
  filter(Locality== "Aulavik_National_Park") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesAU <- 
  data.frame(replifliesAU[,2:13], row.names = replifliesAU$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesAU), 
        q=0, 
        datatype = "abundance")

plot3 <- 
  ggiNEXT(completeness) + ggtitle("Aulavik National Park")+ xlim(0, 1400) + ylim(0,175)

#Goose Bay
replifliesGB <- 
  cleanflies %>% 
  filter(Locality== "Goose_Bay") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesGB <- 
  data.frame(replifliesGB[,2:21], row.names = replifliesGB$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesGB), 
        q=0, 
        datatype = "abundance")
plot4 <- 
  ggiNEXT(completeness) + ggtitle("Goose Bay")+ xlim(0, 1400) + ylim(0,175)

#Kugluktuk
replifliesKG <- 
  cleanflies %>% 
  filter(Locality== "Kugluktuk") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesKG <- 
  data.frame(replifliesKG[,2:28], row.names = replifliesKG$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesKG), 
        q=0, 
        datatype = "abundance")
plot5 <- 
  ggiNEXT(completeness) + ggtitle("Kugluktuk")+ xlim(0, 1400) + ylim(0,175)

#Cambridge Bay
replifliesCB <- 
  cleanflies %>% 
  filter(Locality== "Cambridge_Bay") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesCB <- 
  data.frame(replifliesCB[,2:13], row.names = replifliesCB$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesCB), 
        q=0, 
        datatype = "abundance")
plot6 <- 
  ggiNEXT(completeness) + ggtitle("Cambridge Bay")+ xlim(0, 1400) + ylim(0,175)

#Norman Wells
replifliesNW <- 
  cleanflies %>% 
  filter(Locality== "Norman_Wells") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesNW <- 
  data.frame(replifliesNW[,2:97], row.names = replifliesNW$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesNW), 
        q=0, 
        datatype = "abundance")
plot7 <- 
  ggiNEXT(completeness) + ggtitle("Norman Wells")+ xlim(0, 1400) + ylim(0,175)

#Schefferville
replifliesSC <- 
  cleanflies %>% 
  filter(Locality== "Schefferville") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesSC <- 
  data.frame(replifliesSC[,2:40], row.names = replifliesSC$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesSC), 
        q=0, 
        datatype = "abundance")
plot8 <- 
  ggiNEXT(completeness) + ggtitle("Schefferville")+ xlim(0, 1400) + ylim(0,175)

#Iqaluit
replifliesIQ<- 
  cleanflies %>% 
  filter(Locality== "Iqaluit") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesIQ <- 
  data.frame(replifliesIQ[,2:12], row.names = replifliesIQ$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesIQ), 
        q=0, 
        datatype = "abundance")
plot9 <- 
  ggiNEXT(completeness) + ggtitle("Iqaluit")+ xlim(0, 1400) + ylim(0,175)

#Yellowknife
replifliesYK<- 
  cleanflies %>% 
  filter(Locality== "Yellowknife") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesYK <- 
  data.frame(replifliesYK[,2:72], row.names = replifliesYK$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesYK), 
        q=0, 
        datatype = "abundance")
plot10 <- 
  ggiNEXT(completeness) + ggtitle("Yellowknife")+ xlim(0, 1400) + ylim(0,175)

#Tombstone Mountains
replifliesTM <- 
  cleanflies %>% 
  filter(Locality== "Tombstone_Mountains") %>% 
  dplyr::select(Moisture_Regime, Species, Abundance) %>% 
  group_by(Moisture_Regime,Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) 
replifliesTM <- 
  data.frame(replifliesTM[,2:66], row.names = replifliesTM$Moisture_Regime)
completeness <- 
  iNEXT(t(replifliesTM), 
        q=0, 
        datatype = "abundance")
plot11 <- 
  ggiNEXT(completeness) + ggtitle("Tombstone Mountains")+ xlim(0, 1400) + ylim(0,175)

#Closing
pdf("completeness_moisture.pdf", height = 30, width =30)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, 
             plot7, plot8, plot9, plot10, plot11, ncol =3)
dev.off()


# Map of sites ------------------------------------------------------------
#Get map of Canada
canamap<-  
 raster::getData("GADM", 
                  country = "CAN", 
                  level = 1)
#Average longitude and latitude per site
sitemap <- 
  longilat %>% 
  dplyr::select(Locality, Ecozone, Longitude, Latitude) %>% 
  group_by(Locality, Ecozone) %>% 
  summarise_all(funs(mean))
##bind column with shortened names and add points coordinates
sitemap$LOC <- 
  c("AUL", "CAM", "CHU", "GOO",
    "IQA", "KUG", "MOO", "NOR",
    "SCH", "TOM", "YEL", "HAZ")
sitemap$LON <- 
  sitemap$Longitude
sitemap[12,6] <- 
  -63
sitemap[5,6] <- 
  -58
sitemap[9,6] <- 
  -70
sitemap[4,6] <- 
  -68
sitemap[2,6] <- 
  -101.5
sitemap[6,6] <- 
  -123
sitemap[1,6] <- 
  -128
sitemap[8,6] <- 
  -123.5
sitemap[11,6] <- 
  -107
sitemap[7,6] <- 
  -89
sitemap[3,6] <- 
  -96
sitemap[10,6] <- 
  -135
sitemap$LAT <- 
  sitemap$Latitude
sitemap[12,7] <- 
  80
sitemap[9,7] <- 
  56.8
sitemap[8,7] <- 
  63
sitemap[10,7] <- 
  62
sitemap[2,7] <- 
  66.3
sitemap[3,7] <- 
  56
sitemap[1,7] <- 
  75.5
sitemap[4,7] <- 
  53
##Reorder a bit
sitemap <- 
  sitemap %>% 
  dplyr::select(Locality, LOC, Longitude, Latitude, LON, LAT)
##Plot and save map
tiff("canamap.tiff",
     width = 3200,
     height = 3200, 
     units = "px", 
     res = 800)
par(mar= c(1,1,1,2))
plot(canamap)
points(sitemap$Longitude, 
       sitemap$Latitude, 
       col = c("black", "grey70", "grey40")[sitemap$Ecozone], 
       pch= 19, 
       cex = 2)
text(sitemap$LON, 
     sitemap$LAT, 
     sitemap$LOC)
dev.off()
# Setting up WorldClim values ---------------------------------------------
##Extract coordinates from flies
###Only works if Longitude before Latitude, otherwise returns only NAs
##yearly averages of 1970-2000
##resolution 2.5, because 0.5 has almost only NAs
clim_data <- 
  raster::getData("worldclim",var="bio",res=2.5)
###Note that all T values are incresed by factor 10 (eg 231 is 23.1°), because of file size issues

##Extract coordinates from flies
###Only works if Longitude before Latitude, otherwise returns only NAs, dunno why tho
longilat <-
  cleanflies %>% 
  dplyr::select (Locality, Ecozone, Side, Longitude, Latitude, Moisture_Regime, Replicate) %>% 
  unique()
##Add data for replicates without specimens
###Lake Hazen and Cambridge Bay mesic 2
###Making rows for coordinates
Locality <- 
  c('Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Cambridge_Bay')
Ecozone <-  
  c('HA', 'HA', 'HA', 'HA', 'HA', 'HA', 'HA')    
Side <- 
  c('E', 'E', 'E', 'E', 'E', 'E', 'W')
Longitude <-
  c(-71.45594, -71.48788, -71.45110, -71.32244, -71.49242, -71.44115, -105.42582)
Latitude <-
  c(81.83216, 81.82941, 81.83171, 81.82975, 81.82911, 81.83179, 69.12070)
Moisture_Regime <-
  c('Mesic', 'Mesic', 'Mesic', 'Wet', 'Wet', 'Wet', 'Mesic')
Replicate <-
  c('1', '2', '3', '1', '2', '3', '2')
###Binding them
Missing_coord <- 
  data.frame(Locality, Ecozone, Side, Longitude, Latitude, Moisture_Regime, Replicate)
###Binding to rest
longilat <- 
  rbind(longilat, Missing_coord)
##Make longilat vector
coords <- 
  unique(longilat) %>% 
  dplyr::select(Longitude, Latitude)

##use spatial point function to fit coordinates into the worldclim 
coord_points <- 
  SpatialPoints(coords, proj4string = clim_data@crs)
###remove the tidyr package, because it also has an extract function, whoch conflicts with raster's
.rs.unloadPackage("tidyr")
##Extract the values of clim_data in the coord points
###name of all 19 values present on http://www.worldclim.org/bioclim
worldclim_values <- 
  extract(clim_data, coord_points)
###Now Combine the two data frames, and order it a bit
replicate_climate <- 
  cbind(unique(longilat), worldclim_values)
View(replicate_climate)
##Bring tidyr back                                          
library(tidyr)


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

# Using PCA to select relevant axes ---------------------------------------
##Make sure locaclim is a dataframe
class(locaclim)

##Plot the data, to get a rough idea of the degree of correlation between variables
###a lot of them, so gotta make it a PDF
pdf("climData.pdf",height = 30,width = 30)
plot(locaclim)
dev.off()
###definitely most are highly correlated, bio 7 seems to stand out

##Create PCA object
locapca <- dudi.pca(locality_climate[,4:22], scannf = FALSE, nf = 3)
##Assessing axes
###Degree of variation explained can be obtained by dividing each eigenvalue by their sum
locapca$eig/sum(locapca$eig)
###first three axes explain 71.4, 16.8, and 4.5% variation respectively


##Visualising the axes
##BIO11 (min T of coldest quarter) and BIO7 (T annual range) stand out
##But where do the sites fit on the axes now?
s.class(locapca$li, as.factor(row.names(locaclim)), cpoint = 1)

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
s.corcircle(locapca$co, clabel = 0.5)
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
# Making data frame to visualise axes -------------------------------------
#First goup everything per replicate
repliflies <- 
  cleanflies %>% 
  dplyr::select(Locality, Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate, Species) %>%
  summarise_all(funs(sum)) %>%
  ###fill=0 means that 0 will be put instead of default NAs if the cell is to be empty
  spread(key = Species, Abundance, fill = 0) %>% 
  unite(moisture, Locality, Moisture_Regime, sep ="_", remove = F) %>% 
  unite(plot, moisture, Replicate, sep ="_", remove =F)
##checking
str(repliflies)

##Calculating diversity Indices
###Evenness
Simpson <- 
  diversity(repliflies[,6:338], index = "simpson", MARGIN = 1, base = exp(1))
Shannon <- 
  diversity(repliflies[,6:338], index = "shannon", MARGIN = 1, base = exp(1))
###Margin =1 is number of specie per replicate, Margin = 2 is frequency of species
Richness <- 
  specnumber(repliflies[,6:338], MARGIN = 1)
Frequency <- 
  specnumber(repliflies[,6:338], MARGIN = 2)
###Abundance
repliabundance <- 
  cleanflies %>% 
  dplyr::select(Locality, Moisture_Regime, Replicate, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate) %>%
  summarise_all(funs(sum))

##Bind abundance, richness, simpson vectors
repliindices <- 
  cbind.data.frame(repliabundance, Richness, Simpson, Shannon)
str(repliindices)
replicate_analysis <- 
  replicate_climate[,1:7] %>% 
  left_join(repliindices)
replicate_analysis[is.na(replicate_analysis)] <- 
  0

##Now finishing binding with PCA-selected variables
##T/10 because wordlcim T values are *10 for file size purposes
replicate_analysis <- 
  cbind(replicate_analysis,replicate_climate$bio11/10, 
                            replicate_climate$bio7/10)
str(replicate_analysis)
##Change column names for later sanity
colnames(replicate_analysis)[which(names(replicate_analysis) == "replicate_climate$bio11/10")] <- 
  "coldest_quarter"
colnames(replicate_analysis)[which(names(replicate_analysis) == "replicate_climate$bio7/10")] <- 
  "t_range"
replicate_analysis <- 
  replicate_analysis %>% 
  unite(moisture, Locality, Moisture_Regime, sep ="_", remove = F) %>% 
  unite(plot, moisture, Replicate, sep ="_", remove =F)
str(replicate_analysis)
replicate_analysis$moisture <- 
  as.factor(replicate_analysis$moisture)
replicate_analysis$Locality <- 
  as.factor(replicate_analysis$Locality)
replicate_analysis$plot <- 
  as.factor(replicate_analysis$plot)

##Adding Pielou's evenness
Pielou <- 
  replicate_analysis$Shannon/log(replicate_analysis$Richness)
replicate_analysis <- 
  cbind(replicate_analysis, Pielou)
plot(replicate_analysis$Pielou ~ replicate_analysis$Richness)

##Adding chao1
chao1 <- 
  t(estimateR(repliflies[,6:338]))
chao1 <- 
  chao1[,2]
chao1 <- 
  c(chao1,0,0,0,0,0,0,0)
replicate_analysis <- 
  cbind(replicate_analysis, chao1)
# Testing for environmental effects----------------------------------------------------
##Observed Richness
model_richness <- 
  glmmPQL(Richness~ 
            coldest_quarter*t_range + Moisture_Regime,
          random =~1|Locality/moisture, 
          family = "quasipoisson"(link="sqrt"),
       data = replicate_analysis)
plot(model_richness)
qqnorm(residuals(model_richness))
qqline(residuals(model_richness), col = "red")
summary(model_richness)
summary(model_richness)$tTable
Anova(model_richness)
visreg(model_richness)
r.squaredGLMM(model_richness)
##plot
plot(effect("coldest_quarter:t_range", 
            model_richness),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = c("coral", "coral1", "coral2",
                          "coral3", "coral4")),
     lattice = list(key.args =list(
       space = "top",
       columns = 5,
       x = 0.01, 
       y = 1,
       title= "Annual temperature range (°C)",
       border = "transparent",
       between.columns = 0)),
     ylab = "Observed species richness",
     xlab = "Minimum temperature of the coldest quarter (°C)",
     ylim = c(0,15),
     type = "response",
     main = ""
)
plot(effect("coldest_quarter", 
            model_richness),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Observed richness per plot",
     xlab = "Minimum temperature of the coldest quarter (°C)",
     ylim = c(0,10),
     type = "response",
     main = ""
)
plot(effect("t_range", 
            model_richness),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Observed richness per plot",
     xlab = "Temperature annual range (°C)",
     ylim = c(0,10),
     type = "response",
     main = ""
)
plot(effect("Moisture_Regime", 
            model_richness),
     ci.style = "bar",
     lines = list(multiline = T, 
                  lty =0, 
                  col = c("grey20", "grey35", "grey50",
                          "grey65", "grey80")),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Observed richness per plot",
     xlab = "Moisture Regime",
     ylim = c(0,10),
     type = "response",
     main = ""
)

##Chao1
model_chao1 <- 
  glmmPQL(chao1~ 
            coldest_quarter*t_range + Moisture_Regime,
          random =~1|Locality/moisture,
          family = "quasipoisson"(link="sqrt"),
          data = replicate_analysis)
plot(model_chao1)
qqnorm(residuals(model_chao1))
qqline(residuals(model_chao1), col = "red")
summary(model_chao1)
summary(model_chao1)$tTable
Anova(model_chao1)
visreg(model_chao1)
r.squaredGLMM(model_chao1)
##plot
plot(effect("coldest_quarter:t_range", 
            model_chao1),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = c("coral", "coral1", "coral2",
                          "coral3", "coral4")),
     lattice = list(key.args =list(
       space = "top",
       columns = 5,
       x = 0.01, 
       y = 1,
       title= "Annual T range (°C)",
       border = "transparent",
       between.columns = 0)),
     ylab = "Estimated richness per plot",
     xlab = "Minimum temperature of the coldest quarter (°C)",
     ylim = c(0,20),
     type = "response",
     main = ""
)
plot(effect("coldest_quarter", 
            model_chao1),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Estimated richness per plot",
     xlab = "Minimum temperature of the coldest quarter (°C)",
     ylim = c(0,10),
     type = "response",
     main = ""
)
plot(effect("t_range", 
            model_chao1),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Estimated richness per plot",
     xlab = "Temperature annual range (°C)",
     ylim = c(0,10),
     type = "response",
     main = ""
)
plot(effect("Moisture_Regime", 
            model_chao1),
     ci.style = "bar",
     lines = list(multiline = T, 
                  lty =0, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Estimated richness per plot",
     xlab = "Moisture Regime",
     ylim = c(0,10),
     type = "response",
     main = ""
)

##Abundance
model_abundance <- 
  glmmPQL(Abundance~ 
            coldest_quarter*t_range + Moisture_Regime, 
          random = ~1|Locality/moisture,
          family = "quasipoisson"(link ="sqrt"),
          data = replicate_analysis)
plot(model_abundance)
qqnorm(residuals(model_abundance))
qqline(residuals(model_abundance), col = "red")
summary(model_abundance)
summary(model_abundance)$tTable
Anova(model_abundance)
visreg(model_abundance)
r.squaredGLMM(model_abundance)
##plot
plot(effect("coldest_quarter:t_range", 
            model_chao1),
     ci.style = "band",
     lines = list(multiline = F, 
                  lty =1, 
                  col = c("black", "grey25", "grey40",
                          "grey55", "grey70")),
     lattice = list(key.args =list(
       x = 0.1, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Abundance per plot",
     xlab = "Minimum temperature of the coldest quarter (°C)",
     ylim = c(0,20),
     type = "response",
     main = ""
)
plot(effect("coldest_quarter", 
            model_abundance),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Abundance per plot",
     xlab = "Minimum temperature of the coldest quarter (°C)",
     ylim = c(0,20),
     type = "response",
     main = ""
)
plot(effect("t_range", 
            model_abundance),
     ci.style = "band",
     lines = list(multiline = T, 
                  lty =1, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Abundance per plot",
     xlab = "Temperature annual range (°C)",
     ylim = c(0,20),
     type = "response",
     main = ""
)
plot(effect("Moisture_Regime", 
            model_abundance),
     ci.style = "bar",
     lines = list(multiline = T, 
                  lty =0, 
                  col = "grey30"),
     lattice = list(key.args =list(
       x = 0.79, 
       y = 1,
       cex = 0.75,
       between.columns = 0)),
     ylab = "Abundance per plot",
     xlab = "Moisture Regime",
     ylim = c(0,10),
     type = "response",
     main = ""
)
