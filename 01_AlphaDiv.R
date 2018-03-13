#Load data and check if it's alright
flies <- read.csv("C:/Users/Pierre/OneDrive/Projects/arctic_flies/Data/Inventory.csv", sep = ";")
View(flies)
str(flies)

#Load packages
##Data tidying
library(plyr)
library(dplyr)
library(tidyr)
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
##Modelling
library(MASS)
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
cleanflies$Moisture_Regime <- droplevels(cleanflies$Moisture_Regime)
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
plot(specaccum(replifliesMO[,3:196]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesCH[,3:90]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesAN[,3:18]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesGB[,3:36]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesKG[,3:30]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesCB[,3:11]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesNW[,3:108]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesSC[,3:45]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesIQ[,3:12]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesYK[,3:91]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesTM[,3:76]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesMOm[,3:123]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesMOw[,3:143]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesCHm[,3:47]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesCHw[,3:66]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesANw[,3:15]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesGBm[,3:23]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesGBw[,3:20]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesKGw[,3:23]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesCBm[,3:4]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesCBw[,3:11]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesNWm[,3:77]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesNWw[,3:63]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesSCw[,3:30]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesIQm[,3:6]), ci.type = "poly", ci.col = "tan1",
     col ="tan4",lwd = 2, ci.lty = 0, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Iqaluit")
replifliesIQw <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit"& Moisture_Regime == "Mesic") %>% 
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
plot(specaccum(replifliesIQm[,3:6]), ci.type = "poly", ci.col = "tan1",
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
plot(specaccum(replifliesYKm[,3:36]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesYKw[,3:70]), ci.type = "poly", ci.col = "lightblue",
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
plot(specaccum(replifliesTMm[,3:47]), ci.type = "poly", ci.col = "lightcoral",
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
plot(specaccum(replifliesTMw[,3:62]), ci.type = "poly", ci.col = "lightcoral",
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




# Sample completeness -----------------------------------------------------
#Per site
repliflies <- 
  cleanflies %>% 
  dplyr::select(Locality, Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate, Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) %>% 
  unite(moisture, Locality, Moisture_Regime, sep ="_", remove = F) %>% 
  unite(plot, moisture, Replicate, sep ="_", remove =F)
completeness <- 
  iNEXT(list(repliflies[,6:382]), 
      q=1, 
      datatype = "abundance")
ggiNEXT(completeness)
# Setting_Up_WorldClim_Values ---------------------------------------------
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
  read.csv("C:/Users/Pierre/OneDrive/Projects/arctic_flies/Data/Crystal_weather.csv", sep = ";")
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
locapca <- dudi.pca(locaclim, scannf = FALSE, nf = 2)
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
  read.csv("C:/Users/Pierre/OneDrive/Projects/arctic_flies/Data/plant_dat.csv", sep = ";")
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
  diversity(repliflies[,6:382], index = "simpson", MARGIN = 1, base = exp(1))
Shannon <- 
  diversity(repliflies[,6:382], index = "shannon", MARGIN = 1, base = exp(1))
###Margin =1 is number of specie per replicate, Margin = 2 is frequency of species
Richness <- 
  specnumber(repliflies[,6:382], MARGIN = 1)
Frequency <- 
  specnumber(repliflies[,6:382], MARGIN = 2)
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
replicate_analysis$moisture <- as.factor(replicate_analysis$moisture)
replicate_analysis$Locality <- as.factor(replicate_analysis$Locality)
replicate_analysis$plot <- as.factor(replicate_analysis$plot)

##Adding Pielou's evenness
Pielou <- 
  replicate_analysis$Shannon/log(replicate_analysis$Richness)
replicate_analysis <- 
  cbind(replicate_analysis, Pielou)
plot(replicate_analysis$Pielou ~ replicate_analysis$Richness)

##Adding chao1
chaorichness2 <- 
  t(estimateR(repliflies[,6:382]))
chao1 <- 
  chaorichness[,2]
chao1 <- 
  c(chao1,0,0,0,0,0,0,0)
replicate_analysis <- 
  cbind(replicate_analysis, chao1)
# Plotting for latitudinal effect -----------------------------------------
#Plotting
##Create PDF
pdf("lat_indices.pdf",height = 10,width = 15)
##set par
par(mfrow = c(2,3))
##Sites
###Richness
plot(replicate_analysis$Richness ~ replicate_analysis$Latitude, 
     xlab = '', ylab = 'Richness',
     ylim = c(0, 90),
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     main ="Richness",
     cex.main =2,
     data = replicate_climate)
legend("topright",pch = c(0:12), 
       legend = unique(replicate_climate$Locality), 
       cex = 1)
###Abundance
plot(replicate_analysis$Abundance ~ replicate_analysis$Latitude, 
     xlab = '', ylab = 'Abundance',
     ylim = c(0,500),
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     main ="Abundance",
     cex.main =2,
     data = replicate_climate)
###Chao 1
plot(replicate_analysis$chao1~ replicate_analysis$Latitude, 
     xlab = '', ylab = "Chao 1 estimator",
     ylim = c(0,200),
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     main ="Chao 1 estimator",
     cex.main =2,
     data = replicate_climate)
legend("topright", pch = 1, col = c("red", "blue"), legend = c("Mesic", "Wet"), cex = 1)
##Ecozone
###Richness
plot(replicate_analysis$Richness ~ replicate_analysis$Latitude, 
     xlab = 'Latitude', ylab = 'Richness',
     ylim = c(0, 90),
     pch = c(15:17)[factor(Ecozone)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     data = replicate_climate)
legend("topright",pch = c(15:17), 
       legend = unique(replicate_climate$Ecozone), 
       cex = 1)
###Abundance
plot(replicate_analysis$Abundance ~ replicate_analysis$Latitude, 
     xlab = 'Latitude', ylab = 'Abundance',
     ylim = c(0,500),
     pch = c(15:17)[factor(Ecozone)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     data = replicate_climate)
###Chao1
plot(replicate_analysis$chao1~ replicate_analysis$Latitude, 
     xlab = 'Latitude', ylab = "Chao 1 estimator",
     ylim = c(0,200),
     pch = c(15:17)[factor(Ecozone)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     data = replicate_climate)
legend("topright", pch = 1, col = c("red", "blue"), legend = c("Mesic", "Wet"), cex = 1)
##Closing
dev.off()
par(mfrow=c(1,1))

# Plotting for environmental effects --------------------------------------
#Plotting three variables at stake vs. latitude
##Create PDF
pdf("lat_envt.pdf",height = 3,width = 10)
##set par
par(mfrow = c(1,2))
##Sites
###Mean T of Coldest Quarter(°C)
plot(replicate_analysis$coldest_quarter ~ replicate_analysis$Latitude, 
     xlab = 'Latitude', ylab = 'Mean T of Coldest Quarter(°C)',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     data = replicate_climate)
legend("topright",pch = c(0:12), 
       legend = unique(replicate_climate$Locality), 
       cex = 0.5)
###Annual T range
plot(replicate_analysis$t_range ~ replicate_analysis$Latitude, 
     xlab = 'Latitude', ylab = 'Annual T Range (°C)',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     data = replicate_climate)
##Closing
dev.off()
par(mfrow =c(1,1))

#Plotting variables vs indices
pdf("envt_indices.pdf",height = 15,width = 15)
##set par
par(mfrow = c(3,3), mar =c(5.1, 5.2, 4.2, 2))
#Richness vs coldest quarter
plot(replicate_analysis$Richness ~ replicate_analysis$coldest_quarter, 
     xlab = '', ylab = 'Richness',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     main = "Mean T of Coldest Quarter(°C)",
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
legend("topleft",pch = c(0:12), 
       legend = unique(replicate_climate$Locality), 
       cex = 1)
#Richness vs T range
plot(replicate_analysis$Richness ~ replicate_analysis$t_range, 
     xlab = '', ylab = '',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     main = "Annual T Range (°C)",
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
#Richness vs forb cover
plot(replicate_analysis$Richness ~ replicate_analysis$chao1, 
     xlab = '', ylab = '',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     main = "Chao 1 index",
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
legend("topright", 
       pch = 1, 
       col = c("red", "blue"), 
       legend = c("Mesic", "Wet"), 
       cex = 1)
#Abundance vs coldest quarter
plot(replicate_analysis$Abundance ~ replicate_analysis$coldest_quarter, 
     xlab = '', ylab = 'Abundance',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
#Abundance vs T range
plot(replicate_analysis$Abundance ~ replicate_analysis$t_range, 
     xlab = '', ylab = '',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
#Abundance vs forb cover
plot(replicate_analysis$Abundance ~ replicate_analysis$chao1, 
     xlab = '', ylab = '',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
#Pielou vs coldest quarter
plot(replicate_analysis$Pielou ~ replicate_analysis$coldest_quarter, 
     xlab = '', ylab = "Pielou's evenness",
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
#Pielou vs T range
plot(replicate_analysis$Pielou ~ replicate_analysis$t_range, 
     xlab = '', ylab = '',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
#Pielou vs forb cover
plot(replicate_analysis$Pielou ~ replicate_analysis$chao1, 
     xlab = '', ylab = '',
     pch = c(0:12)[factor(Locality)],
     col = c("red", "blue") [factor(Moisture_Regime)],
     cex.main =2,
     cex.lab =2,
     data = replicate_climate)
##closing
dev.off()
par(mfrow=c(1,1))

# Testing for environmental effects----------------------------------------------------
##Observed Richness
model_richness <- 
  glmmPQL(Richness~ 
             log(abs(coldest_quarter))*log(t_range)*Moisture_Regime,
          random =~1|Locality/moisture,
          family = "quasipoisson"(link="sqrt"),
       data = replicate_analysis)
plot(model_richness)
qqnorm(residuals(model_richness))
qqline(residuals(model_richness), col = "red")
summary(model_richness)
Anova(model_richness)
visreg(model_richness)

##Chao1
model_chao1 <- 
  glmmPQL(round(chao1)~ 
            log(abs(coldest_quarter))*log(t_range)*Moisture_Regime,
          random =~1|Locality/moisture,
          family = "quasipoisson"(link="sqrt"),
          data = replicate_analysis)
plot(model_chao1)
qqnorm(residuals(model_chao1))
qqline(residuals(model_chao1), col = "red")
summary(model_chao1)
Anova(model_chao1)
visreg(model_chao1)

##Abundance
model_abundance <- 
  glmmPQL(Abundance~ 
            log(abs(coldest_quarter))*log(t_range)*Moisture_Regime, 
          random = ~1|Locality/moisture,
          family = "quasipoisson"(link ="log"),
          data = replicate_analysis)
plot(model_abundance)
qqnorm(residuals(model_abundance))
qqline(residuals(model_abundance), col = "red")
summary(model_abundance)
Anova(model_abundance)
visreg(model_abundance)
