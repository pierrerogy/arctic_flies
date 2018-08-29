#Data tidying and exploration
#Load data and check if it has been imported correctly
flies <- 
  read.csv("~/arctic_flies/Data/Inventory.csv", 
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
library(betapart)
##To plot iNEXT results
library(ggplot2)
library(gridExtra)
##Modelling
library(MASS)
library(MuMIn)
library(ggeffects)
##Model analysis and visualization
library(car)
library(visreg)



# Data_Tidying ------------------------------------------------------------
##Remove extra columns not used in this analysis
cleanflies <- 
  flies %>% 
  dplyr::select(-Genus, -Province.Territory, -Trapping_Method, -Date) %>% 
  ##Remove Mcgill Station
  filter(Moisture_Regime != "McGill_Subarctic_Research_Station")
##Drop levels of McGill Station
cleanflies$Moisture_Regime <- 
  droplevels(cleanflies$Moisture_Regime)
##Make Replicate non-numerical
cleanflies$Replicate <- 
  as.factor(cleanflies$Replicate)
##Verify
str(cleanflies)

# Sample-based rarefaction curve per site------------------------------------------------
pdf("rarecurve_site.pdf", 
    height =30, 
    width = 30)
par(mfrow =c(4,3))

#Moosonee, ON
##Data
replifliesMO <- 
  cleanflies %>% 
  filter(Locality =="Moosonee") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesMO[,3:164]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "Northern Boreal", 
     cex.main =2, 
     xlab = "Moosonee", 
     cex.lab = 2,
     ylab ="")

#Churchill, MB
##Data
replifliesCH <- 
  cleanflies %>% 
  filter(Locality =="Churchill") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesCH[,3:84]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "Subarctic", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Churchill")

#Aulavik National Park, NU
##Data
replifliesAN <- 
  cleanflies %>% 
  filter(Locality =="Aulavik_National_Park") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesAN[,3:14]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "High Arctic", 
     cex.main =2, ylab = "", 
     cex.lab = 2,
     xlab ="Aulavik Nat. Park")

#Goose Bay, NL
##Data
replifliesGB <- 
  cleanflies %>% 
  filter(Locality =="Goose_Bay") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesGB[,3:22]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0,
     xlim = c(1,7),
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Goose Bay")

#Kugluktuk, NU
##Data
replifliesKG <- 
  cleanflies %>% 
  filter(Locality =="Kugluktuk") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesKG[,3:30]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Kugluktuk")

#Cambridge Bay, NU
##Data
replifliesCB <- 
  cleanflies %>% 
  filter(Locality =="Cambridge_Bay") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesCB[,3:14]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Cambridge Bay")

#Norman Wells, NT
##Data
replifliesNW <- 
  cleanflies %>% 
  filter(Locality =="Norman_Wells") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesNW[,3:98]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Norman Wells")

#Schefferville, QC
##Data
replifliesSC <- 
  cleanflies %>% 
  filter(Locality =="Schefferville") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesSC[,3:41]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Schefferville")

#Iqaluit, NU
##Data
replifliesIQ <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit") %>% 
  ###Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesIQ[,3:13]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Iqaluit")

#Yellowknife, NT
##Data
replifliesYK <- 
  cleanflies %>% 
  filter(Locality =="Yellowknife") %>% 
  ##Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesYK[,3:73]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Yellowknife")

#Tombstone Mountains, YT
##Data
replifliesTM <- 
  cleanflies %>% 
  filter(Locality =="Tombstone_Mountains") %>% 
  ##Pool different trapping methods by replicates
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesTM[,3:67]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2,
     ci.lty = 0, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "",
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Tombstone Mountains")

#Lake Hazen, NU
##All samples empty
##Plot
plot(NA, 
     xlim = c(1,7), 
     ylim=c(0,200), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Lake Hazen")


dev.off()
par(mfrow =c(1,1))

# Sample-based rarefaction curve per moisture regime per site------------------------------------------------
pdf("rarecurve_moisture.pdf", 
    height =30, 
    width = 30)
par(mfrow =c(4,6))

#Moosonee, ON
##Mesic
###Data
replifliesMOm <- 
  cleanflies %>% 
  filter(Locality =="Moosonee" & Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesMOm[,3:100]),
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "Northern Boreal Mesic", 
     cex.main =2, 
     xlab = "Moosonee", 
     cex.lab = 2,
     ylab ="")
##Wet
###Data
replifliesMOw <- 
  cleanflies %>% 
  filter(Locality =="Moosonee" & Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesMOw[,3:119]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "Northern Boreal Wet",
     cex.main =2, 
     xlab = "Moosonee", 
     cex.lab = 2,
     ylab ="")

#Churchill, MB
##Mesic
###Data
replifliesCHm <- 
  cleanflies %>% 
  filter(Locality =="Churchill"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesCHm[,3:41]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4),
     ylim=c(0,150), 
     main = "Subarctic Mesic", 
     cex.main =2, ylab = "", 
     cex.lab = 2,
     xlab ="Churchill")
##Wet
###Data
replifliesCHw <- 
  cleanflies %>% 
  filter(Locality =="Churchill"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
##Plot
plot(specaccum(replifliesCHw[,3:63]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "Subarctic Wet", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Churchill")

#Aulavik National Park, NU
##Mesic
###Data
replifliesANm <- 
  cleanflies %>% 
  filter(Locality =="Aulavik_National_Park"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesANm[,3:9]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "High Arctic Mesic", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Aulavik Nat. Park")
##Mesic
###Data
replifliesANw <- 
  cleanflies %>% 
  filter(Locality =="Aulavik_National_Park"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesANw[,3:12]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "High Arctic Wet", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Aulavik Nat. Park")

#Goose Bay, NL
##Mesic
###Data
replifliesGBm <- 
  cleanflies %>% 
  filter(Locality =="Goose_Bay"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesGBm[,3:17]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Goose Bay")
##Wet
###Data
replifliesGBw <- 
  cleanflies %>% 
  filter(Locality =="Goose_Bay"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesGBw[,3:10]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "",
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Goose Bay")

#Kugluktuk, NU
##Mesic
###Data
replifliesKGm <- 
  cleanflies %>% 
  filter(Locality =="Kugluktuk"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesKGm[,3:16]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Kugluktuk")
##Wet
###Data
replifliesKGw <- 
  cleanflies %>% 
  filter(Locality =="Kugluktuk"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesKGw[,3:19]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0,
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Kugluktuk")

#Cambridge Bay, NU
##Mesic
###Data
replifliesCBm <- 
  cleanflies %>% 
  filter(Locality =="Cambridge_Bay" & Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesCBm[,3:5]), 
     ci.type = "poly",
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Cambridge Bay")
##Wet
###Data
replifliesCBw <- 
  cleanflies %>% 
  filter(Locality =="Cambridge_Bay" & Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesCBw[,3:13]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Cambridge Bay")

#Norman Wells, NT
##Mesic
###Data
replifliesNWm <- 
  cleanflies %>% 
  filter(Locality =="Norman_Wells"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesNWm[,3:69]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "",
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Norman Wells")
##Wet
###Data
replifliesNWw <- 
  cleanflies %>% 
  filter(Locality =="Norman_Wells"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesNWw[,3:57]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2,
     ci.lty = 0,
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Norman Wells")

#Schefferville, QC
##Mesic
###Data
replifliesSCm <- 
  cleanflies %>% 
  filter(Locality =="Schefferville"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesSCm[,3:19]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Schefferville")
##Wet
###Data
replifliesSCw <- 
  cleanflies %>% 
  filter(Locality =="Schefferville"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesSCw[,3:26]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Schefferville")

#Iqaluit,NU
##Mesic
###Data
replifliesIQm <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesIQm[,3:7]), 
     ci.type = "poly",
     ci.col = "tan1",
     col ="tan4",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Iqaluit")
##Wet
###Data
replifliesIQw <- 
  cleanflies %>% 
  filter(Locality =="Iqaluit"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesIQw[,3:12]), 
     ci.type = "poly", 
     ci.col = "tan1",
     col ="tan4",
     lwd = 2,
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Iqaluit")

#Yellowknife, NT
##Mesic
###Data
replifliesYKm <- 
  cleanflies %>% 
  filter(Locality =="Yellowknife" & Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesYKm[,3:26]), 
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Yellowknife")
##Wet
###Data
replifliesYKw <- 
  cleanflies %>% 
  filter(Locality =="Yellowknife" & Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesYKw[,3:60]),
     ci.type = "poly", 
     ci.col = "lightblue",
     col ="blue",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Yellowknife")

#Tombstone Mountains, YT
##Mesic
###Data
replifliesTMm <- 
  cleanflies %>% 
  filter(Locality =="Tombstone_Mountains"& Moisture_Regime == "Mesic") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
###Plot
plot(specaccum(replifliesTMm[,3:41]), 
     ci.type = "poly",
     ci.col = "lightcoral",
     col ="red",
     lwd = 2,
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "",
     cex.lab = 2,
     xlab ="Tombstone Mountains")
##Wet
###Data
replifliesTMw <- 
  cleanflies %>% 
  filter(Locality =="Tombstone_Mountains"& Moisture_Regime == "Wet") %>% 
  ####Pool different trapping methods by replicate
  dplyr::select(Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Moisture_Regime, Replicate,Species) %>% 
  summarise_all(funs(sum)) %>% 
  tidyr::spread(key = Species, Abundance, fill =0)
####Plot
plot(specaccum(replifliesTMw[,3:53]), 
     ci.type = "poly", 
     ci.col = "lightcoral",
     col ="red",
     lwd = 2, 
     ci.lty = 0, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Tombstone Mountains")

#Lake Hazen, NU
##Mesic
###All replicates empty
###Plot
plot(NA, 
     xlim = c(1,4), 
     ylim=c(0,150), 
     main = "", 
     cex.main =2, 
     ylab = "", 
     cex.lab = 2,
     xlab ="Lake Hazen")
##Wet
###All replicates empty
###Plot
plot(NA, xlim = c(1,4), ylim=c(0,150), 
     main = "", cex.main =2, ylab = "", cex.lab = 2,
     xlab ="Lake Hazen")

dev.off()
par(mfrow =c(1,1))




# Compute diversity measurements per replicate-------------------------------------
#Create specific names for random effects
repliflies <- 
  cleanflies %>% 
  ##Pool different trapping methods by replicate
  dplyr::select(Locality, Moisture_Regime, Replicate,Species, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate, Species) %>%
  summarise_all(funs(sum)) %>%
  spread(key = Species, Abundance, fill = 0) %>% 
  ##Moisture regime-site-specific names
  unite(moisture, Locality, Moisture_Regime, sep ="_", remove = F) %>% 
  ##Replicate-specific names
  unite(plot, moisture, Replicate, sep ="_", remove =F)
##Double-check
str(repliflies)

#Compute raw richness
Richness <- 
  specnumber(repliflies[,6:338], 
             MARGIN = 1)

#Compute estimated richness (Chao1 index)
##Compute index
Chao1 <- 
  t(estimateR(repliflies[,6:338]))
##Only keep column with index
Chao1 <- 
  Chao1[,2]

#Compute Shannon index
Shannon <- 
  diversity(repliflies[,6:338], 
            index = "shannon", 
            MARGIN = 1, 
            base = exp(1))

#Compute abundance
repliabundance <- 
  cleanflies %>% 
  ##Pool different trapping methods by replicate
  dplyr::select(Locality, Moisture_Regime, Replicate, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate) %>%
  summarise_all(funs(sum))

#Bind all measurement vectors
repliindices <- 
  cbind.data.frame(repliabundance, 
                   Richness,
                   Shannon,
                   Chao1)
str(repliindices)


# Set up WorldClim values ---------------------------------------------
#Extract coordinates from WordClim
##Yearly averages of 1970-2000
##Resolution 2.5, because 0.5 has almost many NAs
clim_data <- 
  raster::getData("worldclim",
                  var="bio",
                  res=2.5)
###Note that all T values are incresed by factor 10 (eg 231 is 23.1°C) 
###(because of file size issues)

#Extract replicate coordinates
##Replicates with specimens
longilat <-
  cleanflies %>% 
  dplyr::select (Locality, Ecoclimatic_zone, Side, Longitude, Latitude, Moisture_Regime, Replicate) %>% 
  unique()
##Manually add for replicates without specimens
### All replicates from Lake Hazen, and  for Cambridge Bay Mesic replicate 2
Locality <- 
  c('Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Cambridge_Bay')
Ecoclimatic_zone <-  
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
###Bind them
Missing_coord <- 
  data.frame(Locality, Ecoclimatic_zone, Side, Longitude, Latitude, Moisture_Regime, Replicate)
##Bind all coordinates
longilat <- 
  rbind(longilat, Missing_coord)

#Make longilat vector
coords <- 
  unique(longilat) %>% 
  dplyr::select(Longitude, Latitude)

#Use spatial point function to fit coordinates
coord_points <- 
  SpatialPoints(coords, 
                proj4string = clim_data@crs)

#Extract values of clim_data in the coord points
##Name of all 19 climatic variables present on http://www.worldclim.org/bioclim
worldclim_values <- 
  raster::extract(clim_data, 
                  coord_points)

#Combine climatic values for all 72 replicates
replicate_climate <- 
  cbind(unique(longilat), worldclim_values)
View(replicate_climate)

# Use PCA to select uncorrelated climatic axes ---------------------------------------
#Create PCA object
locapca <- 
  dudi.pca(replicate_climate[,8:26],
           scannf = FALSE, 
           nf = 3)
##Assess axes
##Degree of variation explained can be obtained by dividing each eigenvalue by overall sum
locapca$eig/sum(locapca$eig)

#Visualise correlations
s.corcircle(locapca$co, clabel = 0.5)
##BIO11 and BIO7 relatively uncorrelated because arrows almost perpendicular
##BIO11 correlated with many other variables



# Bind diversity measurement and selected climatic variables to single data frame ----------------------------------------
#Rescale temperature variables
##T/10 because wordlcim T values are *10 for file size purposes
replicate_analysis <- 
  cbind(replicate_climate[,1:7],
        replicate_climate$bio11/10, 
        replicate_climate$bio7/10)

#Change column names for later sanity
colnames(replicate_analysis)[which(names(replicate_analysis) == "replicate_climate$bio11/10")] <- 
  "coldest_quarter"
colnames(replicate_analysis)[which(names(replicate_analysis) == "replicate_climate$bio7/10")] <- 
  "t_range"

#Add diversity measurements
##Data
replicate_analysis <- 
  replicate_analysis %>% 
  left_join(repliindices)
##Add 0 to empty replicates
replicate_analysis[is.na(replicate_analysis)] <- 
  0
#Create specific names for random effects
replicate_analysis <- 
  replicate_analysis %>% 
  ##Moisture regime-site-specific names
  unite(moisture, Locality, Moisture_Regime, sep ="_", remove = F) %>% 
  ##Replicate-specific names
  unite(plot, moisture, Replicate, sep ="_", remove =F)

#Data format
str(replicate_analysis)
##Convert to factor for analysis functions
replicate_analysis$moisture <- 
  as.factor(replicate_analysis$moisture)
replicate_analysis$Locality <- 
  as.factor(replicate_analysis$Locality)
replicate_analysis$plot <- 
  as.factor(replicate_analysis$plot)


# Map of sites ------------------------------------------------------------
#Get map of Canada
canamap<-  
  raster::getData("GADM", 
                  country = "CAN", 
                  level = 1)

#Average longitude and latitude per site
sitemap <- 
  longilat %>% 
  dplyr::select(Locality, Ecoclimatic_zone, Longitude, Latitude) %>% 
  group_by(Locality, Ecoclimatic_zone) %>% 
  summarise_all(funs(mean))

#Bind column with shortened names
sitemap$LOC <- 
  c("AUL", "CAM", "CHU", "GOO",
    "IQA", "KUG", "MOO", "NOR",
    "SCH", "TOM", "YEL", "HAZ")

#Make new columns with modified coordinates for non-overlapping of labels
##Longitude
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
##Latitude
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

#Reorder columns
sitemap <- 
  sitemap %>% 
  dplyr::select(Ecoclimatic_zone ,Locality, LOC, Longitude, Latitude, LON, LAT)

#Plot and save map in tiff format
tiff("canamap.tiff",
     width = 3200,
     height = 3200, 
     units = "px", 
     res = 800)
par(mar= c(1,1,1,2))
##Plot map
plot(canamap)
##Add site points
points(sitemap$Longitude, 
       sitemap$Latitude, 
       pch = c(15,16,17)[sitemap$Ecoclimatic_zone], 
       col= "grey34", 
       cex = 2)
##Add point labels with modified coordinates
text(sitemap$LON, 
     sitemap$LAT, 
     sitemap$LOC)

dev.off()
