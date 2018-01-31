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
##Modelling
library(nlme)
library(lme4)
##Model analysis and visualization
##car for Anova() instead of anova()
##anova() does type I test, all variable tested in sequential order
##Anova() does 
library(car)
##visreg for visualization of model, displays ech variable of th model when maintaining the others
##at a median
##not necessarily for paper, but more to see whats going on
library(visreg)



# Data_Tidying ------------------------------------------------------------
##Remove extra columns
cleanflies <- flies %>% 
  select(-Genus, -Province.Territory, -Trapping_Method, -Date) %>% 
  ##Remove Mcgill Station
  filter(Moisture_Regime != "McGill_Subarctic_Research_Station")
##Make sure it works
str(cleanflies)


# Setting_Up_WorldClim_Values ---------------------------------------------
##Getting the data, bio means all 19 variables, and res=2.5 is 21.62 square meter resolution
##yealry averages of 1970-2000
clim_data <- getData("worldclim",var="bio",res=2.5)
###Note that all T values are incresed by factor 10 (eg 231 is 23.1°), because of file size issues

##Extract coordinates from flies
###Trying to make sure I will not mess up everything
###Only works if Longitude before Latitude, otherwise returns only NAs, dunno why tho
longilat <- cleanflies %>% select (Locality, Ecozone, Side, Longitude, Latitude, Moisture_Regime, 
                                   Replicate)

##Extract unique combinations using unique function in raster package
###Just to check
coords<- unique(longilat)
View(coords)


##Add data for replicates without specimens
###Lake Hazen and Cambridge Bay mesic 2
####Making rows for coordinates
Locality <- c('Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 'Lake_Hazen', 
              'Cambridge_Bay')
Ecozone <-  c('HA', 'HA', 'HA', 'HA', 'HA', 'HA', 'HA')    
Side <-  c('E', 'E', 'E', 'E', 'E', 'E', 'W')
Longitude <- c(-71.45594, -71.48788, -71.45110, -71.32244, -71.49242, -71.44115, -105.42582)
Latitude <- c(81.83216, 81.82941, 81.83171, 81.82975, 81.82911, 81.83179, 69.12070)
Moisture_Regime <- c('Mesic', 'Mesic', 'Mesic', 'Wet', 'Wet', 'Wet', 'Mesic')
Replicate <- c('1', '2', '3', '1', '2', '3', '2')
###Binding them
Missing_coord <- data.frame(Locality, Ecozone, Side, Longitude, Latitude, Moisture_Regime, Replicate)
###Binding to rest
coords <- rbind(coords, Missing_coord)
####Double-checking it's working
View(coords)

##Make new longilat vector
longilat <- coords %>% select(Longitude, Latitude)
###72 lines (12 sitesx2 habitats x3 replicates), so we have all coordinates

##use spatial point function to fit coordinates into the worldclim "mind"
coord_points <- SpatialPoints(longilat, proj4string = clim_data@crs)
###remove the tidyr package, because it also has an extract function, whoch conflicts with raster's
.rs.unloadPackage("tidyr")
##Now extract the values of clim_data in the points I just converted
###name of all 19 values present on http://www.worldclim.org/bioclim
worldclim_values <- extract(clim_data, coord_points)
###Now Combine the two data frames, and order it a bit
replicate_climate <- cbind(coords, worldclim_values)
View(replicate_climate)
##Bring tidyr back                                          
library(tidyr)



# Bringing in Crystal's climate variables -----------------------------------

##Making wordlclim variables pooled by site
locality_climate <- replicate_climate %>% 
  select(-Ecozone, -Side, -Moisture_Regime, -Replicate) %>% 
  group_by(Locality) %>% 
  summarise_each(funs(mean))
###Extract vector with site names
localist <- locality_climate[,1]

##Adding crystal's dataset and checking it
cry <- read.csv("C:/Users/Pierre/OneDrive/Projects/arctic_flies/Data/Crystal_weather.csv", sep = ";")
str(cry)
View(cry)
###Localities in the same order

##Make a new frame with common variables (mean T, and total precipitation)
climate_corrtest <- cbind(locality_climate$bio1, locality_climate$bio13, 
                          cry$Mean.Annual.Temp, cry$Total.Precipitation)
##Check correlation
##Cutoff point set at .95
cor(climate_corrtest, method = c("pearson", "kendall", "spearman"))
##interestingly, cry variables less correlated than worldclim

##Now blend the two climate datasets together
locaclim <- data.frame(c(locality_climate, cry))
###Tidy it a bit, and remove redundant variables, and wind speed because missing values
locaclim <- locaclim %>% select(-Locality, -Average.Wind.Speed, -site, -Mean.Annual.Temp, -Total.Precipitation,
                                -Latitude, -Longitude, -Total.Sun.Hours, -Total.Sun.Days)
locaclim <- as.data.frame(locaclim, row.names = as.character(locality_climate$Locality))
View(locaclim)







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
locapca <- dudi.pca(locaclim)
###the higher the bar, the more variation explained (y-axis NOT percentage)
###three axes should be fine, although the third one seems to explain little variation
###now visualize it
locapca

##Assessing axes
###Degree of variation explained can be obtained by dividing each eigenvalue by their sum
locapca$eig/sum(locapca$eig)
###first three axes explain 71.4, 16.8, and 4.5% variation respectively
##so let's drop the third one
locapca <- dudi.pca(df = locaclim, scannf = FALSE, nf = 2)

##Visualising the axes
s.arrow(locapca$c1, clabel = 0.5, boxes =  F)
s.corcircle(locapca$co, clabel = 0.5)
##BIO11 (min T of coldest quarter) and BIO7 (T annual range) stand out
##But where do the sites fit on the axes now?
s.class(locapca$li, as.factor(row.names(locaclim)), cpoint = 1)

##BUT what if adding Crystal's values blurred the worldclim axes
###especially since her min T is the only variable on the second axis
###and all precipitation variables (12-19) are grouped together in the top-right
###so just to be sure
crypca_dat <- locality_climate[4:20]
crypca <- dudi.pca(crypca_dat)
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

                                                               

# Making data frame to visualise axes -------------------------------------
#First goup everything per replicate
repliflies <- cleanflies %>% 
  select(Locality, Moisture_Regime, Replicate, Species, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate, Species) %>%
  summarise_all(funs(sum)) %>%
  ###fill=0 means that 0 will be put instead of default NAs if the cell is to be empty
  spread(key = Species, Abundance, fill = 0)
##checking
View(repliflies)
str(repliflies)

##Calculating diversity Indices
###Evenness
replisimpson <- diversity(repliflies[4:380], index = "simpson", MARGIN = 1, base = exp(1))

###Margin =1 is number of specie per replicate, Margin = 2 is frequency of species
replirichness <- specnumber(repliflies[4:380], MARGIN = 1)
replifrequency <- specnumber(repliflies[4:380], MARGIN = 2)
###Abundance
repliabundance <- cleanflies %>% 
  select(Locality, Moisture_Regime, Replicate, Abundance) %>% 
  group_by(Locality, Moisture_Regime, Replicate) %>%
  summarise_all(funs(sum))
repliabundance <- repliabundance[,4]

##Bind abundance, richness, simpson vectors
repliindices <- cbind.data.frame(repliabundance, replirichness, replisimpson)
View(repliindices)
###Rename columns
colnames(repliindices) <- c('Abundance', 'Richness', 'Simpson')
###Add the 7 rows with no specimens collected
replimissing <- as.data.frame(matrix(0, ncol =3, nrow = 7))
###Coerce the names so that rbind can take place (not working without in this case)
names(replimissing) <- names(repliindices)
###Combination!
repliindices <- rbind(repliindices, replimissing)
View(repliindices)

##Now finishing binding with the rest, and PCA-selected variables
##T/10 because wordlcim T values are *10 for file size purposes
replicate_analysis <- cbind(replicate_climate[,1:7], repliindices, replicate_climate$bio11/10,
                            replicate_climate$bio7/10)
str(replicate_analysis)
##Change column names for later sanity
colnames(replicate_analysis) <- c("Locality", "Ecozone", "Side", "Longitude", "Latitude",
                                  "Moisture_Regime", "Replicate", "Abundance", "Richness", 
                                  "Simpson", "MeanT_Coldest_Quarter", "T_Annual_Range")
View(replicate_analysis)



# Pattern Visualisation at site level ---------------------------------------------------

par(mfrow = c(2,2))
##Richness vs bio11
plot(replicate_analysis$MeanT_Coldest_Quarter ~ replicate_analysis$Richness, 
     xlab = 'Richness', ylab = 'Mean T of Coldest Quarter(°C)',
     xlim = c(0, 100), ylim = c(-35, -10),
     pch = c(0:12)[factor(Locality)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)
legend("bottomright",pch = c(0:12), legend = unique(replicate_climate$Locality), cex = 0.4)
   
##Simpson vs bio11
plot(replicate_analysis$MeanT_Coldest_Quarter ~ replicate_analysis$Simpson, 
     xlab = '1-D', ylab = 'Mean T of Coldest Quarter(°C)',
     xlim = c(0, 1), ylim = c(-35, -10),
     pch = c(0:12)[factor(Locality)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)

##Richness vs bio7
plot(replicate_analysis$T_Annual_Range ~ replicate_analysis$Richness, 
     xlab = 'Richness', ylab = 'T Annual Range (°C)',
     xlim = c(0, 100), ylim = c(40, 55),
     pch = c(0:12)[factor(Locality)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)
legend("topright", pch = 1, col = c("blue", "red"), legend = c("Mesic", "Wet"), cex = 0.5)

##Simpson vs bio7
plot(replicate_analysis$T_Annual_Range ~ replicate_analysis$Simpson, 
     xlab = '1-D', ylab = 'T Annual Range (°C)',
     xlim = c(0, 1), ylim = c(40, 55),
     pch = c(0:12)[factor(Locality)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)

##Bringing back par
par(mfrow = c(1,1))








# Pattern visualisation at ecozone level ----------------------------------
par(mfrow = c(2,2))
##Richness vs bio11
plot(replicate_analysis$MeanT_Coldest_Quarter ~ replicate_analysis$Richness, 
     xlab = 'Richness', ylab = 'Mean T of Coldest Quarter(°C)',
     xlim = c(0, 100), ylim = c(-35, -10),
     pch = c(0:3)[factor(Ecozone)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)
legend("bottomright",pch = c(0:3), legend = c("High Arctic", "Northern Boreal", "Subarctic"), 
       cex = 0.4)

##Simpson vs bio11
plot(replicate_analysis$MeanT_Coldest_Quarter ~ replicate_analysis$Simpson, 
     xlab = '1-D', ylab = 'Mean T of Coldest Quarter(°C)',
     xlim = c(0, 1), ylim = c(-35, -10),
     pch = c(0:3)[factor(Ecozone)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)

##Richness vs bio7
plot(replicate_analysis$T_Annual_Range ~ replicate_analysis$Richness, 
     xlab = 'Richness', ylab = 'T Annual Range (°C)',
     xlim = c(0, 100), ylim = c(40, 55),
     pch = c(0:3)[factor(Ecozone)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)
legend("topright", pch = 1, col = c("blue", "red"), legend = c("Mesic", "Wet"), cex = 0.5)

##Simpson vs bio7
plot(replicate_analysis$T_Annual_Range ~ replicate_analysis$Simpson, 
     xlab = '1-D', ylab = 'T Annual Range (°C)',
     xlim = c(0, 1), ylim = c(40, 55),
     pch = c(0:3)[factor(Ecozone)],
     col = c("blue", "red") [factor(Moisture_Regime)],
     data = replicate_climate)

##Bringing back par
par(mfrow = c(1,1))

# Model for site-level ----------------------------------------------------

##RICHNESS
model_richness_1 <- lme(Richness ~ MeanT_Coldest_Quarter + T_Annual_Range, 
                       random = ~ 1|Locality/Moisture_Regime, data = replicate_analysis)
##look at model
summary(model_richness_1)
#extract variance components
VarCorr(model_richness_1)
##plot model
plot(model_richness_1)

##Anova
Anova(model_richness_1)
##Isnt that too low for a P value?
visreg(model_richness_1)


# TBD --------------------------------------------------------------------


###Although Bray-Curtis good at detecting underlying gradient, Chao is better at handling
###Different sample sizes
replichao <- vegdist(repliflies[4:380], method = "chao",binary=FALSE, diag=FALSE, upper=FALSE,
                   na.rm = FALSE)
