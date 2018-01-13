#Load data and check if it's alright
flies <- read.csv("C:/Users/Pierre/OneDrive/Projects/arctic_flies/Data/Inventory.csv", sep = ";")
View(flies)
str(flies)

#Load packages
library(raster)
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(sp)


# Data_Tidying ------------------------------------------------------------
##Remove extra columns
flies <- flies %>% 
  select(-Genus, -Province.Territory, -Trapping_Method, -Date) %>% 
##Remove Mcgill Station
  filter(Moisture_Regime != "McGill_Subarctic_Research_Station")
##Make sure it workes
str(flies)


# Setting_Up_WorldClim_Values ---------------------------------------------
##Getting the data, bio means all 19 variables, and res=2.5 is 21.62 square meter resolution
clim_data <- getData("worldclim",var="bio",res=2.5)
###Note that all T values are incresed by factor 10 (eg 231 is 23.1°), because of file size issues

##Extract coordinates from flies
###Trying to make sure I will not mess up everything
###Only works if Longitude before Latitude, otherwise returns only NAs, dunno why tho
longilat <- flies %>% select (Longitude, Latitude)

##Extract unique combinations using unique function in raster package
###Just to check
coords<- unique(longilat)
View(coords)

##use spatial point function to fit coordinates into the worldclim "mind"
coord_points <- SpatialPoints(longilat, proj4string = clim_data@crs)
##remove the tidyr package, because it also has an extract function, whoch conflicts with raster's
.rs.unloadPackage("tidyr")
##Now extract the values of clim_data in the points I just converted
###name of all 19 values present on http://www.worldclim.org/bioclim
worldclim_values <- extract(clim_data, coord_points)
##Now Combine the two data frames
anaflies <- cbind(flies, worldclim_values)

###Bring tydir back
library(tidyr)








# Worldclim_per_Ecozone ---------------------------------------------------
##Isolating vaiables of interest
ecoclim <- anaflies %>% select(-Locality,-Side, -Family, -Species, -Abundance, -Moisture_Regime, -Replicate)
##Extracting unique combinations of site, coordinates, and corresponding variables
##Checking if no mess up
variaecoclim <- unique(ecoclim)
str(variaecoclim)
##Averaging everything using dplyr
averaged_ecoclim <- variaecoclim %>%
  group_by(Ecozone) %>%
  summarise_each(funs(mean))
View(averaged_ecoclim)

# Wordlclim_per_Ecozone_Moisture -------------------------------------------
##Isolating vaiables of interest
ecomoistclim <- anaflies %>% select(-Locality,-Side, -Family, -Species, -Abundance, -Replicate)
##Extracting unique combinations of site, coordinates, and corresponding variables
##Checking if no mess up
variaecomoistclim <- unique(ecomoistclim)
str(variaecomoistclim)
##Averaging everything using dplyr
averaged_ecomoistclim <- variaecomoistclim %>%
  group_by(Ecozone, Moisture_Regime) %>%
  summarise_each(funs(mean))
View(averaged_ecomoistclim)

# Worldclim_per_Locality --------------------------------------------------
##Isolating vaiables of interest
###NOTE that maybe worldclim factors are different in wet vs mesic areas? What about sides?
locaclim <- anaflies %>% select(-Family, -Species, -Abundance, -Moisture_Regime, -Replicate)
##Extracting unique combinations of site, coordinates, and corresponding variables
##Checking if no mess up
varialocaclim <- unique(locaclim)
str(varialocaclim)
##Averaging everything using dplyr (Thanks Andrew!!!)
averaged_localim <- varialocaclim %>%
  group_by(Locality, Ecozone, Side) %>%
  summarise_each(funs(mean))
View(averaged_localim)





# Wordlicm_per_Locality_Moisture ------------------------------------------
##Isolating vaiables of interest
###NOTE that maybe worldclim factors are different in wet vs mesic areas? What about sides?
moistclim <- anaflies %>% select(-Family, -Species, -Abundance, -Replicate)
##Extracting unique combinations of site, coordinates, and corresponding variables
##Checking if no mess up
variamoistclim <- unique(moistclim)
str(variamoistclim)
##Averaging everything using dplyr (Thanks Andrew!!!)
averaged_moistclim <- variamoistclim %>%
  group_by(Locality, Moisture_Regime, Ecozone, Side) %>%
  summarise_each(funs(mean))
View(averaged_moistclim)


# Diversity_Indices_per_Ecozone ---------------------------------------------------
##First goup everything per site
ecoflies <- flies %>% 
  select(Ecozone, Species, Abundance) %>% 
  group_by(Ecozone, Species) %>%
  summarise_each(funs(sum)) %>%
  ###fill=0 means that 0 will be put instead of default NAs if the cell is to be empty
  spread(key = Species, Abundance, fill = 0)
##checking
View(ecoflies)
str(ecoflies)
##Making Locality row names
ecolist <- ecoflies$Ecozone
ecoflies <- ecoflies[,2:378]
##Assigning row names only work with characters
ecoflies <- as.data.frame(ecoflies, row.names = as.character(ecolist))

##Calculating diversity Indices
###Richness
ecoshannon <- diversity(ecoflies, index = "shannon", MARGIN = 1, base = exp(1))
###Evenness
ecosimpson <- diversity(ecoflies, index = "simpson", MARGIN = 1, base = exp(1))
###Although Bray-Curtis good at detecting underlying gradient, Chao is better at handling
###Different sample sizes
ecochao <- vegdist(ecoflies, method = "chao",binary=FALSE, diag=FALSE, upper=FALSE,
                    na.rm = FALSE)
###Margin =1 is number of specie per eco, Margin = 2 is frequency of species
ecorichness <- specnumber(ecoflies, MARGIN = 1)
ecofrequency <- specnumber(ecoflies, MARGIN = 2)
###Abundance
ecoabundance <- flies %>% 
  select(Ecozone, Abundance) %>% 
  group_by(Ecozone) %>%
  summarise_each(funs(sum))
####Remove first column with locality list, always alphabetical so all G
ecoabundance <- ecoabundance[,2]

##Bind everything into nice data frame for analysis
ecoanalysis <- cbind.data.frame(ecorichness, ecoabundance,ecoshannon,ecosimpson, 
                                averaged_ecoclim[,2:22])
View(ecoanalysis)



# Diversity_Indices_per_Ecozone_Moisture ----------------------------------
ecomoistflies <- flies %>% 
  select(Ecozone, Moisture_Regime, Species, Abundance) %>% 
  unite(Ecomoist, Ecozone:Moisture_Regime, sep= "_") %>% 
  group_by(Ecomoist, Species) %>%
  summarise_each(funs(sum)) %>%
  ###fill=0 means that 0 will be put instead of default NAs if the cell is to be empty
  spread(key = Species, Abundance, fill = 0)
##checking
View(ecomoistflies)
str(ecomoistflies)
##Making Locality row names
ecomoistlist <- ecomoistflies$Ecomoist
ecomoistflies <- ecomoistflies[,2:378]
##Assigning row names only work with characters
ecomoistflies <- as.data.frame(ecomoistflies, row.names = as.character(ecomoistlist))

##Calculating diversity Indices
###Richness
ecomoistshannon <- diversity(ecomoistflies, index = "shannon", MARGIN = 1, base = exp(1))
###Evenness
ecomoistsimpson <- diversity(ecomoistflies, index = "simpson", MARGIN = 1, base = exp(1))
###Although Bray-Curtis good at detecting underlying gradient, Chao is better at handling
###Different sample sizes
ecomoistchao <- vegdist(ecomoistflies, method = "chao",binary=FALSE, diag=FALSE, upper=FALSE,
                     na.rm = FALSE)
###Margin =1 is number of specie per ecomoist, Margin = 2 is frequency of species
ecomoistrichness <- specnumber(ecomoistflies, MARGIN = 1)
ecomoistfrequency <- specnumber(ecomoistflies, MARGIN = 2)
###Abundance
ecomoistabundance <- flies %>% 
  select(Ecozone, Moisture_Regime, Abundance) %>% 
  unite(Ecomoist, Ecozone:Moisture_Regime, sep= "_") %>% 
  group_by(Ecomoist) %>%
  summarise_each(funs(sum))
####Remove first column with locality list, always alphabetical so all G
ecomoistabundance <- ecomoistabundance[,2]

##Bind everything into nice data frame for analysis
ecomoistanalysis <- cbind.data.frame(averaged_ecomoistclim[,1:2], ecomoistrichness, 
                                     ecomoistabundance, ecomoistshannon,ecomoistsimpson, 
                                     averaged_ecomoistclim[3:23], row.names = as.character(ecomoistlist))




# Diversity_Indices_per_Locality -------------------------------------------------------
##First goup everything per site
siteflies <- flies %>% 
  select(Locality, Species, Abundance) %>% 
  group_by(Locality, Species) %>%
  summarise_each(funs(sum)) %>%
  ###fill=0 means that 0 will be put instead of default NAs if the cell is to be empty
  spread(key = Species, Abundance, fill = 0)
##checking
View(siteflies)
str(siteflies)
##Making Locality row names
localist <- siteflies$Locality
siteflies <- siteflies[,2:378]
##Assigning row names only work with characters
siteflies <- as.data.frame(siteflies, row.names = as.character(localist))

##Calculating diversity Indices
###Richness
siteshannon <- diversity(siteflies, index = "shannon", MARGIN = 1, base = exp(1))
###Evenness
sitesimpson <- diversity(siteflies, index = "simpson", MARGIN = 1, base = exp(1))
###Although Bray-Curtis good at detecting underlying gradient, Chao is better at handling
###Different sample sizes
sitechao <- vegdist(siteflies, method = "chao",binary=FALSE, diag=FALSE, upper=FALSE,
                    na.rm = FALSE)
###Margin =1 is number of specie per site, Margin = 2 is frequency of species
siterichness <- specnumber(siteflies, MARGIN = 1)
sitefrequency <- specnumber(siteflies, MARGIN = 2)
###Abundance
siteabundance <- flies %>% 
  select(Locality, Abundance) %>% 
  group_by(Locality) %>%
  summarise_each(funs(sum))
####Remove first column with locality list, always alphabetical so all G
siteabundance <- siteabundance[,2]

##Bind everything into nice data frame for analysis
siteanalysis <- cbind.data.frame(averaged_localim[,2:3], siterichness, siteabundance,
                                 siteshannon,sitesimpson, averaged_localim[,4:24])






# Diversity_Indices_per_Locality_Moisture ---------------------------------
##First goup everything per site
moistflies <- flies %>% 
  select(Locality, Moisture_Regime, Species, Abundance) %>% 
  unite(Site, Locality:Moisture_Regime, sep= "_") %>% 
  group_by(Site, Species) %>%
  summarise_each(funs(sum)) %>%
  ###fill=0 means that 0 will be put instead of default NAs if the cell is to be empty
  spread(key = Species, Abundance, fill = 0)
##checking
View(moistflies)
str(moistflies)
##Making Locality row names
moistlist <- moistflies$Site
moistflies <- moistflies[,2:378]
##Assigning row names only work with characters
moistflies <- as.data.frame(moistflies, row.names = as.character(moistlist))

##Calculating diversity Indices
###Richness
moistshannon <- diversity(moistflies, index = "shannon", MARGIN = 1, base = exp(1))
###Evenness
moistsimpson <- diversity(moistflies, index = "simpson", MARGIN = 1, base = exp(1))
###Although Bray-Curtis good at detecting underlying gradient, Chao is better at handling
###Different sample sizes
moistchao <- vegdist(moistflies, method = "chao",binary=FALSE, diag=FALSE, upper=FALSE,
                    na.rm = FALSE)
###Margin =1 is number of specie per moist, Margin = 2 is frequency of species
moistrichness <- specnumber(moistflies, MARGIN = 1)
moistfrequency <- specnumber(moistflies, MARGIN = 2)
###Abundance
moistabundance <- flies %>% 
  select(Locality, Moisture_Regime, Abundance) %>% 
  unite(Site, Locality:Moisture_Regime, sep= "_") %>% 
  group_by(Site) %>%
  summarise_each(funs(sum))
####Remove first column with locality list, always alphabetical so all G
moistabundance <- moistabundance[,2]

##Bind everything into nice data frame for analysis
moistanalysis <- cbind.data.frame(averaged_moistclim[,2:4], moistrichness, moistabundance,
                                 moistshannon,moistsimpson, averaged_moistclim[,5:24])


