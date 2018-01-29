#Load data and check if it's alright, line depends on CPU
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
cleanflies <- flies %>% 
  select(-Genus, -Province.Territory, -Trapping_Method, -Date) %>% 
  ##Remove Mcgill Station
  filter(Moisture_Regime != "McGill_Subarctic_Research_Station")
##Make sure it works
str(cleanflies)


# Setting_Up_WorldClim_Values ---------------------------------------------
##Getting the data, bio means all 19 variables, and res=2.5 is 21.62 square meter resolution
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
###Now Combine the two data frames
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
locaclim <- locaclim %>% select(-Locality, -Average.Wind.Speed, -site, Mean.Annual.Temp, -Total.Precipitation)
locaclim <- as.data.frame(locaclim, row.names = as.character(locality_climate$Locality))
View(locaclim)






