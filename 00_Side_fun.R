#Load data and check if it's alright
flies <- read.csv("C:/Users/Pierre/OneDrive/Projects/arctic_flies/Data/Inventory.csv", sep = ";")
View(flies)
str(flies)


Beginning of side analysis
##Isolating vaiables of interest
###NOTE that maybe worldclim factors are different in wet vs mesic areas? What about sides?
ecoclim <- anaflies %>% select(-Locality, -Family, -Species, -Number.of.Specimens, -Moisture_Regime, -Replicate)
##Extracting unique combinations of site, coordinates, and corresponding variables
##Checking if no mess up
variaecoclim <- unique(ecoclim)
str(variaecoclim)
##Averaging everything using dplyr
averaged_ecoclim <- variaecoclim %>%
  group_by(Ecozone, Side) %>%
  summarise_each(funs(mean))
View(averaged_ecoclim)


