#Metadata

#Load Packages
library(EML)
library(digest)
library(devtools)

#Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Family",
      "Genus",
      "Species",
      "Abundance",
      "Locality",
      "Province.Territory",
      "Ecoclimatic_zone",
      "Side",
      "Latitude",
      "Longitude",
      "Moisture_Regime",
      "Replicate",
      "Date",
      "Trapping_Method"), 
    attributeDefinition = c(
      "Linnaean family of the specimen",
      "Linnaean genus of the specimen",
      "Linnaean binomial species name or morphospecies, may include original taxonomist name",
      "Number of specimens collected",
      "Where the sampling was done",
      "Province/Territory of locality",
      "Ecoclimatic zone of locality",
      "If the sampling was in eastern or western Canada",
      "Decimal latitude of the replicate (WGS-84 datum)",
      "Decimal longitude of the replicate (WGS-84 datum)",
      "Moisture regime of the replicate",
      "Replicate number",
      "Date of collection (days.month.year)",
      "Trapping Method"),
    formatString = c(
      NA,
      NA,
      NA,        
      NA,         
      NA,         
      NA,
      NA,         
      NA,         
      NA,
      NA,        
      NA,         
      NA,         
      NA,
      NA),
    definition = c(        
      NA,
      NA,
      NA,        
      NA,         
      NA,         
      NA,
      NA,         
      NA,         
      NA,
      NA,        
      NA,         
      NA,         
      NA,
      NA),
    unit = c(
      NA,
      NA,
      NA,
      "dimensionless",
      NA,         
      NA,
      NA, 
      NA,
      "dimensionless",
      "dimensionless",        
      NA,         
      "dimensionless",
      NA,
      NA),
    numberType = c(
      NA,
      NA,
      NA,
      "integer",
      NA,         
      NA,
      NA,         
      NA,         
      "coordinate",
      "coordinate",        
      NA,         
      "category",         
      NA,
      NA),
    stringsAsFactors = FALSE
  )

#Create vectors for factors
##Locality
Locality <- 
  c(Aulavik_National_Park = "Aulavik National Park, NT",
    Cambridge_Bay = "Cambridge Bay, NU",
    Churchill = "Churchill, MB",
    Goose_Bay = "Goose Bay, NL",
    Iqaluit = "Iqaluit, NU",
    Kugluktuk = "Kugluktuk, NU",
    Moosonee = "Moosonee, ON",
    Norman_Wells = "Norman Weels, NT",
    Schefferville = "Schefferville, QC",
    Tombstone_Mountains = "Dempster Highway near North Fork Pass, YT",
    Yellowknife = "Yellowknife, NT")
##Ecoclimatic zone
Ecoclimatic_zone <- 
  c(HA = "High Arctic",
    NB = "Northern Boreal",
    SA = "Subarctic")
##Side
Side <-
  c(W = "Western Canada",
    E = "Eastern Canada")
##Moisture regime
Moisture_Regime <- 
  c(Mesic ="Mesic sites", 
    Wet = "Wet sites",
    McGill_Subarctic_Research_Station = "Malaise Trap left at research station")

#Bind the factor vectors
factors <- rbind(
  data.frame(
    attributeName = "Locality",
    code = names(Locality),
    definition = unname(Locality)
  ),
    data.frame(
      attributeName = "Ecoclimatic_zone",
      code = names(Ecoclimatic_zone),
      definition = unname(Ecoclimatic_zone)
    ),
    data.frame(
      attributeName = "Side",
      code = names(Side),
      definition = unname(Side)
    ),
  data.frame(
    attributeName = "Moisture_Regime",
    code = names(Moisture_Regime),
    definition = unname(Moisture_Regime)
  ))

#Merge the two dataframes
attributeList <- 
  set_attributes(attributes, 
                 factors, 
                 col_classes =
                   c("character", "character", "character", "numeric", "factor",
                     "factor", "factor", "factor", "numeric", "numeric",
                     "factor", "factor", "factor", "character"))


#Create location for pre-XML CSV, then bind them
physical <- 
  set_physical("ArcticFlies.csv")

dataTable <- 
  new("dataTable",
      entityName = "ArcticFlies.csv",
      entityDescription = "Arctic Acalyptrate Fly Project",
      physical = physical,
      attributeList = attributeList)

#Add personal info
R_person <- 
  as.person("Pierre_Rogy <rogy@zoology.ubc.ca>")
Pierre_Rogy <-
  as(R_person, "creator")

#Add title and abstract
title <- 
  "Spatial distribution of acalyptrate fly assemblages in northern Canada"
abstract <- 
  "Flies are the dominant animal in Arctic ecosystems, and as such can be used to monitor biodiversity change, but prior research has often focused on the most common taxa. This paper examines the spatial diversity of the poorly-documented Acalyptratae flies using 3603 specimens collected from twelve sites across northern Canada. We predicted that Acalyptratae diversity should be higher in resource-rich habitats, and positively associated with seasonal and limiting temperatures. We also predicted that spatial turnover should result from the combination of biological, geographical and historical features. Climatic variables had a persistent effect on acalyptrate assemblages, consistent with the latitudinal richness gradient seen in other animal groups. However, habitat had a stronger effect on species composition than species richness, and no effect on abundance. Spatial turnover in composition was greatest between sites in the Subarctic zone, with no overlap across sites. The High Arctic ecoclimatic zone was as spatially heterogenous in composition as Northern Boreal and Subarctic zones ecoclimatic zones, hinting at complex interplays between geographical and glacial legacies. Overall, our results contradict the historical view of Arctic arthropod diversity as simple and uniform, and may serve as a baseline for future biodiversity monitoring."
#Merge everything
dataset <- 
  new("dataset",
      title = title,
      creator = Pierre_Rogy,
      abstract = abstract,
      dataTable = dataTable)

#Now write the XML file
write_eml(dataset,
          file = "ArcticFlies.xml")
