#METADATA FOR GIT
##Load Packages
#Load Packages
devtools::install_github("ropensci/EML", build_vignettes = TRUE)
library("EML")
library(devtools)
##Set list of columns, with definitions and details
attributes <-
  data.frame(
    attributeName = c(
      "Family",
      "Genus",
      "Species",
      "Abundance",
      "Locality",
      "Province/Territory",
      "Ecozone",
      "Side",
      "Latitude",
      "Longitude",
      "Moisture_Regime",
      "Replicate",
      "Date",
      "Trapping Method"), 
    attributeDefinition = c(
      "Linnaean family of the specimen",
      "Linnaean genus of the specimen",
      "Linnaean binomial species name or morphospecies, may include original taxonomist name",
      "Number of specimens collected",
      "Where the sampling was done",
      "Province/Territory of locality",
      "Ecozone of locality",
      "If the sampling was in eastern or western Canada",
      "Latitude of the replicate (WGS-84 datum)",
      "Longitude of the replicate (WGS-84 datum)",
      "Moisture regime of the replicate",
      "Replicate number",
      "Date of collection",
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
      "Decimal WGS-84",
      "Decimal WGS-84",        
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

##Create vectors for the factors, as it is better method
Locality <- c(Moosonee = "Moosonee, ON",
              Goose_Bay = "Goose Bay, NL",
              Yellowknife = "Yellowknife, NT",
              Norman_Wells = "Norman Weels, NT",
              Churchill = "Churchill, MB",
              Kugluktuk = "Kugluktuk, NU",
              Schefferville = "Schefferville, QC",
              Tombstone_Mountains = "Dempster Highway near North Fork Pass, YT",
              Iqaluit = "Iqaluit, NU",
              Aulavik_National_Park = "Aulavik National Park, NT",
              Cambridge_Bay = "Cambridge Bay, NU")

Ecozone <- c(HA = "High Arctic",
             NB = "Northern Boreal",
             SA = "Subarctic")

Side <- c(W = "Western Canada",
          E = "Eastern Canada")


Moisture_Regime <- c(Mesic ="Mesic sites", 
                     Wet = "Wet sites",
                     McGill_Subarctic_Research_Station = "Malaise Trap left at research station")


##Now bind the factor vectors
factors <- rbind(
  data.frame(
    attributeName = "Locality",
    code = names(Locality),
    definition = unname(Locality)
  ),
    data.frame(
      attributeName = "Ecozone",
      code = names(Ecozone),
      definition = unname(Ecozone)
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

##Merge the two dataframes
attributeList <- set_attributes(attributes, factors, col_classes =
                                  c("factor", "factor", "factor", "numeric", "factor",
                                    "factor", "factor", "factor", "numeric", "numeric",
                                    "factor", "factor", "factor", "factor"))


##Create location for pre-XML CSV, then bind them
physical <- set_physical("ArcticFlies.csv")

dataTable <- new("dataTable",
                 entityName = "ArcticFlies.csv",
                 entityDescription = "Arctic Fly Project",
                 physical = physical,
                 attributeList = attributeList)

##Add personal info
R_person <- as.person("Pierre_Rogy <rogy@zoology.ubc.ca>")
Pierre_Rogy <-as(R_person, "creator")

##Add title and abstract
title <- "Spatial Diversity of Acalyptrate Flies in the Northern Nearctic"
abstract <- "Solving the riddle of Arctic Diptera dominance requires a comprehensive framework of diversity patterns, and is especially important as this taxon can be used to assess shifts in rapidly-changing Arctic ecosystems. Thus, this paper aims at unraveling the spatial component of this framework for Acalyptratae. Acalyptrate diversity should decrease with increasing latitude for most groups, while abundance per species should increase. On the same gradient, the moisture regime should influence diversity in northern, resource-scarce environments. To study those patterns, the Northern Biodiversity Program (2010-2011) collected arthropods at 12 sites across the Northern Boreal, Subarctic and High Arctic zones. This yielded 4486 acalyptrate specimens, whose biodiversity patterns are compared here across different spatial scales. The latitudinal richness gradient was followed by 22 of the 25 families, but abundance patterns remained similar across zones. Subarctic sites showed most variability, which may result from geographical and glacial legacies. Finally, wet sites harbored more diversity and abundance with increasing latitude, except in the High Arctic, likely due to mating and dispersal behaviors. In conclusion, acalyptrate diversity in the Arctic generally follows patterns seen in other animal groups. These results can be used as bases for further temporal and ecological studies to understand Arctic shifts. "

##Merge everything
dataset <- new("dataset",
               title = title,
               creator = Pierre,
               abstract = abstract,
               dataTable = dataTable)

##Now write the XML file
write_eml(dataset,
          file = "ArcticFlies.xml")
