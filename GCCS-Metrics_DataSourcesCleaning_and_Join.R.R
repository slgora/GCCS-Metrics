### Project: Global Crop Conservation Strategies Metrics ###
### Data sources cleaning individually and Join 
### GCCS-Metrics_DataSourcesCleaning_and_Join.R
### by Sarah Gora
### Date created: 2024_11_06


#### Set working directory ####
setwd()
setwd("C:/Users/sgora/Desktop/GCCS-Metrics/Code/DataCleaning_and_Join/GCCS-Metrics_DataSourcesCleaning_and_Join.R")

#### Install packages ####
install.packages("readr")
library(readr)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
install.packages("TNRS")
libaray(TNRS)


## 5 Databases (dont combine plant treaty data bc it is just ditributions data)
# (1). BGCI Plant Search
# (2). FAO WIEWS
# (3). GBIF
# (4). Genesys PGR
# (5). SGSV

## 3 Other sources of information:
# Plants that Feed the World 
# (1). SDBG study - has 98 metrics, some of which will be useful (e.g. UPOV registrations)
# (2). FAO SOW III Germplasm exchange - FAO; SDBG study - has data from Plant Treaty Data Store and from FAO Views from 2012-2019 on germplasm distributions.  
# (3). Global Crop Conservation Strategies database (SDBG project)
# (4). Plant Treaty GLIS (distributions)




####################################################################################################
########### Read in all database data for all crops ################################################

# Unformatted Data from the data sources to combine: 
BGCI_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/BGCI_allcrops_unformatted.xlsx")
WIEWS_allcrops_unformatted <- read.csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/SDBGExtractRequest/SDGBrequestExp.csv", header=FALSE)
Genesys_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")
GBIF_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/GBIF_allcrops_unformatted.xlsx")
SGSV_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/SGSV_allcrops_unformatted.xlsx")



####################################################################################################
########## Change all field names to be the same format ############################################
## Genesys naming style
## standardize and encode some fields for individual data sources as needed


############### BGCI Plant Search ####################
library(readxl)
BGCI_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/BGCI_allcrops_unformatted.xlsx")
# View(BGCI_allcrops_unformatted)
# colnames(BGCI_allcrops_unformatted)
BGCI_allcrops <- BGCI_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(BGCI_allcrops) <- c("source",
                                        "fullSciName",
                                         "fullTaxa",
                                         "accepted",
                                         "plantSearchId",
                                         "cultivar",
                                         "exSituSiteGardenSearchId",
                                         "instName",
                                         "city",
                                         "stateProvince",
                                         "origCtyFullName",
                                         "country2", #2 letter abrev for origCty
                                         "latitude", #of garden, dont use
                                         "longitude", #of garden, dont use 
                                         "germplasmPlant",
                                         "storage", #changed from germplasmSeed
                                         "germplasmPollen",
                                         "germplasmExplant",
                                         "acceptedNamePlantSearch",
                                         "synNamePlantSearch",
                                          "addedSubmittedNames")

# Fields we want to keep:
library(magrittr)
library(dplyr)
BGCI_allcrops <- subset(BGCI_allcrops, select = c(fullSciName, fullTaxa, 
                                                  cultivar, origCtyFullName,
                                                  country2, storage)) %>%
                  mutate(across(everything(), ~gsub("[[:punct:]]", "", .x)))  #remove special characters

# Add fields: 
# cropName, strategy
BGCI_allcrops <- cbind(BGCI_allcrops, cropName=" ", strategy= " ", data_source = "BGCI")


# Separate fields: fullSciName, still have fullTaxa
# fullSciName into genus and species
library(tidyr)
BGCI_allcrops  <- BGCI_allcrops  %>% separate(fullSciName, c('genus', 'species'))


## Encode the storage field with presence of seed values "1"
# replace '1' in storage column with '10' (code for seed)
BGCI_allcrops['storage'][BGCI_allcrops['storage'] == 1] <- 10 
BGCI_allcrops['storage'][BGCI_allcrops['storage'] == 0] <- NA


## Encode origCty field
library(readr)
geo_names <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Passport/geo_names.csv")

## subset only the relevant column to join:
# 2 letter abreviation for country and the 3 letter abreviation
geo_names_2 <- subset(geo_names, select = c(country2, country3))

library(dplyr)
BGCI_allcrops <- BGCI_allcrops %>%
  left_join(geo_names_2, by = "country2") %>%
  rename(origCty=country3) %>%
  select(-country2) #removed the 2 letter country column

# row 18220- replace NAM with a NA/blank, other NAM values are true abbreviations for Nambia
# Replace the "NAM" value in row 18220, column "origCty"
BGCI_allcrops[18220, "origCty"] <- NA


View(BGCI_allcrops)





############### WIEWS ####################
library(readr)
WIEWS_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/SDBGExtractRequest/WIEWS_allcrops_unformatted.csv")
# View(WIEWS_allcrops_unformatted)
# colnames(WIEWS_allcrops_unformatted)
WIEWS_allcrops <- WIEWS_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(WIEWS_allcrops) <- c("holdingCty",
                                    "instCode",
                                    "acceNumb",
                                    "fullTaxa",
                                    "genus",
                                    "species",
                                    "acceptedGenus",
                                    "acceptedSpecies",
                                    "cropName",
                                    "acqDate",
                                    "origCty",
                                    "sampStat",
                                    "duplSite",
                                    "duplInstName",
                                    "latitude", 
                                    "longitude",
                                    "acqSRC_WIEWS",
                                    "storage",
                                    "mlsStat",
                                    "doi")

# Fields we want to keep:
WIEWS_allcrops <- subset(WIEWS_allcrops, select = c(holdingCty, instCode, acceNumb, 
                            fullTaxa,genus, species, cropName, acqDate, origCty, 
                            sampStat, duplSite, duplInstName, latitude,longitude, 
                            acqSRC_WIEWS, storage, mlsStat, doi))

# Add fields: 
# strategy (empty for now, write code to fill out later)
WIEWS_allcrops <- cbind(WIEWS_allcrops, strategy= " ", data_source = "WIEWS")

## Standardize acceNumb field
## remove blank/space between institute abbreviation and number
install.packages("stringr")
library(stringr)
library(dplyr)
WIEWS_allcrops  <- WIEWS_allcrops  %>%
  mutate(acceNumb = str_replace_all(acceNumb, " ", ""))

# Need to standardize the acquire date format (later maybe as needed)

View(WIEWS_allcrops)



############### Genesys PGR ####################
library(readr)
Genesys_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")
# View(Genesys_allcrops_unformatted)
# colnames(Genesys_allcrops_unformatted)
Genesys_allcrops <- Genesys_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(Genesys_allcrops) <- c("rowNumb",
                                      "source",
                                      "instCode",
                                      "doi",
                                      "acceNumb",
                                      "historic",      
                                      "curation",
                                      "genus",
                                      "species",
                                      "spAuthor",   
                                      "subTaxa",
                                      "subTAuthor",
                                      "grin_Taxon_Id",  ## didnt save grin taxon info
                                      "grin_Name",  
                                       "grin_Author",
                                       "cropName",
                                       "cropCode",
                                       "sampStat", #code
                                       "acqDate",
                                       "acceName_Genesys",
                                       "origCty",
                                       "collSite",
                                       "latitude", #latitude of collection site?
                                       "longitude", #longitude of collection site?
                                       "coordDuncert",
                                       "coordDatum",    
                                       "geoRefmeth",
                                       "elevation",
                                       "collDate",
                                       "collSrc",
                                       "collNumb",
                                       "collCode",
                                       "CollName",
                                       "collInstAddress",
                                       "collMissId",
                                       "donorCode_Genesys",
                                       "donorName_Genesys",
                                       "donorNumb_Genesys",
                                       "otherNumb_Genesys",
                                       "bredCode",
                                       "bredName",
                                       "ancest_Genesys",
                                       "duplSite",
                                       "duplInstName",
                                       "storage",
                                       "mlsStat",
                                       "acceUrl",
                                       "remarks",
                                       "dataProviderId_Genesys",
                                       "uuId",
                                       "lastModified" )
## fields to keep: 
##### KEPT WAYYYY TOO MANY FIELDS, KEEP CUTTING DOWN ######
Genesys_allcrops <- subset(Genesys_allcrops, select = c(instCode, doi, acceNumb, 
                                                    genus, species, 
                                                    # spAuthor, subTaxa, subTAuthor,
                                                    cropName, sampStat, acqDate, acceName_Genesys,
                                                    origCty, latitude,longitude,
                                                    donorCode_Genesys,donorName_Genesys, donorNumb_Genesys,
                                                    otherNumb_Genesys, ancest_Genesys,
                                                    duplSite, duplInstName, storage,
                                                    mlsStat, dataProviderId_Genesys))

# Add fields: 
# strategy (empty for now, write code to fill out later)
Genesys_allcrops <- cbind(Genesys_allcrops, strategy= " ", data_source = "Genesys")

# Duplicate genus, species fields 
Genesys_allcrops$genus2 = Genesys_allcrops$genus
Genesys_allcrops$species2 = Genesys_allcrops$species

#Combine genus2, species2, spAuthor, subTaxa, subTAuthor to make one field = FullTaxa
library(tidyr)
Genesys_allcrops <- unite(Genesys_allcrops, 
                          fullTaxa, genus2, species2,
                          sep = " ", na.rm = TRUE)

# encode mlsStat field
# replace "TRUE" in mlsStat field to be "I" (included), "FALSE" as "N" (not included)
Genesys_allcrops['mlsStat'][Genesys_allcrops['mlsStat'] == "TRUE"] <- "I"
Genesys_allcrops['mlsStat'][Genesys_allcrops['mlsStat'] == "FALSE"] <- "N"

## Standardize acceNumb field
## remove blank/space between institute abbreviation and number
Genesys_allcrops <- Genesys_allcrops %>%
  mutate(acceNumb = str_replace_all(acceNumb, " ", ""))

View(Genesys_allcrops)

# SG Notes: 
# Need to standardize the acquire date field format (later maybe as needed)

# what is instName for Genesys? there is no full instName, only instCode


View(Genesys_allcrops)




############### GBIF ####################

# Read in as a csv, not excel
# helped eliminate data loss
library(readr)
GBIF_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/GBIF_allcrops_unformatted.csv")
View(GBIF_allcrops_unformatted)


# data loss test:
# unique(GBIF_allcrops_unformatted$disposition)

# colnames(GBIF_allcrops_unformatted)
GBIF_allcrops <- GBIF_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(GBIF_allcrops) <- c("acceNumb",
                             "sharing",
                             "bibliographicCitation",
                             "language",
                             "license",
                             "modified",
                             "publisher",
                             "references",
                             "rightsHolder",
                             "type",
                             "institutionID",
                             "collectionID",
                             "doi",
                             "instAcronym",
                             "acqSRC_GBIF",
                             "datasetName",
                             "ownerInstCode", #not helpful mostly empty also
                             "basisOfRecord",
                             "informationWithheld",
                             "dataGeneralizations",
                             "dynamicProperties",
                             "occurrenceID_GBIF",
                             "catalogNumber_GBIF",
                             "acceNumb",
                             "recordedBy",
                             "recordedByID",
                             "individualCount",
                             "organismQuantity",
                             "organismQuantityType",
                             "sex",
                             "lifeStage",
                             "reproductiveCondition",
                             "caste",
                             "behavior",
                             "vitality",
                             "establishmentMeans",
                             "degreeOfEstablishment",
                             "pathway",
                             "georeferenceVerificationStatus",
                             "occurrenceStatus",
                             "storage",
                             "disposition",
                             "associatedOccurrences",
                             "associatedReferences",
                             "associatedSequences",
                             "associatedTaxa",
                             "otherCatalogNumbers_GBIF",
                             "occurrenceRemarks",
                             "organismID",
                             "organismName",
                             "organismScope",
                             "associatedOrganisms",
                             "previousIdentifications",
                             "organismRemarks",
                             "materialEntityID",
                             "materialEntityRemarks",
                             "verbatimLabel",
                             "materialSampleID",
                             "eventID",
                             "parentEventID",
                             "eventType",
                             "fieldNumber",
                             "eventDate",
                             "eventTime",
                             "startDayOfYear",
                             "endDayOfYear",
                             "year",
                             "month",
                             "day",
                             "verbatimEventDate",
                             "habitat",
                             "samplingProtocol",
                             "sampleSizeValue",
                             "sampleSizeUnit",
                             "samplingEffort",
                             "fieldNotes",
                             "eventRemarks",
                             "locationID",
                             "higherGeographyID",
                             "higherGeography",
                             "continent",
                             "waterBody",
                             "islandGroup",
                             "island",
                             "country2", 
                             "stateProvince",
                             "county",
                             "municipality",
                             "instName",
                             "verbatimLocality",
                             "verbatimElevation",
                             "verticalDatum",
                             "verbatimDepth",
                             "minimumDistanceAboveSurfaceInMeteters",
                             "maximumDistanceAboveSurfaceInMeters",
                             "locationAccordingTo",
                             "locationRemarks",
                             "latitude",
                             "longitude",
                             "coordinateUncertaintyInMeters",
                             "coordinatePrecision",
                             "pointRadiusSpatialFit",
                             "verbatimCoordinateSystem",
                             "verbatimSRS",
                             "footprintWKT",
                             "footprintSRS",
                             "footprintSpatialFit",
                             "georeferencedBy",
                             "georeferencedDate",
                             "georeferenceProtocol",
                             "georeferenceSources",
                             "georeferenceRemarks",
                             "geologicalContextID",
                             "earliestEonOrLowestEonothem",
                             "latestEonOrHighestEonothem",
                             "earliestEraOrLowestErathem",
                             "latestEraOrHighestErathem",
                             "earliestPeriodOrLowestSystem",
                             "latestPeriodOrHighestSystem",
                             "earliestEpochOrLowestSeries",
                             "latestEpochOrHighestSeries",
                             "earliestAgeOrLowestStage",
                             "latestAgeOrHighestStage",
                             "lowestBiostratigraphicZone",
                             "highestBiostratigraphicZone",
                             "lithostratigraphicTerms",
                             "group",
                             "formation",
                             "member",
                             "bed",
                             "identificationID",
                             "verbatimIdentification",
                             "identificationQualifier",
                             "typeStatus",
                             "identifiedBy",
                             "identifiedByID",
                             "dateIdentified",
                             "identificationReferences",
                             "identificationVerificationStatus",
                             "identificationRemarks",
                             "taxonID",
                             "scientificNameID",
                             "acceptedNameUsageID",
                             "parentNameUsageID",
                             "originalNameUsageID",
                             "nameAccordingToID",
                             "namePublishedInID",
                             "taxonConceptID",
                             "fullTaxa",
                             "acceptedNameUsage",
                             "parentNameUsage",
                             "originalNameUsage",
                             "nameAccordingTo",
                             "namePublishedIn",
                             "namePublishedInYear",
                             "higherClassification",
                             "kingdom",
                             "phylum",
                             "class",
                             "order",
                             "superfamily",
                             "family",
                             "subfamily",
                             "tribe",
                             "subtribe",
                             "genusSynonym",
                             "genus",
                             "subgenus",
                             "infragenericEpithet",
                             "species",
                             "infraspecificEpithet",
                             "cultivarEpithet",
                             "taxonRank",
                             "verbatimTaxonRank",
                             "vernacularName",
                             "nomenclaturalCode",
                             "taxonomicStatus",
                             "nomenclaturalStatus",
                             "taxonRemarks",
                             "datasetKey",
                             "publishingCountry",
                             "lastInterpreted",
                             "elevation",
                             "elevationAccuracy",
                             "depth",
                             "depthAccuracy",
                             "distanceFromCentroidInMeters",
                             "issue",
                             "mediaType",
                             "hasCoordinate",
                             "hasGeospatialIssues",
                             "taxonKey",
                             "acceptedTaxonKey",
                             "kingdomKey",
                             "phylumKey",
                             "classKey",
                             "orderKey",
                             "familyKey",
                             "genusKey",
                             "subgenusKey",
                             "speciesKey",
                             "species2",
                             "acceptedScientificName",
                             "verbatimScientificName",
                             "typifiedName",
                             "protocol",
                             "lastParsed",
                             "lastCrawled",
                             "repatriated",
                             "relativeOrganismQuantity",
                             "projectId",
                             "isSequenced",
                             "gbifRegion",
                             "publishedByGbifRegion",
                             "origCty",
                             "level0Name",
                             "level1Gid",
                             "level1Name",
                             "level2Gid",
                             "level2Name",
                             "level3Gid",
                             "level3Name",
                             "iucnRedListCategory"    )

## fields to keep: 
##### KEPT WAYYYY TOO MANY FIELDS, KEEP CUTTING DOWN ######
GBIF_allcrops <- subset(GBIF_allcrops, select = c(acceNumb, doi, instAcronym, acqSRC_GBIF,
                                                  occurrenceID_GBIF, catalogNumber_GBIF,
                                                  storage, associatedTaxa, 
                                                  otherCatalogNumbers_GBIF, country2, 
                                                  instName, latitude, longitude, 
                                                  fullTaxa, genusSynonym, genus, species, origCty ))

# Add fields: 
# strategy (empty for now, write code to fill out later)
GBIF_allcrops <- cbind(GBIF_allcrops, strategy= " ", cropName= " ", data_source = "GBIF")


## Encode abbreviation field to 3 letter country and then add to origCty column
## Replace the GBIF country abbreviation with the ISO3 name 
## Already a column with 3 letter country of origin but can extract MORE data from 
# other accessions that only filled out the 2 letter county code field 

geo_names <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Passport/geo_names.csv")

## subset only the relevant column to join- 2 letter country code and the 3 letter abreviation
geo_names_2 <- subset(geo_names, select = c(country2, country3))

library(dplyr)
GBIF_allcrops <- GBIF_allcrops %>%
  left_join(geo_names_2, by = "country2") %>%
  select(-country2) #removed the 2 letter country column


# repace all NAM with blanks/Nas 
# initally picked up and read NA as a 2 letter country code for Nambia
# did check and Nambia is NOT present as a country in this dataset
# didnt work to remove NAs initially
GBIF_allcrops['country3'][GBIF_allcrops['country3'] == "NAM" ] <- NA


## join origCty (already formatted to be 3 letter country code and country3 column just created)
## if there is an NA, then move over country data to origCty
GBIF_allcrops <- GBIF_allcrops %>%
  mutate(origCty = if_else(is.na(origCty), country3, origCty),
         country3 = if_else(is.na(country3), origCty, country3)) %>%
        select(-country3) #removed extra column
  
                                
## Encode storage field

# Encode categories of storage
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Cryo-conserved seeds;-20 °C" ] <- "10; 13; 40"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Cryo-conserved seeds;4° C (Paper Bags), -20 °C (Sealed Cans)"] <- "10; 13; 40"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Dried at 4 °C;Cryo-conserved seeds;-20 °C"] <- "10; 13; 40"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Dried at 4 Â°C;Cryo-conserved seeds;-20 Â°C"] <- "10; 13; 40"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "vacuumed jar"] <- "99"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "field collection"] <- "20"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Planting"] <- "99"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "material vegetal"] <- "99"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Plant voucher specimen; Genetic sequence"] <- "50"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Ejemplar completo"] <- "99"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Semilla"] <- "10"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "whole organism"] <- "99"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "Living Specimen"] <- "99"
GBIF_allcrops['storage'][GBIF_allcrops['storage'] == "seed"] <- "10"

  
View(GBIF_allcrops)



                                
############### SGSV ####################
library(readxl)
SGSV_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/SGSV_allcrops_unformatted.xlsx")
SGSV_allcrops <- SGSV_allcrops_unformatted

# View(SGSV_allcrops_unformatted)
# colnames(SGSV_allcrops_unformatted)

#CHANGE ALL NAMES TO GENESYS STYLE
colnames(SGSV_allcrops) <- c( "source",
                                    "instName",
                                    "instCode",
                                    "instAcronym",
                                    "acceNumb",
                                    "fullTaxa",
                                    "fullTaxa1",
                                    "ctyFullName") #origCtyFullName
## Keep fields
SGSV_allcrops <- subset(SGSV_allcrops, select = c(instName, instCode, instAcronym, 
                                                  acceNumb, fullTaxa, fullTaxa1, 
                                                  ctyFullName))

# Add fields: 
# strategy (empty for now, write code to fill out later)
SGSV_allcrops <- cbind(SGSV_allcrops, data_source = "SGSV", strategy= " ", cropName = " ")

# Split scientific name column (fullTaxa1) into genus and species
library(tidyr)
SGSV_allcrops  <- SGSV_allcrops  %>% separate(fullTaxa1, c('genus', 'species'))

## dont need this info
# Add instCountry field and encode 
## use the first 3 letters of instCode and make new field, "instCountry" 
# the 3 letter code for the country in which the holding institute is located


## Standardize acceNumb field
## remove blank/space between institute abbreviation and number
SGSV_allcrops <- SGSV_allcrops %>%
  mutate(acceNumb = str_replace_all(acceNumb, " ", ""))


## Encode origCty field
geo_names <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Passport/geo_names.csv")

## subset only the relevant column to join- full country name spelled out and the 3 letter abreviation
geo_names_fullName <- subset(geo_names, select = c(ctyFullName, country3))

library(dplyr)
SGSV_allcrops <- SGSV_allcrops %>%
  left_join(geo_names_fullName, by = "ctyFullName") %>%
  rename(origCty=country3)

# Replace "unknown" countries with Nas
SGSV_allcrops['ctyFullName'][SGSV_allcrops['ctyFullName'] == "Unknown" ] <- NA

View(SGSV_allcrops)





####################################################################################################
## Remove duplicates in Genesys and WIEWS, WIEWS ##################################################

# keep Genesys records
# filter by acceNumb AND instCode
library(dplyr)

# Check for duplications/common records between WIEWS and Genesys
common_rows <- inner_join(WIEWS_allcrops, Genesys_allcrops, by = c("acceNumb", "instCode"))
print(common_rows)
duplications_removedfromWIEWS <- common_rows
# over 1M rows in common with same acceNumb and instCode
# exact number of rows in common: 1,094,530

## Remove duplicated in WIEWS
# if acceNumb and instCode are the same in Genesys and WIEWS, then remove row/accession record in WIEWS
WIEWS_allcrops_duprmv <- WIEWS_allcrops %>%
  anti_join(Genesys_allcrops, by = c("acceNumb", "instCode"))
# over 1M rows of data lost
# exact number of rows lost:             check against common row count above^^^^^
# 2,430,810 (WIEWS_allcrops) - 1,336,391 (WIEWS_allcrops_duprmv) = 1,094,419

# Check for duplications removed from Genesys and WIEWs_allcrops_duprmv
common_rows_check <- inner_join(WIEWS_allcrops_duprmv, Genesys_allcrops, by = c("acceNumb", "instCode"))
print(common_rows_check)
# 0 rows in common with same acceNumb and instCode
# also checked a few records by hand search in files 
# to see if duplicate records existed in Genesys and the new WIEWS file with duplicates removed
  

View(WIEWS_allcrops_duprmv)


# Summary of WIEWS with duplications removed by genus
WIEWS_allcrops_duprmv_sum <- WIEWS_allcrops_duprmv %>%
  group_by(, genus) %>%
  summarise(count = n())

#save as file
library(writexl)
write_xlsx(WIEWS_allcrops_duprmv_sum, path = "C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_duprmv_genuscount.xlsx")






####################################################################################################
########################### Standardize full taxon names with TNRS  ################################
install.packages("TNRS")
library(TNRS)


## set function and parameters
#' Resolve plant taxonomic names
#'
#' Resolve plant taxonomic names.
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed). Note that these two columns must be in this order. Alternatively, a character vector of names can be supplied.
#' @param sources Character. Taxonomic sources to use. Default is c("wcvp", "wfo"). Options include "wfo", "wcvp", and "cact". Use TNRS_sources() for more information.
#' @param classification Character. Family classification to use. Currently options include "wfo" (the default).
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned. If left NULL, the default threshold will be used.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param name_limit Numeric. The maximum number of names to check in one batch.  The default is 5000 and is usually the fastest option.  This cannot exceed 5000.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note wfo = World Flora Online, wcvp = World Checklist of Vascular Plants, cact = Cactaceae at Caryophyllales.org.
#' @note For queries of more than 5000 names, the function will automatically divide the query into batches of 5000 names and then run the batches one after the other. Thus, for very large queries this may take some time. When this is the case, a progress bar will be displayed.
#' @note IMPORTANT: Note that parallelization of queries is automatically handled by the API, and so there is no need to further parallelize in R (in fact, doing so may actually slow things down!).
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples \dontrun{
#' # Take a subset of the testfile to speed up runtime
#' tnrs_testfile <- tnrs_testfile[1:20, ]
#'
#' results <- TNRS(taxonomic_names = tnrs_testfile)
#'
#' # Inspect the results
#' head(results, 10)
#' }
#'
TNRS <- function(taxonomic_names,
                 sources = c("wcvp", "wfo"),
                 classification = "wfo",
                 mode = "resolve",
                 matches = "best",
                 accuracy = NULL,
                 skip_internet_check = FALSE,
                 name_limit = 5000,
                 ...) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }
  
  # If taxonomic names are supplied as a character string, make them into a data.frame
  
  if (inherits(x = taxonomic_names, what = "character")) {
    taxonomic_names <- as.data.frame(cbind(1:length(taxonomic_names), taxonomic_names))
  }
  
  
  # Specify the limit of names for the TNRS
  
  if (name_limit > 5000) {
    message("name_limit cannot exceed 5000, fixing")
    name_limit <- 5000
  }
  
  # Check that accuracy makes sense
  
  if (!class(accuracy) %in% c("NULL", "numeric")) {
    stop("accuracy should be either numeric between 0 and 1, or NULL")
  }
  
  # Check that sources are valid
  if (!all(sources %in% c("wfo", "wcvp", "cact"))) {
    message("Invalid source(s) specified. Current options are: wfo, wcvp, cact ")
    return(invisible(NULL))
  }
  
  
  # Check that classification is valid
  if (length(classification) != 1 | !classification %in% c("wfo")) {
    message("Invalid classification specified. Current options are: wfo ")
    return(invisible(NULL))
  }
  
  # Check that mode is valid
  if (length(mode) != 1 | !mode %in% c("resolve", "parse")) {
    message("Invalid mode specified. Current options are: resolve, parse ")
    return(invisible(NULL))
  }
  
  # Check that matches is valid
  if (length(matches) != 1 | !matches %in% c("best", "all")) {
    message("Invalid mode specified. Current options are: best, all ")
    return(invisible(NULL))
  }
  
  # reformat sources to match API input
  sources <- paste0(sources, collapse = ",")
  
  
  
  # If there are less than the max number of names allowable, send them to the base package
  if (nrow(taxonomic_names) <= name_limit) {
    return(TNRS_base(
      taxonomic_names = taxonomic_names,
      sources = sources,
      classification = classification,
      mode = mode,
      matches = matches,
      accuracy = accuracy,
      skip_internet_check = skip_internet_check,
      ...
    ))
  } #
  
  # If there are more than the max number of records, divide them into chunks and process the chunks
  
  
  
  if (nrow(taxonomic_names) > name_limit) {
    nchunks <- ceiling(nrow(taxonomic_names) / name_limit)
    
    # set up progress bar
    pb <- txtProgressBar(
      min = 0, # Minimum value of the progress bar
      max = nchunks, # Maximum value of the progress bar
      style = 3, # Progress bar style (also available style = 1 and style = 2)
      # width = 50,   # Progress bar width. Defaults to getOption("width")
      char = "="
    ) # Character used to create the bar
    
    
    for (i in 1:nchunks) {
      # Use the first batch of results to set up the output file
      if (i == 1) {
        results <- TNRS_base(
          taxonomic_names = taxonomic_names[(((i - 1) * name_limit) + 1):(i * name_limit), ],
          sources = sources,
          classification = classification,
          mode = mode,
          matches = matches,
          accuracy = accuracy,
          skip_internet_check = skip_internet_check,
          ...
        )
        
        # results<-matrix(nrow = nrow(taxonomic_names),ncol = ncol(results_i))
        # $results <- as.data.frame(results,stringsAsFactors = F)
        # colnames(results)<-colnames(results_i)
        # results[(((i-1)*name_limit)+1):(i*name_limit),]<-results_i
        # rm(results_i)
      } # for first batch
      
      
      # For last batch
      if (i == nchunks) {
        results <- rbind(
          results,
          TNRS_base(
            taxonomic_names = taxonomic_names[(((i - 1) * name_limit) + 1):(nrow(taxonomic_names)), ],
            sources = sources,
            classification = classification,
            mode = mode,
            matches = matches,
            accuracy = accuracy,
            skip_internet_check = skip_internet_check,
            ...
          )
        )
      } # last batch
      
      
      # middle bits
      if (i != nchunks & i != 1) {
        results <- rbind(
          results,
          TNRS_base(
            taxonomic_names = taxonomic_names[(((i - 1) * name_limit) + 1):(i * name_limit), ],
            sources = sources,
            classification = classification,
            mode = mode,
            matches = matches,
            accuracy = accuracy,
            skip_internet_check = skip_internet_check,
            ...
          )
        )
      } # middle bits
      
      setTxtProgressBar(pb, i)
    } # i loop
  } # if more than 10k
  
  
  close(pb)
  return(results)
} # fx



## TNRS Taxon Cleaning for data sources:
# Dataset containing two columns: scientific name and row number
# run TNRS by fullTaxa field 
#            -full taxonomic name with as much information as possible
#            -found in all data sources
## SG note: TNRS would not run on combined dataset, im guessing too large of processing 
# attempted twice and ran overnight 
# will have to clean taxon by individual data source to break up processing into chunks


## BGCI
BGCI_allcrops_fullTaxa <- BGCI_allcrops[c("fullTaxa")]
TNRS_BGCI_cropdata <- BGCI_allcrops_fullTaxa
head(TNRS_BGCI_cropdata, n = 20)

# Note that there are a variety of formats represented here, sometimes including
# scientific name only
# genus only
# family and genus
# family, scientific name, and author

# run TNRS, may take 30 min to seveveral hours
results <- TNRS(taxonomic_names = TNRS_BGCI_cropdata)
  
# Inspect the results
head(results, 10)

# The output includes information on the name submitted, the match score (how close the match is), the name matched, the status of the matched name, and the accepted name.
View(results)
TNRS_BGCI_results <- results
write_xlsx(TNRS_BGCI_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/TNRS_BGCI_results.xlsx")



## WIEWS (duplication removed data)
WIEWS_allcrops_fullTaxa <- WIEWS_allcrops_duprmv[c("fullTaxa")]
TNRS_WIEWS_cropdata <- WIEWS_allcrops_fullTaxa
head(TNRS_WIEWS_cropdata, n = 20)
   ## run TNRS taxon clean
results <- TNRS(taxonomic_names = TNRS_WIEWS_cropdata)
   ## view results
head(results, 10)
View(results)
   ## save results
TNRS_WIEWS_results <- results
write_xlsx(TNRS_WIEWS_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/TNRS_WIEWS_results.xlsx")



## Genesys
Genesys_allcrops_fullTaxa <- Genesys_allcrops[c("fullTaxa")]
TNRS_Genesys_cropdata <- Genesys_allcrops_fullTaxa
head(TNRS_Genesys_cropdata, n = 20)
   ## run TNRS taxon clean 
results <- TNRS(taxonomic_names = TNRS_Genesys_cropdata) #### something is not right here, only Oryza was cleaned
  ## view results
head(results, 10)
View(results)
  ## save results
TNRS_Genesys_results <- results
write_xlsx(TNRS_Genesys_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/TNRS_Genesys_results.xlsx")



## GBIF
GBIF_allcrops_fullTaxa <- GBIF_allcrops[c("fullTaxa")]
TNRS_GBIF_cropdata <- GBIF_allcrops_fullTaxa
head(TNRS_GBIF_cropdata, n = 20)
## run TNRS taxon clean 
results <- TNRS(taxonomic_names = TNRS_GBIF_cropdata)
## view results
head(results, 10)
View(results)
## save results
TNRS_GBIF_results <- results
write_xlsx(TNRS_GBIF_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/TNRS_GBIF_results.xlsx")



## SGSV
SGSV_allcrops_fullTaxa <- SGSV_allcrops[c("fullTaxa")]
TNRS_SGSV_cropdata <- SGSV_allcrops_fullTaxa
head(TNRS_SGSV_cropdata, n = 20)
## run TNRS taxon clean 
results <- TNRS(taxonomic_names = TNRS_SGSV_cropdata)
## view results
head(results, 10)
View(results)
## save results
TNRS_SGSV_results <- results
write_xlsx(TNRS_SGSV_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/TNRS_SGSV_results.xlsx")



#### Add accepted taxon names (TNRS) to datasets
# View TNRS datasets, join to original data sources

library(readr)
TNRS_BGCI_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/TNRS_BGCI_results.xlsx")
TNRS_WIEWS_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/TNRS_WIEWS_results.xlsx")
#### genesys didnt work properly?
TNRS_Genesys_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/TNRS_Genesys_results.xlsx")
TNRS_GBIF_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/TNRS_GBIF_results.xlsx")
TNRS_SGSV_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/TNRS_SGSV_results.xlsx")

#### Add accepted taxon names to datasets

# Rename columns that will be added
TNRS_BGCI_results <- TNRS_BGCI_results %>% rename( fullTaxa = Name_submitted, taxonStatus_TNRS = Taxonomic_status, acceptedName_TNRS = Accepted_name, acceptedNameRank_TNRS = Accepted_name_rank)
# Join relevant columns to data source
BGCI_allcrops <- BGCI_allcrops %>%
  left_join(select(TNRS_BGCI_results, fullTaxa, taxonStatus_TNRS, acceptedNameRank_TNRS, acceptedName_TNRS), by = "fullTaxa", relationship = "many-to-many")

# Rename columns that will be added
TNRS_WIEWS_results <- TNRS_WIEWS_results %>% rename( fullTaxa = Name_submitted, taxonStatus_TNRS = Taxonomic_status, acceptedName_TNRS = Accepted_name, acceptedNameRank_TNRS = Accepted_name_rank)
# Join relevant columns to data source
WIEWS_allcrops <- WIEWS_allcrops_duprmv %>%
  left_join(select(TNRS_WIEWS_results, fullTaxa, taxonStatus_TNRS, acceptedNameRank_TNRS, acceptedName_TNRS), by = "fullTaxa", relationship = "many-to-many")

# Rename columns that will be added
TNRS_Genesys_results <- TNRS_Genesys_results %>% rename( fullTaxa = Name_submitted, taxonStatus_TNRS = Taxonomic_status, acceptedName_TNRS = Accepted_name, acceptedNameRank_TNRS = Accepted_name_rank)
# Join relevant columns to data source
Genesys_allcrops <- Genesys_allcrops %>%
  left_join(select(TNRS_Genesys_results, fullTaxa, taxonStatus_TNRS, acceptedNameRank_TNRS, acceptedName_TNRS), by = "fullTaxa", relationship = "many-to-many")

# Rename columns that will be added
TNRS_GBIF_results <- TNRS_GBIF_results %>% rename( fullTaxa = Name_submitted, taxonStatus_TNRS = Taxonomic_status, acceptedName_TNRS = Accepted_name, acceptedNameRank_TNRS = Accepted_name_rank)
# Join relevant columns to data source
GBIF_allcrops <- GBIF_allcrops %>%
  left_join(select(TNRS_GBIF_results, fullTaxa, taxonStatus_TNRS, acceptedNameRank_TNRS, acceptedName_TNRS), by = "fullTaxa", relationship = "many-to-many")

# Rename columns that will be added
TNRS_SGSV_results <- TNRS_SGSV_results %>% rename( fullTaxa = Name_submitted, taxonStatus_TNRS = Taxonomic_status, acceptedName_TNRS = Accepted_name, acceptedNameRank_TNRS = Accepted_name_rank)
# Join relevant columns to data source
SGSV_allcrops <- SGSV_allcrops %>%
  left_join(select(TNRS_SGSV_results, fullTaxa, taxonStatus_TNRS, acceptedNameRank_TNRS, acceptedName_TNRS), by = "fullTaxa", relationship = "many-to-many")


### clean Taxon MORE
## 








##################################################################################################
############### Compile all data sources into single dataset #####################################

# read in acceNumb as class: "character" so that data sets can merge properly
class(GBIF_allcrops$acceNumb)
GBIF_allcrops$acceNumb <- as.character(GBIF_allcrops$acceNumb) #changed from numeric to charater class
class(GBIF_allcrops$acceNumb) #check 

# combine all data sources
combined_allcrops <-  bind_rows(BGCI_allcrops, Genesys_allcrops, WIEWS_allcrops, GBIF_allcrops, SGSV_allcrops) 

# add a column that is row number, named index
combined_allcrops$index <- 1:nrow(combined_allcrops)


# Summary of data by data source
combined_allcrops_sum <- combined_allcrops %>%
  group_by(, data_source) %>%
  summarise(count = n())

# Summary of counts on taxonomic names
combined_allcrops_sum2 <- combined_allcrops %>%
  group_by(genus, data_source) %>%
  summarise(count = n())

# save file later
write_csv(data, file = "/Summary.csv")


###############################################################################################
#### Data cleaning to do after all data sources combined ######################################

# institution international status field, based on institution list
# taxon fields cleaned by hand (round 3 of taxon cleaning) <<< maybe clean by individual data source, need to do this before combining?
# cropName field based on crop list and taxon names 
# strategy field based on crop list 
# isCrop field, Y/N based on crop list and taxon names
# isCWR, Y/N (based on sampStat?, and based on taxon names)




## Institution international status field

# 2 types of institutions found in data: 
# duplInst_internationalStatus, Y/N
# holdingInst_internationalStatus, Y/N --- holding institution not spelled out in WIEWS,Gensys, except in SGSV

# read in international institutions list
library(readr)
internationalInst <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_internationalgenebankslist.xlsx")

### fill out holdingInst_internationalStatus 
## subset only the relevant column of names list
internationalInst <- subset(internationalInst, select = c(instName, internationalStatus))

# rename instFullName to be duplInstName
internationalInst<- internationalInst %>% rename(holdingInst_internationalStatus = internationalStatus)

#join duplInst_internationalStatus column 
library(dplyr)
combined_allcrops <- combined_allcrops %>%
  left_join(internationalInst, by = "instName") 


### fill out duplInst_internationalStatus 
## subset only the relevant column of names list
internationalInst <- subset(internationalInst, select = c(instName, internationalStatus))

# rename instFullName to be duplInstName
internationalInst<- internationalInst %>% rename(duplInstName = instName, duplInst_internationalStatus = internationalStatus)

#join duplInst_internationalStatus column 
library(dplyr)
combined_allcrops <- combined_allcrops %>%
  left_join(internationalInst, by = "duplInstName") 






## FIELDS STILL REMAINING
# taxon fields cleaned by hand (round 3 of taxon cleaning) <<< maybe clean by individual data source
# then fill out cropName field 
# then fil out strategy field
# isCrop field, Y/N
# isCWR (based on sampStat?, and based on taxon names), Y/N
# what else.. 






