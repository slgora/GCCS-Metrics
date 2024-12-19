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


## 5 Data sources (dont combine SGSV) to combine into one dataset
# (1). BGCI Plant Search
# (2). FAO WIEWS
# (3). GBIF
# (4). Genesys PGR
# (5). SGSV 



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


############### BGCI Plant Search: Data Read in and Cleaning ####################
library(readxl)
BGCI_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/BGCI_allcrops_unformatted.xlsx")
# View(BGCI_allcrops_unformatted)
# colnames(BGCI_allcrops_unformatted)
BGCI_allcrops <- BGCI_allcrops_unformatted

# Rename all columns according to Genesys naming style
colnames(BGCI_allcrops) <- c("source", "fullSciName", "fullTaxa", "accepted", "plantSearchId",
                             "cultivar", #dropped cultivar
                             "exSituSiteGardenSearchId", "instName", "city",
                             "stateProvince", "origCtyFullName", "country2", # 2 letter abrev for origCty
                             "latitude", # of garden, don't use
                             "longitude", # of garden, don't use 
                             "germplasmPlant", "germplasmSeed", 
                             "germplasmPollen", "germplasmExplant",
                             "acceptedNamePlantSearch", "synNamePlantSearch", "addedSubmittedNames")

# Select fields to keep and remove special characters
library(magrittr)
library(dplyr)
BGCI_allcrops <- BGCI_allcrops %>%
  select(fullSciName, fullTaxa, origCtyFullName, country2, germplasmPlant, germplasmSeed, 
         germplasmPollen, germplasmExplant) %>%
  mutate(across(everything(), ~gsub("[[:punct:]]", "", .x)))  # Remove special characters, noticed lots of special characters


# Add field: data source
BGCI_allcrops <- cbind(BGCI_allcrops, data_source = "BGCI")

# Separate fields: fullSciName, still have fullTaxa
# fullSciName into genus and species
library(tidyr)
BGCI_allcrops  <- BGCI_allcrops  %>% separate(fullSciName, c('genus', 'species'))
 
# Combine genus and species to fill out fullTaxa only if it is empty (106 rows)
BGCI_allcrops <- BGCI_allcrops %>% 
  mutate(fullTaxa = if_else(is.na(fullTaxa) | fullTaxa == "", paste(genus, species, sep = " "), fullTaxa))


### Encode all fields relevant to storage fields
## germplasmSeed == 10, 13 (seed, long-term)
## germplasmPlant == 20 (field)
## germplasmPollen == 99 (other)
## germplasmExplant == 30 (in vitro)


# replace '1' in storage column with storage code(s)
# change '0' to NAs (no data)
BGCI_allcrops['germplasmSeed'][BGCI_allcrops['germplasmSeed'] == 1] <- "10; 13"
BGCI_allcrops['germplasmSeed'][BGCI_allcrops['germplasmSeed'] == 0] <- NA

BGCI_allcrops['germplasmPlant'][BGCI_allcrops['germplasmPlant'] == 1] <- 20
BGCI_allcrops['germplasmPlant'][BGCI_allcrops['germplasmPlant'] == 0] <- NA

BGCI_allcrops['germplasmPollen'][BGCI_allcrops['germplasmPollen'] == 1] <- 99
BGCI_allcrops['germplasmPollen'][BGCI_allcrops['germplasmPollen'] == 0] <- NA

BGCI_allcrops['germplasmExplant'][BGCI_allcrops['germplasmExplant'] == 1] <- 30
BGCI_allcrops['germplasmExplant'][BGCI_allcrops['germplasmExplant'] == 0] <- NA

# combine all 4 fields into one storage field 
# keep all data across all fields, separate with ; accoring to Genesys listing standard
BGCI_allcrops <- BGCI_allcrops %>% 
        mutate(storage = pmap_chr(list(germplasmSeed, germplasmPlant, germplasmPollen, germplasmExplant), ~ paste(na.omit(c(...)), collapse = "; "))) %>% 
        select(-germplasmSeed, -germplasmPlant, -germplasmPollen, -germplasmExplant)


## Encode origCty field
library(readr)
geo_names <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Passport/geo_names.csv")

## subset only the relevant column to join:
# 2 letter abbreviation for country and the 3 letter abbreviation
geo_names_2 <- subset(geo_names, select = c(country2, country3))

library(dplyr)
BGCI_allcrops <- BGCI_allcrops %>%
  left_join(geo_names_2, by = "country2") %>%
  rename(origCty=country3) %>%
  select(-country2) #removed the 2 letter country column

# row 18220- replace NAM with a NA/blank, other NAM values are true abbreviations for Nambia
# Replace the "NAM" value in row 18220, column "origCty"
BGCI_allcrops[18220, "origCty"] <- NA


# ADD Cleaned taxon names to BGCI dataset from TNRS results
TNRS_BGCI_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/TNRS_BGCI_results.xlsx")

TNRS_BGCI_results <- TNRS_BGCI_results %>% 
  select(Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_rank) %>% 
  rename(
    fullTaxa = Name_submitted, 
    taxonStatus_TNRS = Taxonomic_status, 
    acceptedName_TNRS = Accepted_name, 
    acceptedNameRank_TNRS = Accepted_name_rank
  ) %>%
  drop_na()

# Ensure fullTaxa is unique
TNRS_BGCI_results_unique <- TNRS_BGCI_results %>%
  group_by(fullTaxa) %>%
  summarise(
    taxonStatus_TNRS = first(taxonStatus_TNRS),
    acceptedName_TNRS = first(acceptedName_TNRS),
    acceptedNameRank_TNRS = first(acceptedNameRank_TNRS)
  )

# Join data frames
BGCI_allcrops <- BGCI_allcrops %>%
  left_join(TNRS_BGCI_results_unique, by = "fullTaxa")

# need to fill out the blanks in acceptedName_TNRS
# if blank then switch over to field
BGCI_allcrops <- BGCI_allcrops %>%
  mutate(acceptedName_TNRS = if_else(is.na(acceptedName_TNRS), fullTaxa, acceptedName_TNRS))

## ADD an acceptedGenus_TNRS column 
#then split the field by acceptedGenus_TNRS 
BGCI_allcrops <- BGCI_allcrops %>% 
  mutate(acceptedGenus_TNRS = sub(" .*", "", acceptedName_TNRS))


View(BGCI_allcrops)





############### WIEWS: Data Read in and Cleaning ####################
library(readr)
WIEWS_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/SDBGExtractRequest/WIEWS_allcrops_unformatted.csv")
# View(WIEWS_allcrops_unformatted)
# colnames(WIEWS_allcrops_unformatted)
WIEWS_allcrops <- WIEWS_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(WIEWS_allcrops) <- c("holdingCty","instCode", "acceNumb", "fullTaxa", "genus", 
                              "species", "acceptedGenus","acceptedSpecies", "cropName", 
                              "acqDate", "origCty", "sampStat", "duplSite", "duplInstName",
                               "latitude", "longitude", "acqSRC_WIEWS", "storage",
                                "mlsStat","doi")

# Fields we want to keep:
WIEWS_allcrops <- subset(WIEWS_allcrops, select = c(holdingCty, instCode, acceNumb, 
                            fullTaxa, genus, species, cropName, origCty, 
                            sampStat, duplSite, latitude,longitude, 
                            storage, mlsStat))

# Add field: data source
WIEWS_allcrops <- cbind(WIEWS_allcrops, data_source = "WIEWS")

## Standardize acceNumb field
## remove blank/space between institute abbreviation and number
#install.packages("stringr")
library(stringr)
library(dplyr)
WIEWS_allcrops  <- WIEWS_allcrops  %>%
  mutate(acceNumb = str_replace_all(acceNumb, " ", ""))


# Make and standardize the instName field
# Make a instName column with the fullname of the institution
# can fill in InstName from WIEWS file of institution codes, based on the instCodes
institute_names <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/WIEWS_institutes_2020.xlsx")

## subset only the relevant column to join:
# 3 letter ISO3 institute code and the full name of the institute
institute_names_full <- subset(institute_names, select = c(INSTCODE, FULL_NAME))
#rename relevant columns 
institute_names_full <- institute_names_full %>% rename( instCode = INSTCODE, instName= FULL_NAME) %>% drop_na()

library(dplyr)
WIEWS_allcrops <- WIEWS_allcrops %>%
  left_join(institute_names_full, by = "instCode")


# ADD Cleaned taxon names to BGCI from TNRS results
TNRS_WIEWS_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/TNRS_WIEWS_results.xlsx")

TNRS_WIEWS_results <- TNRS_WIEWS_results %>% 
  select(Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_rank) %>% 
  rename(
    fullTaxa = Name_submitted, 
    taxonStatus_TNRS = Taxonomic_status, 
    acceptedName_TNRS = Accepted_name, 
    acceptedNameRank_TNRS = Accepted_name_rank
  ) %>%
  drop_na()

# Ensure fullTaxa is unique
TNRS_WIEWS_results_unique <- TNRS_WIEWS_results %>%
  group_by(fullTaxa) %>%
  summarise(
    taxonStatus_TNRS = first(taxonStatus_TNRS),
    acceptedName_TNRS = first(acceptedName_TNRS),
    acceptedNameRank_TNRS = first(acceptedNameRank_TNRS)
  )

# Join data frames
WIEWS_allcrops <- WIEWS_allcrops %>%
  left_join(TNRS_WIEWS_results_unique, by = "fullTaxa")

# need to fill out the blanks in acceptedName_TNRS
# if blank then switch over to field
WIEWS_allcrops <- WIEWS_allcrops %>%
  mutate(acceptedName_TNRS = if_else(is.na(acceptedName_TNRS), fullTaxa, acceptedName_TNRS))

## ADD an acceptedGenus_TNRS column 
#then split the field by acceptedGenus_TNRS 
WIEWS_allcrops <- WIEWS_allcrops %>% 
  mutate(acceptedGenus_TNRS = sub(" .*", "", acceptedName_TNRS))

## Clean country field according to notes from CountryCodes_toClean file
WIEWS_allcrops['origCty'][WIEWS_allcrops['origCty'] == "ANT"] <- "ATG"
WIEWS_allcrops['origCty'][WIEWS_allcrops['origCty'] == "BYS"] <- "BLR"
WIEWS_allcrops['origCty'][WIEWS_allcrops['origCty'] == "SCG"] <- "SER, MNE"
WIEWS_allcrops['origCty'][WIEWS_allcrops['origCty'] == "YUG"] <- "SLO, HRV, BIH, SRB, MNE, MKD, XKX"
WIEWS_allcrops['origCty'][WIEWS_allcrops['origCty'] == "CSK"] <- "CZE, SVK"
WIEWS_allcrops['origCty'][WIEWS_allcrops['origCty'] == "SUN"] <- "RUS"







############### Genesys PGR: Data Read in and Cleaning ####################
library(readr)
Genesys_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")
# View(Genesys_allcrops_unformatted)
# colnames(Genesys_allcrops_unformatted)
Genesys_allcrops <- Genesys_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(Genesys_allcrops) <- c("rowNumb","source","instCode","doi","acceNumb","historic",      
                                 "curation","genus","species","spAuthor","subTaxa","subTAuthor",
                                 "grin_Taxon_Id",  #not useful, Grin code
                                  "acceptedName_TNRS",  #renames GRIN_NAME to acceptedName_TNRS
                                  "grin_Author","cropName_Genesys","cropCode","sampStat", #code
                                   "acqDate","acceName_Genesys","origCty","collSite",
                                   "latitude", #latitude of collection site?
                                   "longitude", #longitude of collection site?
                                   "coordDuncert","coordDatum","geoRefmeth","elevation",
                                   "collDate","collSrc","collNumb","collCode","CollName",
                                   "collInstAddress","collMissId","donorCode_Genesys",
                                   "donorName_Genesys","donorNumb_Genesys","otherNumb_Genesys",
                                   "bredCode", "bredName", "ancest_Genesys","duplSite",
                                   "duplInstName","storage","mlsStat","acceUrl","remarks",
                                  "dataProviderId_Genesys","uuId", "lastModified" )
## fields to keep: 
Genesys_allcrops <- subset(Genesys_allcrops, select = c(instCode, acceNumb, 
                                                    genus, species, acceptedName_TNRS,
                                                    cropName_Genesys, sampStat, acceName_Genesys,
                                                    origCty, latitude,longitude,
                                                    duplSite, storage,
                                                    mlsStat))
# Add field: data source 
Genesys_allcrops <- cbind(Genesys_allcrops, data_source = "Genesys")

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


# Make and standardize the instName field
# Make an instName column with the fullname of the institution
# can fill in InstName from WIEWS file of institution codes, based on the instCodes
institute_names <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/WIEWS_institutes_2020.xlsx")

## subset only the relevant column to join:
# 3 letter ISO3 institute code and the full name of the institute
institute_names_full <- subset(institute_names, select = c(INSTCODE, FULL_NAME))
#rename relevant columns 
institute_names_full <- institute_names_full %>% rename( instCode = INSTCODE, instName= FULL_NAME)

library(dplyr)
Genesys_allcrops <- Genesys_allcrops %>%
  left_join(institute_names_full, by = "instCode")

# used GRIN names as cleaned names (TNRS wouldnt run on Genesys)
# need to fill out the blanks in acceptedName_TNRS
# if blank then switch over to field
Genesys_allcrops <- Genesys_allcrops %>%
  mutate(acceptedName_TNRS = if_else(is.na(acceptedName_TNRS), fullTaxa, acceptedName_TNRS))

## ADD an acceptedGenus_TNRS column 
#then split the field by acceptedGenus_TNRS 
Genesys_allcrops <- Genesys_allcrops %>% 
  mutate(acceptedGenus_TNRS = sub(" .*", "", acceptedName_TNRS))


## Clean country field according to notes from CountryCodes_toClean file
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "XAE"] <- "CYP"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "ANT"] <- "ATG"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "XAM"] <- "PHL"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "ZAR"] <- "COD"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "ROM"] <- "ROU"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "BYS"] <- "BLR"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "SCG"] <- "SER, MNE"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "YUG"] <- "SLO, HRV, BIH, SRB, MNE, MKD, XKX"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "CSK"] <- "CZE, SVK"
Genesys_allcrops['origCty'][Genesys_allcrops['origCty'] == "SUN"] <- "RUS"


View(Genesys_allcrops)





############### GBIF: Data Read in and Cleaning ####################

# Read in as a csv, not excel, helped eliminate data loss
library(readr)
GBIF_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/GBIF_allcrops_unformatted.csv")

# colnames(GBIF_allcrops_unformatted)
GBIF_allcrops <- GBIF_allcrops_unformatted

#rename all columns according to Genesys naming style:
colnames(GBIF_allcrops) <- c("acceNumb","sharing","bibliographicCitation","language",
                             "license","modified","publisher","references","rightsHolder",
                             "type","institutionID","collectionID","doi","instAcronym",
                             "acqSRC_GBIF","datasetName","ownerInstCode", #not helpful mostly empty also
                             "basisOfRecord","informationWithheld","dataGeneralizations",
                             "dynamicProperties","occurrenceID_GBIF","catalogNumber_GBIF",
                             "acceNumb","recordedBy","recordedByID","individualCount",
                             "organismQuantity","organismQuantityType","sex","lifeStage",
                             "reproductiveCondition","caste","behavior","vitality",
                             "establishmentMeans","degreeOfEstablishment","pathway",
                             "georeferenceVerificationStatus","occurrenceStatus",
                             "storage","disposition","associatedOccurrences",
                             "associatedReferences","associatedSequences","associatedTaxa",
                             "otherCatalogNumbers_GBIF","occurrenceRemarks","organismID",
                             "organismName","organismScope","associatedOrganisms",
                             "previousIdentifications","organismRemarks","materialEntityID",
                             "materialEntityRemarks","verbatimLabel","materialSampleID",
                             "eventID","parentEventID","eventType","fieldNumber","eventDate",
                             "eventTime","startDayOfYear","endDayOfYear","year","month",
                             "day","verbatimEventDate","habitat","samplingProtocol",
                             "sampleSizeValue","sampleSizeUnit","samplingEffort","fieldNotes",
                             "eventRemarks","locationID","higherGeographyID","higherGeography",
                             "continent","waterBody","islandGroup","island","country2", 
                             "stateProvince","county","municipality","locality","verbatimLocality",
                             "verbatimElevation","verticalDatum","verbatimDepth",
                             "minimumDistanceAboveSurfaceInMeteters","maximumDistanceAboveSurfaceInMeters",
                             "locationAccordingTo","locationRemarks","latitude","longitude",
                             "coordinateUncertaintyInMeters", "coordinatePrecision",
                             "pointRadiusSpatialFit","verbatimCoordinateSystem","verbatimSRS",
                             "footprintWKT","footprintSRS","footprintSpatialFit",
                             "georeferencedBy","georeferencedDate","georeferenceProtocol",
                             "georeferenceSources","georeferenceRemarks","geologicalContextID",
                             "earliestEonOrLowestEonothem","latestEonOrHighestEonothem",
                             "earliestEraOrLowestErathem","latestEraOrHighestErathem",
                             "earliestPeriodOrLowestSystem","latestPeriodOrHighestSystem",
                             "earliestEpochOrLowestSeries","latestEpochOrHighestSeries",
                             "earliestAgeOrLowestStage","latestAgeOrHighestStage",
                             "lowestBiostratigraphicZone","highestBiostratigraphicZone",
                             "lithostratigraphicTerms","group","formation","member","bed",
                             "identificationID", "verbatimIdentification","identificationQualifier",
                             "typeStatus","identifiedBy","identifiedByID","dateIdentified",
                             "identificationReferences","identificationVerificationStatus",
                             "identificationRemarks","taxonID","scientificNameID","acceptedNameUsageID",
                             "parentNameUsageID","originalNameUsageID","nameAccordingToID",
                             "namePublishedInID","taxonConceptID","fullTaxa","acceptedNameUsage",
                             "parentNameUsage", "originalNameUsage", "nameAccordingTo",
                             "namePublishedIn","namePublishedInYear","higherClassification",
                             "kingdom","phylum","class","order","superfamily","family",
                             "subfamily","tribe","subtribe","genusSynonym","genus","subgenus",
                             "infragenericEpithet","species","infraspecificEpithet",
                             "cultivarEpithet","taxonRank","verbatimTaxonRank","vernacularName",
                             "nomenclaturalCode","taxonomicStatus","nomenclaturalStatus",
                             "taxonRemarks","datasetKey","publishingCountry","lastInterpreted",
                             "elevation","elevationAccuracy","depth","depthAccuracy",
                             "distanceFromCentroidInMeters","issue","mediaType","hasCoordinate",
                             "hasGeospatialIssues","taxonKey","acceptedTaxonKey","kingdomKey",
                             "phylumKey","classKey","orderKey","familyKey","genusKey","subgenusKey",
                             "speciesKey","species2","acceptedScientificName","verbatimScientificName",
                             "typifiedName","protocol","lastParsed","lastCrawled","repatriated",
                             "relativeOrganismQuantity","projectId","isSequenced","gbifRegion",
                             "publishedByGbifRegion","origCty","level0Name","level1Gid","level1Name",
                             "level2Gid","level2Name","level3Gid","level3Name","iucnRedListCategory"    
                             )

## fields to keep: 
GBIF_allcrops <- subset(GBIF_allcrops, select = c(acceNumb, instAcronym,storage, 
                                                  country2, latitude, longitude, 
                                                  fullTaxa, genusSynonym, genus, 
                                                  species, origCty ))

# Add field: data source
GBIF_allcrops <- cbind(GBIF_allcrops, data_source = "GBIF")


## Encode abbreviation field to 3 letter country and then add to origCty column
## Replace the GBIF country abbreviation with the ISO3 name 
## Already a column with 3 letter country of origin but can extract MORE data from 
# other accessions that only filled out the 2 letter county code field 

# read in WIEWS geo names guide file
geo_names <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Passport/geo_names.csv")

## subset only the relevant column to join- 2 letter country code and the 3 letter abbreviation
geo_names_2 <- subset(geo_names, select = c(country2, country3))

library(dplyr)
GBIF_allcrops <- GBIF_allcrops %>%
                  left_join(geo_names_2, by = "country2") %>%
                  select(-country2) #removed the 2 letter country column


# replace all NAM with blanks/Nas 
# initially picked up and read NA as a 2 letter country code for Nambia
# did check and Nambia is NOT present as a country in this dataset
# didnt work to remove NAs initially
GBIF_allcrops['country3'][GBIF_allcrops['country3'] == "NAM" ] <- NA


## join origCty (already formatted to be 3 letter country code and country3 column just created)
## if there is an NA, then move over country data to origCty
GBIF_allcrops <- GBIF_allcrops %>%
                    mutate(origCty = if_else(is.na(origCty), country3, origCty),
                    country3 = if_else(is.na(country3), origCty, country3)) 


# Country clean, Part I:
# fix the countries that were labled "Z01", "Z06" and "Z07" and drop the extra country field
# cleaned field by replacing with correct country code (variable) from country3 field
# only for those rows with countries that were labled as Z01, Z06, Z07
GBIF_allcrops <- GBIF_allcrops %>% 
                    mutate( origCty = if_else(origCty %in% c("Z01", "Z06", "Z07"), country3, origCty) )%>%
                    select(-country3)

# Country clean, Part II
## Clean other errors in country field according to notes from CountryCodes_toClean file
GBIF_allcrops['origCty'][GBIF_allcrops['origCty'] == "ZNC"] <- "CYP"
GBIF_allcrops['origCty'][GBIF_allcrops['origCty'] == "XAD"] <- "CYP"
GBIF_allcrops['origCty'][GBIF_allcrops['origCty'] == "XAC"] <- "IRN"
GBIF_allcrops['origCty'][GBIF_allcrops['origCty'] == "BYS"] <- "BLR"



## Encode all unique notes in storage field with WIEWS codes
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



# Make and standardize the instName field
# Make a instName column with the fullname of the institution
# can fill in InstName from WIEWS file of institution codes, based on the instCodes
library(readr)
library(readxl)
library(tidyr)
institute_names <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/WIEWS_institutes_2020.xlsx")

## subset only the relevant column to join:
# 3 letter ISO3 institute code and the full name of the institute
institute_names_acron <- subset(institute_names, select = c(ACRONYM, FULL_NAME, INSTCODE))
#rename relevant columns 
institute_names_acron <- institute_names_acron %>% 
                            rename( instAcronym = ACRONYM, instName= FULL_NAME, instCode = INSTCODE) %>%
                            drop_na()


library(dplyr)
GBIF_allcrops <- GBIF_allcrops %>%
                  left_join(institute_names_acron, by = "instAcronym", relationship= "many-to-many")


# ADD Cleaned taxon names to BGCI from TNRS results
TNRS_GBIF_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/TNRS_GBIF_results.xlsx")

TNRS_GBIF_results <- TNRS_GBIF_results %>% 
            select(Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_rank) %>% 
            rename( fullTaxa = Name_submitted, 
                    taxonStatus_TNRS = Taxonomic_status, 
                    acceptedName_TNRS = Accepted_name, 
                    acceptedNameRank_TNRS = Accepted_name_rank) %>%
            drop_na()

# Ensure fullTaxa is unique
TNRS_GBIF_results_unique <- TNRS_GBIF_results %>%
                            group_by(fullTaxa) %>%
                            summarise(taxonStatus_TNRS = first(taxonStatus_TNRS),
                                      acceptedName_TNRS = first(acceptedName_TNRS),
                                      acceptedNameRank_TNRS = first(acceptedNameRank_TNRS))

# Join data frames
GBIF_allcrops <- GBIF_allcrops %>%
                  left_join(TNRS_GBIF_results_unique, by = "fullTaxa")

# need to fill out the blanks in acceptedName_TNRS
# if blank then switch over to field
GBIF_allcrops <- GBIF_allcrops %>%
                 mutate(acceptedName_TNRS = if_else(is.na(acceptedName_TNRS), fullTaxa, acceptedName_TNRS))

## ADD an acceptedGenus_TNRS column 
#then split the field by acceptedGenus_TNRS 
GBIF_allcrops <- GBIF_allcrops %>% 
                 mutate(acceptedGenus_TNRS = sub(" .*", "", acceptedName_TNRS))


View(GBIF_allcrops)




                                
############### SGSV ####################
library(readxl)
SGSV_allcrops_unformatted <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/SGSV_allcrops_unformatted.xlsx")
SGSV_allcrops <- SGSV_allcrops_unformatted
# colnames(SGSV_allcrops_unformatted)

# CHANGE ALL NAMES TO GENESYS STYLE
colnames(SGSV_allcrops) <- c( "source","instName","instCode","instAcronym","acceNumb",
                              "fullTaxa","fullTaxa1","ctyFullName") #origCtyFullName

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


## Standardize acceNumb field
## remove blank/space between institute abbreviation and number
library(stringr)
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


# ADD Cleaned taxon names to from TNRS results
TNRS_SGSV_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/TNRS_SGSV_results.xlsx")

TNRS_SGSV_results <- TNRS_SGSV_results %>% 
                       select(Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_rank) %>% 
                       rename(fullTaxa = Name_submitted, 
                               taxonStatus_TNRS = Taxonomic_status, 
                               acceptedName_TNRS = Accepted_name, 
                               acceptedNameRank_TNRS = Accepted_name_rank) %>%
                        drop_na()

# Ensure fullTaxa is unique
TNRS_SGSV_results_unique <- TNRS_SGSV_results %>%
                              group_by(fullTaxa) %>%
                              summarise(taxonStatus_TNRS = first(taxonStatus_TNRS),
                                        acceptedName_TNRS = first(acceptedName_TNRS),
                                        acceptedNameRank_TNRS = first(acceptedNameRank_TNRS))

# Join data frames
SGSV_allcrops <- SGSV_allcrops %>%
                  left_join(TNRS_SGSV_results_unique, by = "fullTaxa")

# need to fill out the blanks in acceptedName_TNRS
# if blank then switch over to field
SGSV_allcrops <- SGSV_allcrops %>%
                  mutate(acceptedName_TNRS = if_else(is.na(acceptedName_TNRS), fullTaxa, acceptedName_TNRS))

## ADD an acceptedGenus_TNRS column 
#then split the field by acceptedGenus_TNRS 
SGSV_allcrops <- SGSV_allcrops %>% 
                  mutate(acceptedGenus_TNRS = sub(" .*", "", acceptedName_TNRS))


## Fill out isCrop field
## based on TNRS clean taxa field and Crop List 
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

# based on "acceptedName_TNRS" field 
# isCrop = Y if names match
# isCrop = N if does not match
isCrop_vector <- ifelse(SGSV_allcrops$acceptedName_TNRS %in% croplist$Taxa_main, "Y", "N")

# add vector to dataframe
SGSV_allcrops <- SGSV_allcrops %>% mutate(isCrop = isCrop_vector)

### Fill out isCWR field (Crop Wild Relative field)
# Add column assgning all "no"s in isCrop as "yes"s in isCWR
SGSV_allcrops <- SGSV_allcrops %>% mutate(isCWR = if_else(isCrop == "N", "Y", "N"))






##### CK and SG decided to re-assign isCrop and isCWR in an alterntive way
### found issues when assigning isInprimaryRegions based on sampStat

## Re-assign CWRs: 
#  Step 1: re-assign isCrop
# assign isCrop=Y, for sampSat= 100s
# assign isCrop= N, for sampStat=  400s, 500, 600
# What abt weedy?= 200? <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


#  Then assign all rows with NAs/not filled in, 
# assign isCrop=Y based on fullTaxa from croplist
# assign isCrop=Y for special cases (entire genus isCrop =Y)
# assign isCrop=N for the remaining NAs 

# Then assign isCWR  based on isCrop
# isCWR=Y if isCrop=N
# isCWR=N if isCrop=Y














# Fill out strategy field
# by cleaned genus 
# join Genera_primary in croplist with Genus_cleaned in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column to join:
croplist_strategy <- subset(croplist, select = c(Genera_primary, CropStrategy))

#rename relevant columns 
croplist_strategy <- croplist_strategy %>% 
  rename( acceptedGenus_TNRS = Genera_primary, cropStrategy = CropStrategy) %>%
  drop_na()

library(dplyr)
SGSV_allcrops <- SGSV_allcrops %>%
  left_join(croplist_strategy, by = "acceptedGenus_TNRS", relationship= "many-to-many")


######## re-run with original genus to fill out the rest of the crop strategies

# Fill out rest of strategy field
# by original genus 
# join Genera_primary in croplist with genus in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column to join:
croplist_strategy <- subset(croplist, select = c(Genera_primary, CropStrategy))

#rename relevant columns 
croplist_strategy <- croplist_strategy %>% 
  rename( genus = Genera_primary, cropStrategy = CropStrategy) %>%
  drop_na()

library(dplyr)
SGSV_allcrops <- SGSV_allcrops %>%
  left_join(croplist_strategy, by = "genus", relationship= "many-to-many")

# need to fill out the blanks in cropStrategy field
# if blank then switch over to field
SGSV_allcrops <- SGSV_allcrops %>%
  mutate(cropStrategy.x = if_else(is.na(cropStrategy.x), cropStrategy.y, cropStrategy.x))

## drop extra column of cropStrategy
SGSV_allcrops <- SGSV_allcrops %>% select(-cropStrategy.y)

# rename the cropStrategy.x column to just cropStrategy
SGSV_allcrops <- SGSV_allcrops %>% 
  rename(cropStrategy = cropStrategy.x)

# Check if any rows didnt get filled out with crop strategy
na_count <- SGSV_allcrops %>% summarise(na_count = sum(is.na(cropStrategy)))
# There are no rows with NA = cropStrategy 
na_rows <- SGSV_allcrops %>% filter(is.na(cropStrategy))
view(na_rows)


# drop row if SGSV dataset doesnt have our crop
# dont need this, there are no NAs after above code
# SGSV_allcrops <- SGSV_allcrops %>% filter(!is.na(cropStrategy))







####################################################################################################
## Remove duplicates in Genesys and WIEWS, WIEWS ##################################################

# keep Genesys records
# filter by acceNumb AND instCode
library(dplyr)

# Check for duplication/common records between WIEWS and Genesys
common_rows <- inner_join(WIEWS_allcrops, Genesys_allcrops, by = c("acceNumb", "instCode"))
print(common_rows)
duplications_removedfromWIEWS <- common_rows
# over 1M rows in common with same acceNumb and instCode

## Remove duplicated in WIEWS
# if acceNumb and instCode are the same in Genesys and WIEWS, then remove row/accession record in WIEWS
WIEWS_allcrops_duprmv <- WIEWS_allcrops %>%
  anti_join(Genesys_allcrops, by = c("acceNumb", "instCode"))
# over 1M rows of common data removed

library(write.csv)
write_csv(WIEWS_allcrops_duprmv, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_allcrops_duprmv.csv")


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





##################################################################################################
############### Combine all data sources into single dataset #####################################

# read in acceNumb as class: "character" so that data sets can merge properly
class(GBIF_allcrops$acceNumb)
GBIF_allcrops$acceNumb <- as.character(GBIF_allcrops$acceNumb) #changed from numeric to character class
class(GBIF_allcrops$acceNumb) #check 

# combine all data sources
combined_allcrops <-  bind_rows(BGCI_allcrops, Genesys_allcrops, WIEWS_allcrops_duprmv, GBIF_allcrops) 
View(combined_allcrops)

# Summary of data by data source
combined_allcrops_sum <- combined_allcrops %>%
  group_by(, data_source) %>%
  summarise(count = n())

# Summary of counts on taxonomic names
combined_allcrops_sum2 <- combined_allcrops %>%
  group_by(genus, data_source) %>%
  summarise(count = n())

# save file later
library(write.csv)
write_csv(combined_allcrops, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")





###############################################################################################
#### Data cleaning to do after all data sources combined ######################################

# combined data
library(readr)
combined_allcrops <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")
View(combined_allcrops)




## Clean taxon names or genus names for 32 rows to keep from the empty_cropStrategies file
# special cases, 4 errors to fix 
empty_cropstrategies <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/empty_cropstrategies_ck.xlsx")
rows_to_keep <- empty_cropstrategies %>% filter(action == "keep")


## Fix spelling errors: 
# replace "Pennisteum" with "Pennisetum" in all cases (1 row)
# replace "Indgofera" with "Indigofera" in all cases (6 rows)
combined_allcrops <- combined_allcrops %>% 
  mutate(across(everything(), ~ gsub("Pennisteum", "Pennisetum", .))) %>% 
  mutate(across(everything(), ~ gsub("Indgofera", "Indigofera", .)))


# overwrite names in row with Alocasia portora (filled out in the wrong fields and odd space removed) fill out in the correct fields
combined_allcrops <- combined_allcrops %>% 
  mutate( fullTaxa = if_else(acceptedName_TNRS == " Alocasia portora", "Alocasia portora", fullTaxa), 
          genus = if_else(acceptedName_TNRS == " Alocasia portora", "Alocasia", genus), 
          species = if_else(acceptedName_TNRS == " Alocasia portora", "portora", species), 
          cropName = if_else(acceptedName_TNRS == " Alocasia portora", "Aroids", cropName),
          acceptedName_TNRS = if_else(acceptedName_TNRS == " Alocasia portora", "Alocasia portora", acceptedName_TNRS))


# then fill out cropStrategy field
# by cleaned genus 
# join Genera_primary in croplist with Genus_cleaned in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column to join:
croplist_strategy <- subset(croplist, select = c(Genera_primary, CropStrategy))

#rename relevant columns 
croplist_strategy <- croplist_strategy %>% 
  rename( acceptedGenus_TNRS = Genera_primary, cropStrategy = CropStrategy) %>%
  drop_na()

library(dplyr)
combined_allcrops <- combined_allcrops %>%
  left_join(croplist_strategy, by = "acceptedGenus_TNRS", relationship= "many-to-many")



######## run with original genus to fill out the rest of the crop strategies

# then fill out strategy field
# by original genus 
# join Genera_primary in croplist with genus in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column to join:
croplist_strategy <- subset(croplist, select = c(Genera_primary, CropStrategy))

#rename relevant columns 
croplist_strategy <- croplist_strategy %>% 
  rename( genus = Genera_primary, cropStrategy = CropStrategy) %>%
  drop_na()

library(dplyr)
combined_allcrops <- combined_allcrops %>%
  left_join(croplist_strategy, by = "genus", relationship= "many-to-many")

# need to fill out the blanks in cropStrategy field
# if blank then switch over to field
combined_allcrops <- combined_allcrops %>%
  mutate(cropStrategy.x = if_else(is.na(cropStrategy.x), cropStrategy.y, cropStrategy.x))

## drop extra column of cropStrategy
combined_allcrops <- combined_allcrops %>% select(-cropStrategy.y)

# rename the cropStrategy.x column to just cropStrategy
combined_allcrops <- combined_allcrops %>% 
  rename(cropStrategy = cropStrategy.x)


# view how many rows didnt get filled out with crop strategy
na_count <- combined_allcrops %>% summarise(na_count = sum(is.na(cropStrategy)))
# View the rows with NA values
na_rows <- combined_allcrops %>% filter(is.na(cropStrategy))
view(na_rows)
# saved these NA rows as empty crop strategies and asked CK to review


# fix the issue of not all crops are filled out
# some have crop strategy = Na
# CK reviewed and suggested to keep 32 rows (and drop the remaining)
empty_cropstrategies <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/empty_cropstrategies_ck.xlsx")

# Make sure columns match and are in same format
empty_cropstrategies <- empty_cropstrategies %>% 
  mutate( latitude = as.character(latitude), longitude = as.character(longitude), 
          sampStat = as.character(sampStat) ) 
combined_allcrops <- combined_allcrops %>% 
  mutate( latitude = as.character(latitude), longitude = as.character(longitude), 
          sampStat = as.character(sampStat) ) 

# Identify common columns excluding "action" and "corrected" 
common_columns <- intersect(names(combined_allcrops), names(empty_cropstrategies)) 
common_columns <- common_columns[!common_columns %in% c("action", "corrected")] 

# Filter rows in empty_cropstrategies where action is "delete" 
rows_to_delete <- empty_cropstrategies %>% 
  filter(action == "delete") %>% 
  select(all_of(common_columns)) %>% 
  distinct() 

# Make sure no type mismatches by converting all columns in rows_to_delete to character 
rows_to_delete <- rows_to_delete %>% mutate(across(everything(), as.character)) 
combined_allcrops <- combined_allcrops %>% mutate(across(everything(), as.character)) 

# Remove these rows from combined_allcrops 
combined_allcrops <- combined_allcrops %>% anti_join(rows_to_delete, by = common_columns)


# Fix cropStrategy field blank error with note from CK in empty_cropStrategies file
# if "Faba vulgaris is in fullTaxa, then fill out cropStrategy field with Faba bean
combined_allcrops <- combined_allcrops %>% 
  mutate(cropStrategy = if_else(str_detect(fullTaxa, "Faba vulgaris"), "Faba bean", cropStrategy))


## drop all remaining rows with cropStrategy = Na
## ALL == Pisum sativum (already identified to drop by CK)
## suspecting these were added in via later cleaning stages, post CK review of empty_cropStrategies file
combined_allcrops <- combined_allcrops %>% drop_na(cropStrategy)


# Check all rows have cropStrategy filled out (no Nas)
na_count <- combined_allcrops %>% summarise(na_count = sum(is.na(cropStrategy)))
# View the rows with NA values
na_rows <- combined_allcrops %>% filter(is.na(cropStrategy))
view(na_rows)

# all special cases were also correctly labeled with crop strategy 
# ALL genera for Cajanus, Cenchrus, Eragrostis, Glycine, Pennisetum, Vigna were labeled with the correct strategy




### Fill out internationalStatus column 
## subset only the relevant column of names list
library(readxl)
internationalInst <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_internationalgenebanks_list.xlsx")
internationalInst <- subset(internationalInst, 
                            select = c(instCode, internationalStatus)) %>% 
                            distinct()
View(internationalInst)

#join internationalStatus column by institution code
library(dplyr)
combined_allcrops <- combined_allcrops %>% 
  left_join(internationalInst, combined_allcrops, by = c("instCode"))

# All cells are filled in if Y= international
# fill out N= not international
# Replace all NA values in the internationalStatus column with "N" 
combined_allcrops <- combined_allcrops %>% 
  mutate(internationalStatus = ifelse(is.na(internationalStatus), "N", internationalStatus))
View(combined_allcrops)




##### CK and SG decided to re-assign isCrop and isCWR in an alternative way 2024_12_12
### found issues when assigning isInprimaryRegions based on sampStat (later on)

## Re-assign isCrop and isCWR, first based on sampStat: 

# What abt weedy?= 200? <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Assign isCrop based on sampStat values 
# assign isCrop=Y, for sampSat= 100s
# assign isCrop= N, for sampStat=  400s, 500, 600
# keep all other rows as NA
combined_allcrops <- combined_allcrops %>% 
  mutate(isCrop = case_when( sampStat %in% 100:199 ~ "Y", sampStat %in% c(400:499, 500, 600) ~ "N", 
                             TRUE ~ NA_character_ ))


#  Then assign all rows with NAs/not filled in, based on croplist (fullTaxa and genus)
# assign isCrop=Y based on fullTaxa from croplist
# assign isCrop=Y for special cases (entire genus isCrop =Y)
# assign isCrop=N for the remaining NAs 
# keep all data if already filled in

### Fill out other rows that are NA for the isCrop field based on croplist
## based on TNRS clean taxa field and Crop List 
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

# based on "acceptedName_TNRS" field 
# isCrop = Y if names match
# isCrop = N if does not match
isCrop_vector <- ifelse(combined_allcrops$acceptedName_TNRS %in% croplist$Taxa_main, "Y", "N")

# fill in isCrop field ONLY where it is NA, dont overwrite existing data
combined_allcrops <- combined_allcrops %>% 
  mutate(isCrop = ifelse(is.na(isCrop), isCrop_vector, isCrop))


# special cases have all genera that are the crop 
specialcases <- c("Cajanus", "Cenchrus", "Eragrostis", "Glycine", "Pennisetum", "Vigna") 

# Create regex pattern 
pattern <- paste(specialcases, collapse = "|") 

# Update isCrop = "Y" if genus or acceptedName_TNRS contains any of the specified special cases 
# dont overwrite isCrop data if sampStat = 400s, 500, 600
combined_allcrops <- combined_allcrops %>% 
  mutate(isCrop = ifelse((!sampStat %in% c(400:499, 500, 600)) & 
                           (str_detect(genus, regex(pattern, ignore_case = TRUE)) | 
                              str_detect(acceptedName_TNRS, regex(pattern, ignore_case = TRUE))), "Y", isCrop ))

 
# Check all rows have isCrop filled out (no Nas)
na_count <- combined_allcrops %>% summarise(na_count = sum(is.na(isCrop)))
na_count
# View the rows with NA values, should be 0 rows
na_rows <- combined_allcrops %>% filter(is.na(isCrop))
view(na_rows)




### Fill out isCWR field (Crop Wild Relative field)
# Add column assigning all "no"s in isCrop as "yes"s in isCWR, and vice versa
combined_allcrops <- combined_allcrops %>% 
     mutate(isCWR = if_else(isCrop == "N", "Y", "N"))

View(combined_allcrops)





# add Annex I field
# Use annex1 guidefile combined with taxon field

# join annex1 (and annex1inclusions) field in croplist with cleaned taxon field in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column(s) to join:
croplist_annex1 <- subset(croplist, select = c(Genera_primary, Annex1)) %>% 
  rename(annex1 = Annex1, acceptedGenus_TNRS = Genera_primary)
View(croplist_annex1)

# join 2 datsets by genus, and fill out annex1 field if isCrop=Y
# isCrop = N, then annex1 field = NA
# Join the datasets by acceptedGenus_TNRS 
combined_allcrops <- combined_allcrops %>% 
  left_join(croplist_annex1, by = "acceptedGenus_TNRS")


## fill out the NAs in annex1 field based on genus field 
# run by genus if there are NAs 
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")
croplist_annex1 <- subset(croplist, select = c(Genera_primary, Annex1)) %>% 
  rename(annex1 = Annex1, genus = Genera_primary)
combined_allcrops <- combined_allcrops %>% left_join(croplist_annex1, by = "genus")


# Fill NAs in annex1.x with annex1.y and vice versa 
combined_allcrops <- combined_allcrops %>% 
  mutate(annex1.x = ifelse(is.na(annex1.x), annex1.y, annex1.x), 
         annex1.y = ifelse(is.na(annex1.y), annex1.x, annex1.y)) %>%
         rename(annex1 = annex1.x) %>% 
         select(-annex1.y)

# special case: "Faba vulgaris" not on the croplist? 
# Faba bean is annex1 = Y
# Update annex1 to "Y" if fullTaxa or acceptedName_TNRS contains "Faba vulgaris" 
combined_allcrops <- combined_allcrops %>% 
  mutate(annex1 = if_else( str_detect(fullTaxa, regex("Faba vulgaris", ignore_case = TRUE)) | 
                             str_detect(acceptedName_TNRS, regex("Faba vulgaris", ignore_case = TRUE)), 
                             "Y", annex1 ))


## add special cases for annex1 field

# if acceptedGenus_TNRS = Manihot, then annex1= N 
#     except when acceptedName = Manihot esculenta, then annex1=Y

# if acceptedGenus_TNRS = Andropogon, then annex1= N 
#     except when acceptedName = Andropogon gayanus, then annex1=Y

# if acceptedGenus_TNRS = Astragalus, then annex1= N
#     except when acceptedName_TNRS = Astragalus chinensis, Astragalus cicer, or Astragalus arenarius then annex1=Y

# if acceptedGenus= Zea, then annex1=Y 
#     except when acceptedName_TNRS = Zea perennis, Zea diploperennis, or Zea luxurians, then annex1-N

# if acceptedGenus= Atriplex, then annex1=N 
#     except when acceptedName_TNRS = Atriplex halimus, Atriplex nummularia then annex1-Y

# if acceptedGenus= Lotus, then annex1=N 
#     except when acceptedName_TNRS =  Lotus corniculatus, Lotus subbiflorus, Lotus uliginosus then annex1-Y

# if acceptedGenus= Medicago, then annex1=N 
#     except when acceptedName_TNRS = Medicago sativa, Medicago arborea, Medicago falcata, Medicago scutellata, Medicago rigidula, Medicago truncatula, Medicago sativa subsp. sativa, Medicago sativa ssp. sativa, Medicago sativa var. sativa then annex1=Y

# if acceptedGenus= Melilotus, then annex1=N 
#     except when acceptedName_TNRS = Melilotus albus, Melilotus officinalis then annex1=Y

# if acceptedGenus= Trifolium, then annex1=N 
#    except when acceptedName_TNRS = Trifolium repens, Trifolium pratense, Trifolium alexandrinum, Trifolium alpestre, 
#          Trifolium ambiguum, Trifolium angustifolium, Trifolium arvense, 
#          Trifolium agrocicerum, Trifolium hybridum, Trifolium incarnatum, 
#          Trifolium pratense, Trifolium resupinatum, Trifolium rueppellianum, 
#          Trifolium semipilosum, Trifolium subterraneum, Trifolium vesiculosum then annex1=Y



### had issue with original code to assign special cases, not re-running sometimes
## updated code below minimizes redundant operations and reduces lag in running

combined_allcrops <- combined_allcrops %>%
  mutate(annex1 = case_when(
    acceptedGenus_TNRS == "Manihot" & acceptedName_TNRS == "Manihot esculenta" ~ "Y",
    acceptedGenus_TNRS == "Manihot" & acceptedName_TNRS != "Manihot esculenta" ~ "N",
    
    acceptedGenus_TNRS == "Astragalus" & acceptedName_TNRS %in% c("Astragalus chinensis", "Astragalus cicer", "Astragalus arenarius") ~ "Y",
    acceptedGenus_TNRS == "Astragalus" ~ "N",
    
    acceptedGenus_TNRS == "Zea" & acceptedName_TNRS %in% c("Zea perennis", "Zea diploperennis", "Zea luxurians") ~ "N",
    acceptedGenus_TNRS == "Zea" ~ "Y",
    
    acceptedGenus_TNRS == "Atriplex" & acceptedName_TNRS %in% c("Atriplex halimus", "Atriplex nummularia") ~ "Y",
    acceptedGenus_TNRS == "Atriplex" ~ "N",
    
    acceptedGenus_TNRS == "Lotus" & acceptedName_TNRS %in% c("Lotus corniculatus", "Lotus subbiflorus", "Lotus uliginosus") ~ "Y",
    acceptedGenus_TNRS == "Lotus" ~ "N",
    
    acceptedGenus_TNRS == "Medicago" & acceptedName_TNRS %in% c("Medicago sativa", "Medicago arborea", "Medicago falcata", "Medicago scutellata", "Medicago rigidula", "Medicago truncatula", 
                                                                "Medicago sativa subsp. sativa", "Medicago sativa ssp. sativa", 
                                                                "Medicago sativa var. sativa") ~ "Y",
    acceptedGenus_TNRS == "Medicago" ~ "N",
    
    acceptedGenus_TNRS == "Melilotus" & acceptedName_TNRS %in% c("Melilotus albus", "Melilotus officinalis") ~ "Y",
    acceptedGenus_TNRS == "Melilotus" ~ "N",
    
    acceptedGenus_TNRS == "Trifolium" & acceptedName_TNRS %in% c("Trifolium repens", "Trifolium pratense", "Trifolium alexandrinum", "Trifolium alpestre", "Trifolium ambiguum", 
                                                                 "Trifolium angustifolium", "Trifolium arvense", "Trifolium agrocicerum", "Trifolium hybridum", "Trifolium incarnatum", 
                                                                 "Trifolium pratense", "Trifolium resupinatum", "Trifolium rueppellianum", "Trifolium semipilosum", "Trifolium subterraneum", 
                                                                 "Trifolium vesiculosum") ~ "Y",
    acceptedGenus_TNRS == "Trifolium" ~ "N",
    
    TRUE ~ annex1 # Keep the existing value for all other cases
  ))

View(combined_allcrops)


# check to see if some of the special cases were assigned properly
library(dplyr) 
library(stringr) 
# Filter rows that contain "Astragalus chinensis" 
astragalus_chinensis_rows <- combined_allcrops %>% 
  filter_all(any_vars(str_detect(., regex("Astragalus chinensis", ignore_case = TRUE))))
manihot_rows <- combined_allcrops %>% 
  filter_all(any_vars(str_detect(., regex("Manihot", ignore_case = TRUE))))
View(manihot_rows)


# Check all rows have annex1 filled out (no Nas)
na_count <- combined_allcrops %>% summarise(na_count = sum(is.na(annex1)))
na_count
# View the rows with NA values, should be 0 rows
na_rows <- combined_allcrops %>% filter(is.na(annex1))
view(na_rows)



### Add primaryRegions field and isInPrimaryRegions field

### Read in the plants that feed the world regions guide file
library(readxl)
PlantsThatFeedTheWorld_regions <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/CropRegions_PlantsThatFeedtheWorld/PlantsThatFeedTheWorld_Regions.xlsx")
View(PlantsThatFeedTheWorld_regions)

# rename column 
PlantsThatFeedTheWorld_regions <- PlantsThatFeedTheWorld_regions %>% 
  rename("RegionsofDiversity_name" = "PlantsThatFeedTheWorld_name", "primaryRegions"= "Region")

# read in the 
library(readxl)
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

# rename columns
croplist <- croplist %>% 
  rename("PlantsThatFeedTheWorld_name" = "PlantsthatFeedtheWorld_name", 
         "cropStrategy" = "CropStrategy", "genus" = "Genera_primary")

# Deduplicate the PlantsThatFeedTheWorld_regions dataset 
unique_PTFW_regions <- PlantsThatFeedTheWorld_regions %>% distinct(RegionsofDiversity_name, .keep_all = TRUE)

# join only our crops
croplist <- croplist %>% left_join(unique_PTFW_regions, by = "RegionsofDiversity_name")


# Assign special cases, where CK re-assigned primary regions for some crops 
# (column: Regionsofdiversity_new), add to Region column in croplist 
croplist <- croplist %>% mutate(primaryRegions = ifelse(is.na(primaryRegions) | primaryRegions == "", 
                                                Regionsofdiversity_new, 
                                                paste(primaryRegions, Regionsofdiversity_new, 
                                                sep = ", "))) %>% 
                                                # Remove all instances of ", NA" from the Region field 
                                                mutate(primaryRegions = str_replace_all(primaryRegions, ", NA", ""))






## add primaryRegions column from in croplist guide file to the combined_allcrops dataset, join by crop strategy 
## DONT RUN FOR FORAGES ..... and 
# ONLY include CWR, landraces and other 
# sampStat = 100s (wild + other wild types)
# sampStat = 300 (landrace)
# sampStat = 999 (other)
# sampStat = 200 (weedy)      ? include? <<<<<<<<<<< SG ask abt this

## exclude:
## sampStat = 400s (Breeding research material =  other breeding materials)
## sampStat = 500 (advanced or improved cultivar)
## sampStat =  600 (GMO)

# prep primaryRegions column to join in croplist
# EXCLUDE FORAGES
## subset only the relevant column(s) to join:
croplist_primaryRegions <- subset(croplist, select = c(genus, primaryRegions, cropStrategy)) %>%
          filter(cropStrategy != "Tropical and subtropical forages")%>% 
          select(-cropStrategy)

# join primaryRegions column in croplist_primaryRegions to combined_allcrops,
# join by genera
combined_allcrops <- combined_allcrops %>% left_join(croplist_primaryRegions, by = "genus")




## RE_RUN WITH CLEANED GENUS with cleaned genus to fill in all accessions
# rename the primary genus column in croplist to "acceptedGenus_TNRS"
# duplicate steps above for acceptedGenus_TNRS
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")
croplist <- croplist %>% rename("PlantsThatFeedTheWorld_name" = "PlantsthatFeedtheWorld_name", 
                              "cropStrategy" = "CropStrategy", "acceptedGenus_TNRS" = "Genera_primary")
croplist <- croplist %>% left_join(unique_PTFW_regions, by = "RegionsofDiversity_name")
croplist <- croplist %>% mutate(primaryRegions = ifelse(is.na(primaryRegions) | primaryRegions == "", 
                                                       Regionsofdiversity_new, 
                                                       paste(primaryRegions, Regionsofdiversity_new, 
                                                             sep = ", "))) %>% 
                                                            # Remove all instances of ", NA" from the Region field 
                                                        mutate(primaryRegions = str_replace_all(primaryRegions, ", NA", ""))

# subset relevant columns to join
croplist_primaryRegions <- subset(croplist, select = c(acceptedGenus_TNRS, primaryRegions, cropStrategy)) %>%
  filter(cropStrategy != "Tropical and subtropical forages")%>% 
  select(-cropStrategy)

# join by cleaned genus 
combined_allcrops <- combined_allcrops %>% left_join(croplist_primaryRegions, by = "acceptedGenus_TNRS")




# Fill NAs in primaryRegions.x with primaryRegions.y and vice versa 
combined_allcrops <- combined_allcrops %>% 
  mutate(primaryRegions.x = ifelse(is.na(primaryRegions.x), primaryRegions.y, primaryRegions.x), 
         primaryRegions.y = ifelse(is.na(primaryRegions.y), primaryRegions.x, primaryRegions.y)) %>%
  rename(primaryRegions = primaryRegions.x) %>% 
  select(-primaryRegions.y)


# Re-assign primaryRegions to NA if sampStat is 400s, 500, or 600 
combined_allcrops <- combined_allcrops %>% 
  mutate(primaryRegions = ifelse(sampStat %in% c(400:499, 500, 600), NA, primaryRegions))


# special case, assign "Faba vulgaris" by hand
# Update primaryRegions if fullTaxa contains "Faba vulgaris" and sampStat is not in 400s, 500, or 600 
combined_allcrops <- combined_allcrops %>% 
  mutate(primaryRegions = ifelse( str_detect(fullTaxa, "Faba vulgaris") 
                                 & !sampStat %in% c(400:499, 500, 600), "Asia_West", primaryRegions ))

View(combined_allcrops)



# Check all rows (excluding forages and sampStat gen modified) have primaryRegions assigned
na_count <- combined_allcrops %>% 
  filter(cropStrategy != "Tropical and subtropical forages" & !(sampStat %in% c(400:499, 500, 600))) %>% 
  summarise(na_count = sum(is.na(primaryRegions)))
# na count = 0
na_count








## Make a new column, isInPrimaryRegions
## is the country of origin of that crop accession, in the primary region of diversity for that crop?
## only include data with crops and CWRS (do not include info from landraces and Other)
## calculate based on country of origin field and primary region field (from guidefile)
                                      
## guidefile for what countries are in what regions 
library(readxl)
countries_in_regions <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/CropRegions_PlantsThatFeedtheWorld/countries_in_regions.xlsx")


# subset out columns we need, country codes field and primary regions field 
countries_in_regions <- subset(countries_in_regions, select = c(Country_code, PlantsThatFeedTheWorld_Region_new)) %>% 
  rename("primaryRegions" = "PlantsThatFeedTheWorld_Region_new", 
         "origCty" = "Country_code")


## try this
# processing was too laggy so created an alternative way to compute this 

library("dplyr") 
library("stringr") 

# Create a flag in countries_in_regions 
countries_in_regions <- countries_in_regions %>% mutate(match_flag = "Y") 

# Perform a left join and then use vectorized string matching 
# Update the flag based on partial matches 
# Leave isInPrimaryRegion as NA if no origCty
combined_allcrops <- combined_allcrops %>% 
  left_join(countries_in_regions, by = "origCty") %>% 
  mutate(isInPrimaryRegions = ifelse( str_detect(primaryRegions.x, paste0("\\b", primaryRegions.y, "\\b")), "Y", "N")) %>% 
  mutate(isInPrimaryRegions = ifelse( cropStrategy == "Tropical and subtropical forages", NA, isInPrimaryRegions)) %>% 
  mutate(isInPrimaryRegions = ifelse( sampStat %in% c(400:499, 500, 600), NA, isInPrimaryRegions)) %>% 
  mutate(isInPrimaryRegions = ifelse( is.na(origCty), NA, isInPrimaryRegions)) %>% 
  select(-match_flag) # remove temp match column


# rename primaryRegions.x to primaryRegions (primary region of diversity)
# rename primaryRegions.y to regionOforigCty (region of country of origin)
#could be useful to keep BOTH fields for visual checks on correct isInPrimaryRegion assignment
combined_allcrops <- combined_allcrops %>% 
       rename("primaryRegions" = "primaryRegions.x", 
              "regionOforigCty" = "primaryRegions.y")









# Check all rows (excluding forages and sampStat gen modified) have primaryRegions assigned
na_count <- combined_allcrops %>% 
  filter(cropStrategy != "Tropical and subtropical forages" & !(sampStat %in% c(400:499, 500, 600))) %>% 
  summarise(na_count = sum(is.na(primaryRegions)))
# na count = 0
na_count








## dont forget to resave combined_allcrops data file at end



############# Prep doi GLIS data ############################################
## list of number of DOIs by genus

# GLIS data
library(readxl)
GLIS_DOIs_by_genus <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/PlantTreatyGLIS_data/GLIS_DOIs/GLIS_ DOIs_by_genus.xlsx")
View(GLIS_DOIs_by_genus)

# Fill out cropStrategy field
# assign by original genus 
# join Genera_primary in croplist with genus in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column to join:
croplist_strategy <- subset(croplist, select = c(Genera_primary, CropStrategy))

#rename relevant columns 
croplist_strategy <- croplist_strategy %>% 
  rename( genus = Genera_primary, cropStrategy = CropStrategy) %>%
  drop_na()

# join cropStrategy to GLIS_DOIs_by_genus
GLIS_DOIs <- GLIS_DOIs_by_genus %>% left_join(croplist_strategy, by = "genus")


# arrange by alphabetical order of cropStrategy and columns with cropStategy as column 1
library("dplyr") 
GLIS_DOIs <- GLIS_DOIs %>% 
  arrange(cropStrategy) %>% 
  select(cropStrategy, genus, dois)

#save to file
library(writexl)
write_xlsx(GLIS_DOIs, path = "C:/Users/sgora/Desktop/GCCS-Metrics/Data/PlantTreatyGLIS_data/GLIS_DOIs/GLIS_ DOIs.xlsx")













# working notes, delete 
# find the country errors 

# Identify rows that have 400s, 500, or 600 in the sampStat column 
rows_with <- combined_allcrops3 %>% filter(cropStrategy != "Tropical and subtropical forages") %>%
  filter(apply(., 1, function(row) any(row == "Z07", na.rm = TRUE)))
View(rows_with)


# Identify rows that have 400s, 500, or 600 in the sampStat column 
rows_with <- combined_allcrops3 %>% 
  filter(cropStrategy != "Tropical and subtropical forages") %>% 
  filter(sampStat %in% c(400:499, 500, 600))

View(rows_with)
# Check all rows (excluding forages) have isInprimaryRegions assigned
na_count <- combined_allcrops3 %>% filter(cropStrategy != "Tropical and subtropical forages") %>%
  summarise(na_count = sum(is.na(isInPrimaryRegions)))

# View rows (excluding foragaes) with NA values
na_rows <- combined_allcrops3 %>% filter(cropStrategy != "Tropical and subtropical forages") %>%
  filter(is.na(isInPrimaryRegions))
view(na_rows)





# Identify rows with sampStat in the 400s, 500, or 600 
rows_with <- combined_allcrops3 %>% 
  filter(sampStat %in% c(400:499, 500, 600)) 


# Find and view the rows with CWR = "Y" 
rows_with_grasspea <- combined_allcrops %>% filter(fullTaxa == "Lathyrus sativus") 

# View the resulting rows 
View(rows_with_grasspea)







