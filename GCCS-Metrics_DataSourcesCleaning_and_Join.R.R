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

#rename all columns according to Genesys naming style:
colnames(BGCI_allcrops) <- c("source","fullSciName","fullTaxa","accepted","plantSearchId",
                              "cultivar","exSituSiteGardenSearchId","instName","city",
                              "stateProvince","origCtyFullName","country2", #2 letter abrev for origCty
                              "latitude", #of garden, dont use
                              "longitude", #of garden, dont use 
                              "germplasmPlant", "storage", #changed from germplasmSeed
                              "germplasmPollen", "germplasmExplant",
                              "acceptedNamePlantSearch","synNamePlantSearch","addedSubmittedNames")

# Fields we want to keep:
library(magrittr)
library(dplyr)
BGCI_allcrops <- subset(BGCI_allcrops, 
                        select = c(fullSciName, fullTaxa, 
                                                  cultivar, origCtyFullName,
                                                  country2, storage)) %>%
                        mutate(across(everything(), ~gsub("[[:punct:]]", "", .x)))  #remove special characters

# Add fields: data source
BGCI_allcrops <- cbind(BGCI_allcrops, data_source = "BGCI")


# Separate fields: fullSciName, still have fullTaxa
# fullSciName into genus and species
library(tidyr)
BGCI_allcrops  <- BGCI_allcrops  %>% separate(fullSciName, c('genus', 'species'))


## Encode the storage field with presence of seed values "1"
# replace '1' in storage column with '10' (code for seed)
BGCI_allcrops['storage'][BGCI_allcrops['storage'] == 1] <- 10 
BGCI_allcrops['storage'][BGCI_allcrops['storage'] == 0] <- NA

### need to combine other germplasm storage columns- germplasmPlant, germplasmPollen, germplasmExplant
## encode as "other" storage 






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

# Join the data frames
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


## Clean country field according to notes




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
                            sampStat, duplSite, latitude,longitude, # acqSRC_WIEWS, 
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

# Join the data frames
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

## Clean country field according to notes





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
##### KEPT WAYYYY TOO MANY FIELDS, KEEP CUTTING DOWN ######
Genesys_allcrops <- subset(Genesys_allcrops, select = c(instCode, acceNumb, 
                                                    genus, species, acceptedName_TNRS,
                                                    # spAuthor, subTaxa, subTAuthor, doi, acqDate,
                                                    cropName_Genesys, sampStat, acceName_Genesys,
                                                    origCty, latitude,longitude,
                                                    # donorCode_Genesys,donorName_Genesys, donorNumb_Genesys,
                                                    # otherNumb_Genesys, ancest_Genesys, duplInstName,
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


## Clean country field according to notes


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
##### KEPT WAYYYY TOO MANY FIELDS, KEEP CUTTING DOWN ######
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


# Country clean:
# fix the countries that were labled "Z01", "Z06" and "Z07" and drop the extra country field
# move over the data from country3 field 
# only for those rows with countries that were labled as Z01, Z06, Z07
GBIF_allcrops <- GBIF_allcrops %>% 
                    mutate( origCty = if_else(origCty %in% c("Z01", "Z06", "Z07"), country3, origCty) )%>%
                    select(-country3)

View(GBIF_allcrops)


## Encode storage field with WIEWS codes
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

# Join the data frames
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


## Clean country field according to notes



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

# Join the data frames
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

## Clean country field according to notes







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

# drop row if SGSV dataset doesnt have our crop
SGSV_allcrops <- SGSV_allcrops %>% filter(!is.na(cropStrategy))







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

## Remove duplicated in WIEWS
# if acceNumb and instCode are the same in Genesys and WIEWS, then remove row/accession record in WIEWS
WIEWS_allcrops_duprmv <- WIEWS_allcrops %>%
  anti_join(Genesys_allcrops, by = c("acceNumb", "instCode"))
# over 1M rows of data lost

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
library(write.csv)
write_csv(combined_allcrops, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")





###############################################################################################
#### Data cleaning to do after all data sources combined ######################################

# combined data
library(readr)
combined_allcrops <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")
View(combined_allcrops)


# fix the issue of not all crops are filled out
# colin went through, can add 32 rows back in later 

# temp fix: 
# for now, just dropped the rows (32 rows) that have crop strategy = NA
combined_allcrops <- combined_allcrops %>% filter(!is.na(cropStrategy))

## need to go and add in the 32 rows of useable data 




## Institution international status field
# holdingInst_internationalStatus, Y/N, based on instCode ***

# read in international institutions list
library(readr)
library(readxl)
institute_names <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/WIEWS_institutes_2020.xlsx")

internationalInst <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_internationalgenebankslist.xlsx")

## subset only the relevant column to join:
# 3 letter ISO3 institute code and the full name of the institute
institute_names_full <- subset(institute_names, select = c(INSTCODE, FULL_NAME))

#rename relevant columns 
institute_names_full <- institute_names_full %>% 
  rename( instName= FULL_NAME, instCode = INSTCODE) %>%
  drop_na()

library(dplyr)
internationalInst <- internationalInst %>%
  left_join(institute_names_full, by = "instName", relationship= "many-to-many")

## save 
library(writexl)
write_xlsx(internationalInst, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_internationalgenebankslist_codes2.xlsx")



### fill out internationalStatus column 
## subset only the relevant column of names list
internationalInst <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_internationalgenebankslist_codes2.xlsx")
internationalInst <- subset(internationalInst, select = c(instName, internationalStatus))

internationalInstCodes <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_internationalgenebankslist_codes2.xlsx")
internationalInstCodes <- subset(internationalInstCodes, select = c(instCode, internationalStatus)) %>%
  drop_na()


#join internationalStatus column from institution code
library(dplyr)
combined_allcrops <- combined_allcrops %>% left_join(internationalInstCodes, combined_allcrops, by = c("instCode"))


### Fill out isCrop field
## based on TNRS clean taxa field and Crop List 
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")


# based on "acceptedName_TNRS" field 
# isCrop = Y if names match
# isCrop = N if does not match
isCrop_vector <- ifelse(combined_allcrops$acceptedName_TNRS %in% croplist$Taxa_main, "Y", "N")

# add vector to dataframe
combined_allcrops <- combined_allcrops %>% mutate(isCrop = isCrop_vector)

### Fill out isCWR field (Crop Wild Relative field)
# Add column assgning all "no"s in isCrop as "yes"s in isCWR
combined_allcrops <- combined_allcrops %>% mutate(isCWR = if_else(isCrop == "N", "Y", "N"))



# then fill out strategy field
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
View(combined_allcrops)



na_count <- combined_allcrops_test %>% summarise(na_count = sum(is.na(cropStrategy.y)))
na_rows <- combined_allcrops_test %>% filter(is.na(cropStrategy.x)) # View the rows with NA values na_rows



# then fill out strategy field
# by original genus 
# join Genera_primary in croplist with genus in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")


# this code didnt work, delete below
# add Crop Descriptors field
# by Crop
# join cropStrategy (AKA CROP) in croplist with cropStrategy in combined_allcrops
# croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column to join:
# croplist_descriptor <- subset(croplist, select = c('CropStrategy', 'HasCropDescriptor', 'Crop Descriptor Link'))

#rename relevant columns 
# croplist_descriptor <- croplist_descriptor %>% rename( cropStrategy = CropStrategy, hasCropDescriptor = HasCropDescriptor, cropDescriptorLink = `Crop Descriptor Link` )

# library(dplyr)
# combined_allcrops <- combined_allcrops %>%
#  left_join(croplist_descriptor, by = "cropStrategy", relationship= "many-to-many")








# add Annex I field
# Use Annex1 guidefile combined with taxon field

# join Annex1 (and Annex1inclusions) field in croplist with cleaned taxon field in combined_allcrops
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

## subset only the relevant column(s) to join:
croplist_Annex1 <- subset(croplist, select = c(Genera_primary, Annex1, Annex1inclusions))


# rename the primary genus column in croplist to "acceptedName_TNRS"
croplist_Annex1 <- croplist_Annex1 %>% 
  rename(acceptedGenus_TNRS = Genera_primary)

# join 2 datsets by genus, and fill out Annex1 field if isCrop=Y
# isCrop = N, then Annex1 field = NA
# Join the datasets by acceptedGenus_TNRS 
combined_allcrops <- combined_allcrops %>% 
  left_join(croplist_Annex1, by = "acceptedGenus_TNRS") 

# Fill out the Annex1 field from croplist_Annex1 only if isCrop is "Y" and set to NA if isCrop is "N" 
combined_allcrops <- combined_allcrops %>% 
  mutate(Annex1 = case_when( isCrop == "Y" ~ croplist_Annex1$Annex1[match(acceptedGenus_TNRS, croplist_Annex1$acceptedGenus_TNRS)], 
                             isCrop == "N" ~ NA_character_, TRUE ~ Annex1 ))


## add special cases for Annex1 field

# if acceptedGenus_TNRS = Manihot, then Annex1= N 
#     except when acceptedName = Manihot esculenta, then Annex1=Y

# if acceptedGenus_TNRS = Andropogon, then Annex1= N 
#     except when acceptedName = Andropogon gayanus, then Annex1=Y

# if acceptedGenus_TNRS = Astragalus, then Annex1= N
#     except when acceptedName_TNRS = Astragalus chinensis, Astragalus cicer, or Astragalus arenarius then Annex1=Y

# if acceptedGenus= Zea, then Annex1=Y 
#     except when acceptedName_TNRS = Zea perennis, Zea diploperennis, or Zea luxurians, then Annex1-N

# if acceptedGenus= Atriplex, then Annex1=N 
#     except when acceptedName_TNRS = Atriplex halimus, Atriplex nummularia then Annex1-Y

# if acceptedGenus= Lotus, then Annex1=N 
#     except when acceptedName_TNRS =  Lotus corniculatus, Lotus subbiflorus, Lotus uliginosus then Annex1-Y

# if acceptedGenus= Medicago, then Annex1=N 
#     except when acceptedName_TNRS = Medicago sativa, Medicago arborea, Medicago falcata, Medicago scutellata, Medicago rigidula, Medicago truncatula, Medicago sativa subsp. sativa, Medicago sativa ssp. sativa, Medicago sativa var. sativa then Annex1=Y

# if acceptedGenus= Melilotus, then Annex1=N 
#     except when acceptedName_TNRS = Melilotus albus, Melilotus officinalis then Annex1=Y

# if acceptedGenus= Trifolium, then Annex1=N 
#    except when acceptedName_TNRS = Trifolium repens, Trifolium pratense, Trifolium alexandrinum, Trifolium alpestre, 
#          Trifolium ambiguum, Trifolium angustifolium, Trifolium arvense, 
#          Trifolium agrocicerum, Trifolium hybridum, Trifolium incarnatum, 
#          Trifolium pratense, Trifolium resupinatum, Trifolium rueppellianum, 
#          Trifolium semipilosum, Trifolium subterraneum, Trifolium vesiculosum then Annex1=Y



# Update the Annex1 column based on the special cases
combined_allcrops <- combined_allcrops %>% 
  mutate(Annex1 = case_when( 
    acceptedGenus_TNRS == "Manihot" & acceptedName_TNRS != "Manihot esculenta" ~ "N", 
    acceptedName_TNRS == "Manihot esculenta" ~ "Y", 
    acceptedGenus_TNRS == "Astragalus" & !acceptedName_TNRS %in% c("Astragalus chinensis", "Astragalus cicer", "Astragalus arenarius") ~ "N", 
    acceptedName_TNRS %in% c("Astragalus chinensis", "Astragalus cicer", "Astragalus arenarius") ~ "Y", 
    acceptedGenus_TNRS == "Zea" & !acceptedName_TNRS %in% c("Zea perennis", "Zea diploperennis", "Zea luxurians") ~ "Y", 
    acceptedName_TNRS %in% c("Zea perennis", "Zea diploperennis", "Zea luxurians") ~ "N", 
    acceptedGenus_TNRS == "Atriplex" & !acceptedName_TNRS %in% c("Atriplex halimus", "Atriplex nummularia") ~ "N", 
    acceptedName_TNRS %in% c("Atriplex halimus", "Atriplex nummularia") ~ "Y", 
    acceptedGenus_TNRS == "Lotus" & !acceptedName_TNRS %in% c("Lotus corniculatus", "Lotus subbiflorus", "Lotus uliginosus") ~ "N", 
    acceptedName_TNRS %in% c("Lotus corniculatus", "Lotus subbiflorus", "Lotus uliginosus") ~ "Y", 
    acceptedGenus_TNRS == "Medicago" & !acceptedName_TNRS %in% c("Medicago sativa", "Medicago arborea", "Medicago falcata", 
                                                                 "Medicago scutellata", "Medicago rigidula", "Medicago truncatula", 
                                                                 "Medicago sativa subsp. sativa", "Medicago sativa ssp. sativa", 
                                                                 "Medicago sativa var. sativa") ~ "N", 
    acceptedName_TNRS %in% c("Medicago sativa", "Medicago arborea", "Medicago falcata", "Medicago scutellata", "Medicago rigidula", 
                             "Medicago truncatula", "Medicago sativa subsp. sativa", "Medicago sativa ssp. sativa", 
                             "Medicago sativa var. sativa") ~ "Y", 
    acceptedGenus_TNRS == "Melilotus" & !acceptedName_TNRS %in% c("Melilotus albus", "Melilotus officinalis") ~ "N", 
    acceptedName_TNRS %in% c("Melilotus albus", "Melilotus officinalis") ~ "Y", 
    acceptedGenus_TNRS == "Trifolium" & !acceptedName_TNRS %in% c("Trifolium repens", "Trifolium pratense", "Trifolium alexandrinum", 
                                                                  "Trifolium alpestre", "Trifolium ambiguum", "Trifolium angustifolium", 
                                                                  "Trifolium arvense", "Trifolium agrocicerum", "Trifolium hybridum", 
                                                                  "Trifolium incarnatum", "Trifolium pratense", "Trifolium resupinatum", 
                                                                  "Trifolium rueppellianum", "Trifolium semipilosum", "Trifolium subterraneum", 
                                                                  "Trifolium vesiculosum") ~ "N", 
    acceptedName_TNRS %in% c("Trifolium repens", "Trifolium pratense", "Trifolium alexandrinum", "Trifolium alpestre", "Trifolium ambiguum", 
                             "Trifolium angustifolium", "Trifolium arvense", "Trifolium agrocicerum", "Trifolium hybridum", "Trifolium incarnatum", 
                             "Trifolium pratense", "Trifolium resupinatum", "Trifolium rueppellianum", "Trifolium semipilosum", "Trifolium subterraneum", 
                             "Trifolium vesiculosum") ~ "Y", 
    TRUE ~ Annex1 # Keep the existing value for all other cases 
     ))

view(combined_allcrops)






### Read in the plants that feed the world regions guide file

# library(readxl)
PlantsThatFeedTheWorld_regions <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/CropRegions_PlantsThatFeedtheWorld/PlantsThatFeedTheWorld_Regions.xlsx")
View(PlantsThatFeedTheWorld_regions)

# rename column 
PlantsThatFeedTheWorld_regions <- PlantsThatFeedTheWorld_regions %>% 
  rename("RegionsofDiversity_name" = "PlantsThatFeedTheWorld_name")

# read in the 
library(readxl)
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")

# rename column 
croplist <- croplist %>% 
  rename("PlantsThatFeedTheWorld_name" = "PlantsthatFeedtheWorld_name")

# only contains the crops in our dataset
## i dont think this works
# PlantsThatFeedTheWorld_regions <- PlantsThatFeedTheWorld_regions %>% 
#  inner_join(croplist %>% select(PlantsThatFeedTheWorld_name), by = "PlantsThatFeedTheWorld_name")

### join the Plants that Feed the World regions to the croplist by PlantsThatFeedTheWorld_name" 
# croplist3 <- croplist %>% left_join(PlantsThatFeedTheWorld_regions, by = "PlantsThatFeedTheWorld_name")


# Deduplicate the PlantsThatFeedTheWorld_regions dataset 
unique_PTFW_regions <- PlantsThatFeedTheWorld_regions %>% distinct(RegionsofDiversity_name, .keep_all = TRUE)

# join only our crops
croplist <- croplist %>% left_join(unique_PTFW_regions, by = "RegionsofDiversity_name")


# Combine data from Regionsofdiversity_new with existing data in Region column 
croplist <- croplist %>% mutate(Region = ifelse(is.na(Region) | Region == "", 
                                                Regionsofdiversity_new, 
                                                paste(Region, Regionsofdiversity_new, 
                                                sep = ", ")))



##  join Regions column in croplist guide file to the combined_allcrops dataset, join by crop strategy 
## 




## make a new column, isinPrimaryRegion
## only include data with crops and CWRS (do not include info from landraces and Other)
## calculate based on country of origin field and primary region field (from guidefile)
                                      
## guidefile for what countries are in what regions 


View(croplist)

View(PlantsThatFeedTheWorld_regions)









# working notes, delete 
# find the country errors 

# Identify rows that have the record "Z06" in any column 
rows_with_ZAR <- combined_allcrops %>% 
  filter(apply(., 1, function(row) any(row == "ZAR", na.rm = TRUE)))
View(rows_with_ZAR)


