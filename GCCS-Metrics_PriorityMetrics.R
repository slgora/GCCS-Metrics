### Project: Global Crop Conservation Strategies Metrics ###
### Priority Metrics Calculations 
### GCCS-Metrics_PriorityMetrics.R
### by Sarah Gora
### Date created: 2024_11_20



#### Set working directory ####
setwd()
setwd("C:/Users/sgora/Desktop/GCCS-Metrics/Code/Metrics_Calculations/GCCS-Metrics_PriorityMetrics.R")


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

# read in combined data set (this does not include SGSV)
combined_allcrops <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")

# combined dataset
View(combined_allcrops)




##### METRIC: Total number of accessions in collection ##############################
# filter by crop  
combined_allcrops_sum <- combined_allcrops %>%
  group_by(cropStrategy) %>%
  summarise(cropcount = n())
View(combined_allcrops_sum)

# filter by crop and data source 
combined_allcrops_sum2 <- combined_allcrops %>%
  group_by(data_source, cropStrategy) %>%
  summarise(count = n())
#View(combined_allcrops_sum2)



##### METRIC: Number of institutions holding crop germplasm ##############################
# filter by crop 
uniqueInstitionsCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(unique_instCount = n_distinct(instCode))
View(uniqueInstitionsCount)



##### METRIC: Number of accessions of Crop Wild Relatives ##############################
# metric 1, count # of cwrs by code, 
# metric 2, count/ list cwr taxon names 
# filter by crop 
# sampStat = 100 
# need integer and %


## Metric 1, count # of cwrs by code (all values between 100-199)

## what abt calculation by assigning of CWR? based on isCWR and croplist?

# count- integer
cwrCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_100sampStat = sum(sampStat >= 100 & sampStat < 200, na.rm = TRUE)) 
#View(cwrCount)

# count as a % of the whole
cwrPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_100sampStat = sum(sampStat >= 100 & sampStat < 200, na.rm = TRUE), cwr_total_records = n()) %>%  
  mutate(percent_100sampStat = round((count_100sampStat / cwr_total_records) * 100, 2))
#View(cwrPerc)


## metric 2, count/ list unique taxon 
unique_taxa <- combined_allcrops %>% 
  select(cropStrategy, acceptedName_TNRS) %>% 
  distinct() %>% # Get unique rows 
  group_by(cropStrategy) %>% 
  summarise( unique_taxa = list(unique(acceptedName_TNRS)), 
             # List of unique taxa 
             unique_taxa_count = n_distinct(acceptedName_TNRS) 
             # Count the number of unique taxa 
             )

View(unique_taxa)



# Create the list of unique taxa and count them 
unique_taxa <- combined_allcrops %>% 
  filter(isCWR == "Y") %>% 
  select(cropStrategy, acceptedName_TNRS) %>% 
  distinct() %>% # Get unique rows 
  group_by(cropStrategy) %>% 
  summarise( unique_taxa = list(unique(acceptedName_TNRS)), unique_taxa_count = n_distinct(acceptedName_TNRS) )
View(unique_taxa)

# example list of taxa names 
desired_crop_strategy <- "Aroids" 
unique_taxa_for_strategy <- unique_taxa %>% filter(cropStrategy == desired_crop_strategy) 
# Extract the list of unique taxa 
taxa_list <- unique_taxa_for_strategy$unique_taxa[[1]] 
# View the list of unique taxa 
print(taxa_list)




##### METRIC: Number of accessions of Weedy ############################################
# filter by crop 
# sampStat = 200 
# need integer and %

# count- integer
# filter by crop
weedyCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_200sampStat = sum(sampStat == 200, na.rm = TRUE))
#View(weedyCount)

# Count as a % of the whole 
weedyPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_200sampStat = sum(sampStat == 200, na.rm = TRUE), weedy_total_records = n() ) %>% 
  mutate( percent_200sampStat = round((count_200sampStat / weedy_total_records) * 100, 2) )
#View(weedyPerc)



##### METRIC: Number of accessions of Landraces ############################################
# filter by crop 
# sampStat = 300 
# need integer and %

# count- integer
# filter by crop
landraceCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_300sampStat = sum(sampStat == 300, na.rm = TRUE))
#View(landraceCount)

# Count as a % of the whole 
landracePerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_300sampStat = sum(sampStat == 300, na.rm = TRUE), landrace_total_records = n() ) %>% 
  mutate( percent_300sampStat = round((count_300sampStat / landrace_total_records) * 100, 2) )
#View(landracePerc)



##### METRIC: Number of accessions of Breeding Material ############################################
# filter by crop 
# sampStat = 400s (all values in 400s)
# need integer and %

## count # of breeding material by code (all values between 400-499)

# count- integer
breedingmatCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_400sampStat = sum(sampStat >= 400 & sampStat < 500, na.rm = TRUE)) 
#View(breedingmatCount)

# count as a % of the whole
breedingmatPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_400sampStat = sum(sampStat >= 400 & sampStat < 500, na.rm = TRUE), breedingmat_total_records = n()) %>%  
  mutate(percent_400sampStat = round((count_400sampStat / breedingmat_total_records) * 100, 2))
#View(breedingmatPerc)



##### METRIC: Number of accessions of Improved Varieties ######################################
# filter by crop 
# sampStat = 500 
# need integer and %

# count- integer
# filter by crop
improvedvarCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_500sampStat = sum(sampStat == 500, na.rm = TRUE))
View(improvedvarCount)

### calculate % 
improvedvarPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_500sampStat = sum(sampStat == 500, na.rm = TRUE), improvedvar_total_records = n() ) %>% 
  mutate( percent_500sampStat = round((count_500sampStat / improvedvar_total_records) * 100, 2) )
View(improvedvarPerc)



##### METRIC: Number of accessions of Other Varieties ######################################
# filter by crop 
# sampStat = 999 
# need integer and %

# count- integer
# filter by crop
othervarCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_999sampStat = sum(sampStat == 999, na.rm = TRUE))
View(othervarCount)

# calculate %
othervarPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_999sampStat = sum(sampStat == 999, na.rm = TRUE), othervar_total_records = n() ) %>% 
  mutate( percent_999sampStat = round((count_999sampStat / othervar_total_records) * 100, 2) )
View(othervarPerc)



##### METRIC: Number of accessions not marked with a sampStat type ############################################
# filter by crop 
# sampStat = NA
# need integer and %

# count- integer
# filter by crop
# Count the number of accessions with sampStat == NA 
nosampStatCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_NA_sampStat = sum(is.na(sampStat)))
#View(nosampStatCount)

# Count as a % of the whole 
nosampStatPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_NA_sampStat = sum(is.na(sampStat)), nosampStat_total_records = n()) %>% 
  mutate(percent_NA_sampStat = round((count_NA_sampStat / nosampStat_total_records) * 100, 2))
#View(nosampStatPerc)



##### METRIC: Number of countries where germplasm has been collected ########################

# calculation has excluded sampStat rows of 400s, 500, 600 (improved varieties, etc)

# count- integer
# filter by crop
countryCount <- combined_allcrops %>% 
  filter(!(sampStat %in% c(400:499, 500, 600))) %>% 
  group_by(cropStrategy) %>% summarise(unique_countryCount = n_distinct(origCty))
#View(countryCount)




### METRIC:  Number of accessions from primary region(s) of diversity
# filter by crop 
# isInPrimaryRegions = Y
# need integer and %

# count- integer
isInPrimaryRegionCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(isInPrimaryRegion_Y_count = sum(isInPrimaryRegions == "Y", na.rm = TRUE))
#View(isInPrimaryRegionCount)


# calculate %
isInPrimaryRegionPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(isInPrimaryRegion_Y_count = sum(isInPrimaryRegions == "Y", na.rm = TRUE), primaryRegions_total_records = n() ) %>% 
  mutate( isInPrimaryRegion_Perc = round((isInPrimaryRegion_Y_count / primaryRegions_total_records) * 100, 2) )
#View(isInPrimaryRegionPerc)




### METRIC:  What kind of institutions #####################################################
## categorize as international vs national 
## need integer and % 

## 2 metrics-
# number of accessions in international
# number of accessions in national/other
## Svalbard already categorized as N= Not international **


# number of accessions in institutions categorized as international 
# count- integer
combined_allcrops

internationalInst_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE))


## number of institutions categorized as not international or other SGSV 
# count- integer 
notinternationalInst_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(notinternationalInst_count = sum(internationalStatus == "N" | is.na(internationalStatus)))



# number of accessions with institutions categorized as international 
# calculate %
internationalInst_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(internationalIns_count = sum(internationalStatus == "Y", na.rm = TRUE), internationalInst_total_records = n() ) %>% 
  mutate( internationalInst_Perc = round((internationalIns_count / internationalInst_total_records) * 100, 2) )

# number of accessions with institutions categorized as not international 
# calculate %
notinternationalInst_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(notinternationalInst_count = sum(internationalStatus == "N" | is.na(internationalStatus)), notinternationalInst_total_records = n()) %>% 
  mutate( notinternationalInst_Perc = round((notinternationalInst_count / notinternationalInst_total_records) * 100, 2) )





### METRIC:  Number of accessions in the MLS #############################################
# Need integer and % (of total accessions included in MLS)

## filter by crop 
# I stands for included
mlsI_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_IncludedMLS = sum(mlsStat == "I", na.rm = TRUE))

# N stands for not included
mlsN_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_NotIncludedMLS = sum(mlsStat == "N" | is.na(mlsStat))) #count No and Nas #count No and Nas


# calculate %
mlsI_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(mlsIncluded_count = sum(mlsStat == "I", na.rm = TRUE), mlsIncluded_total_records = n()) %>% 
  mutate( mlsIncluded_Perc = round((mlsIncluded_count / mlsIncluded_total_records) * 100, 2) )

# calculate %
mlsN_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(mlsNotIncluded_count = sum(mlsStat == "N" | is.na(mlsStat)), mlsNotIncluded_total_records = n()) %>% 
  mutate( mlsNotIncluded_Perc = round((mlsNotIncluded_count / mlsNotIncluded_total_records) * 100, 2) )


### METRIC:  Number of accessions in the MLS held by international collections
# Need integer and % of total accessions included in MLS

# Count of accessions in the MLS held by international collections
mlsY_internationalInst_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE), mlsStat_count = sum(mlsStat == "I", na.rm = TRUE))

# calculate % of accessions in the MLS held by international collections
mlsY_internationalInst_perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE), mlsStat_count = sum(mlsStat == "I", na.rm = TRUE), 
             mlsY_internationalInst_count = sum(mlsStat == "I" & internationalStatus == "Y", na.rm = TRUE), 
             percentage_mlsY_internationalInst = (mlsY_internationalInst_count / mlsStat_count) * 100 )




### METRIC:  Number of accessions held in Annex I
# Need integer and % of total accessions held in Annex I

# Count of accessions in Annex I 
annex1_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_IncludedAnnex1 = sum(annex1 == "Y", na.rm = TRUE))

# calculate % of accessions in Annex I
annex1_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(annex1_count = sum(annex1 == "Y", na.rm = TRUE), annex1_total_records = n()) %>% 
  mutate( annex1_Perc = round((annex1_count / annex1_total_records) * 100, 2) )




### METRIC: Number of accessions held in each storage category 
##
# 6 different metrics
# Seed = 10, 11, 12, 13
# Field = 20 
# In vitro = 30 
# Cryo = 40 
# DNA = 50 
# Other = 99 

## number of accessions held in seed storage
seed_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(seed_storage_count = sum(str_detect(storage, "10|11|12|13"), na.rm = TRUE))

# calculate %
seed_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( seed_storage_count = sum(str_detect(storage, "10|11|12|13"), na.rm = TRUE), seedstorage_total_records = n() ) %>% 
  mutate( seed_storage_Perc = round((seed_storage_count / seedstorage_total_records) * 100, 2) )



## number of accessions from field collections 
field_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(field_col_count = sum(str_detect(storage, "20"), na.rm = TRUE))


field_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(field_storage_count = sum(str_detect(storage, "20"), na.rm = TRUE), fieldstorage_total_records = n()) %>% 
  mutate( field_storage_Perc = round((field_storage_count / fieldstorage_total_records) * 100, 2) )



## number of accessions in in-vitro collections 
invitro_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(invitro_count = sum(str_detect(storage, "30"), na.rm = TRUE))

# percent
invitro_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( invitro_storage_count = sum(str_detect(storage, "30"), na.rm = TRUE), invitrostorage_total_records = n() ) %>% 
  mutate( invitro_storage_Perc = round((invitro_storage_count / invitrostorage_total_records) * 100, 2) ) 
View(invitro_Perc)


## number of accessions in cryo collections 
cryo_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(cryo_col_count = sum(str_detect(storage, "40"), na.rm = TRUE))

cryo_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(cryo_storage_count = sum(str_detect(storage,"40"), na.rm = TRUE), cryostorage_total_records = n()) %>% 
  mutate( cryo_storage_Perc = round((cryo_storage_count / cryostorage_total_records) * 100, 2) )


## number of accessions in DNA collections 
dna_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(dna_col_count = sum(str_detect(storage, "50"), na.rm = TRUE))

dna_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(dna_storage_count = sum(str_detect(storage, "50"), na.rm = TRUE), dna_storage_total_records = n()) %>% 
  mutate( dna_storage_Perc = round((dna_storage_count / dna_storage_total_records) * 100, 2) )


## number of accessions in other collections 
other_col_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(other_col_count = sum(str_detect(storage, "99"), na.rm = TRUE))

otherstorage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(other_storage_count = sum(str_detect(storage, "99"), na.rm = TRUE), other_storage_total_records = n()) %>% 
  mutate( other_storage_Perc = round((other_storage_count / other_storage_total_records) * 100, 2) )


## number of accessions with no data in the storage field 
# Count the number of accessions with storage == NA 
nostorage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(nostorage_count = sum(is.na(storage))) 

# Count as a % of the whole 
nostorage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(nostorage_count = sum(is.na(storage)), nostorage_total_records = n()) %>% 
  mutate(nostorage_Perc = round((nostorage_count / nostorage_total_records) * 100, 2))





### METRIC: Number of accessions held in Long-term storage 
# storage = 13
## number of accessions in other collections 
longterm_storage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(longterm_storage_count = sum(str_detect(storage,"13"), na.rm = TRUE))

longterm_storage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(longterm_storage_count = sum(str_detect(storage,"13"), na.rm = TRUE), longterm_storage_total_records = n()) %>% 
  mutate( longterm_storage_Perc = round((longterm_storage_count / longterm_storage_total_records) * 100, 2) )


### METRIC: Number of accessions held in Medium-term storage 
## storage = 12
medterm_storage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(medterm_storage_count = sum(str_detect(storage,"12"), na.rm = TRUE))

medterm_storage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(medterm_storage_count = sum(str_detect(storage, "12"), na.rm = TRUE), medterm_storage_total_records = n()) %>% 
  mutate( medterm_storage_Perc = round((medterm_storage_count / medterm_storage_total_records) * 100, 2) )


### METRIC: Number of accessions held in Short-term storage 
## storage = 11
shortterm_storage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(shortterm_storage_count = sum(str_detect(storage, "11"), na.rm = TRUE))

shortterm_storage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(shortterm_storage_count = sum(str_detect(storage, "11"), na.rm = TRUE), shortterm_storage_total_records = n()) %>% 
  mutate( shortterm_storage_Perc = round((shortterm_storage_count / shortterm_storage_total_records) * 100, 2) )










### METRIC: Number of accessions safety duplicated ########################################
# count everything that has a value in it, everything but the NAs
safetydupl_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(safDuplSite_count = sum(!is.na(duplSite)))


## i dont know if this is correct..... 
safetydupl_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(safDuplSite_count = sum(!is.na(duplSite)), safDuplSite_total_records = n()) %>% 
  mutate( safDuplSite_Perc = round((safDuplSite_count / safDuplSite_total_records) * 100, 2) )




### METRIC: Number of accessions safety duplicated in SGSV ##################################

### use SGSV dataset
View(SGSV_allcrops)

SGSV_dupl_count <- SGSV_allcrops %>%
  group_by(cropStrategy) %>%
  summarise(SGSVcount = n())


### didnt figure this one out yet
## need to be divided by total accessions of crop 

# SGSV_dupl_perc <- SGSV_dupl_count %>% 
  # Calculate the total count 
#  mutate(total_count = sum(SGSVcount)) %>% 
  # Calculate the percentage of the whole 
#  mutate(percentage = (SGSVcount / total_count) * 100)









### METRIC: Number of DOIs (per crop) ##################################

## can run by genus too?

## use GLIS plant treaty data 
library(readxl)
GLIS_DOIs <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/PlantTreatyGLIS_data/GLIS_DOIs/GLIS_ DOIs.xlsx")
View(GLIS_DOIs)

# count of DOIs per crop
GLIS_dois_count <- GLIS_DOIs %>% 
  group_by(cropStrategy) %>% 
  summarise(DOIs = sum(dois, na.rm = TRUE))
View(GLIS_dois_count)




################## Plants That Feed the World data metrics 



### read in Plants that Feed the World indicator file that has been filtered by our crops
library(readxl)
PTFTW_indicator_avg_ourCrops <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/PlantsThatFeedTheWorld/PTFTW_indicator_avg_ourCrops.xlsx")
View(PTFTW_indicator_avg_ourCrops)

PTFTW_indicator_sum_ourCrops <- subset(PTFTW_indicator_avg_ourCrops, 
                                       select = c( "cropStrategy",
                                       "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene", 
                                       "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome",
                                       "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide", 
                                       "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein", 
                                       "supply-research_supply-research_supply_gbif-research_supply_gbif_taxon", 
                                       "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions",
                                       "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples", 
                                       "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty", 
                                       "demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon", 
                                       "demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon", 
                                       "crop_use-faostat-production-area_harvested_ha", 
                                       "crop_use-faostat-production-gross_production_value_us", 
                                       "crop_use-faostat-production-production_quantity_tonnes", 
                                       "crop_use-faostat-trade-export_quantity_tonnes", 
                                       "crop_use-faostat-trade-export_value_tonnes"
                                       ))

## sum across genera and crops for metrics that need to be summed
PTFTW_summarised <- PTFTW_indicator_sum_ourCrops %>% 
  group_by(cropStrategy) %>% 
  summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE))

# save PTFTW summed metrics
write_xlsx(PTFTW_summarised, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/PTFTW_metrics_sum.xlsx")


# average PTFTW data by crop (some have multiple genera per crop)
# combine each field as a list and separate by a semicolon: PTFTW_name, genus, fullTaxa
# average the numbers across crops
# removed irrelvant columns 
PTFTW_indicator_avg_combinedbycrop <- PTFTW_indicator_avg_ourCrops %>% 
  group_by(cropStrategy) %>% 
  summarise( PTFTW_name = paste(unique(na.omit(PTFTW_name)), collapse = "; "), 
             genus = paste(unique(na.omit(genus)), collapse = "; "), 
             fullTaxa = paste(unique(na.omit(fullTaxa)), collapse = "; "), 
             across(where(is.numeric), ~ ifelse(all(is.na(.x)), NA, mean(.x, na.rm = TRUE))))
View(PTFTW_indicator_avg_combinedbycrop)


# remove any column that doesnt have numeric data (fields with taxon data ect)
PTFTW_metrics <- PTFTW_indicator_avg_combinedbycrop %>%
  select(-PTFTW_name, -genus, -fullTaxa)
       
# join PTFTW_metrics of with other data metrics calculations     
View(PTFTW_metrics)




# Transpose for Tables (Table 6) later 
PTFTW_metrics_transposed <- as.data.frame(t(PTFTW_metrics))

# Set the first row as the column names 
colnames(PTFTW_metrics_transposed) <- PTFTW_metrics_transposed[1, ] 
# Remove the first row since it's now the header 
PTFTW_metrics_transposed <- PTFTW_metrics_transposed[-1, ]

PTFTW_metrics_transposed_rch <- PTFTW_metrics_transposed %>% select(Rice, Chickpea) 
View(PTFTW_metrics_transposed)



# list of PTWFTW metrics 

# "crop_use-faostat-food_supply-fat_supply_quantity_g"
# "crop_use-faostat-food_supply-food_supply_kcal"                                                                          
# "crop_use-faostat-food_supply-food_supply_quantity_g"                                                                    
# "crop_use-faostat-food_supply-protein_supply_quantity_g"                                                                 
# "crop_use-faostat-production-area_harvested_ha"                                                                          
# "crop_use-faostat-production-gross_production_value_us"                                                                  
# "crop_use-faostat-production-production_quantity_tonnes"                                                                 
# "crop_use-faostat-trade-export_quantity_tonnes"                                                                          
# "crop_use-faostat-trade-export_value_tonnes"                                                                             
# "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g"                                     
# "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal"                                          
# "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g"                                    
# "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g"                                 
# "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha"                                          
# "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us"                                  
# "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes"                                 
# "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes"                                          
# "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes"                                             
# "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes"                                          
# "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes"                                             
# "crop_use-public_interest-wikipedia_pageviews-taxon"                                                                     
#"crop_use-research_significance-google_scholar-taxon"                                                                    
# "crop_use-research_significance-pubmed_central-taxon"                                                                    
# "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions"   
# "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples"      
# "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty"
# "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty"                    
# "demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon"                       
# "demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon"                          
# "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene"                                    
# "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome"                                  
# "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide"                              
# "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein"




### WIEWS indicator metrics
## Number of accessions regenerated by location/institution not included
## ^^^ not calculated yet

library(readxl)
WIEWS_indicator_ourcrops <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/Indicator/WIEWS_indicator_ourcrops.xlsx")
View(WIEWS_indicator_ourcrops)

# rename as metrics file
WIEWS_indicator_metrics <- WIEWS_indicator_ourcrops










## join all together
library("dplyr") 
library("purrr") 

# Example list of datasets 
metrics_list <- list(combined_allcrops_sum, uniqueInstitionsCount, cwrPerc, 
                     weedyPerc, landracePerc, breedingmatPerc, improvedvarPerc, 
                     othervarPerc, nosampStatPerc,countryCount, isInPrimaryRegionPerc, 
                     internationalInst_Perc, notinternationalInst_Perc,
                     mlsI_Perc, mlsN_Perc, mlsY_internationalInst_perc, annex1_Perc, 
                     seed_Perc, field_Perc, invitro_Perc, cryo_Perc, dna_Perc, 
                     otherstorage_Perc, nostorage_Perc, longterm_storage_Perc,
                     medterm_storage_Perc, shortterm_storage_Perc, safetydupl_count, 
                     safetydupl_Perc, SGSV_dupl_count, #SGSV_dupl_perc, 
                     GLIS_dois_count, 
                     PTFTW_metrics, #PTFTW metrics dataset
                     WIEWS_indicator_metrics) # WIEWS indicator metrics dataset


# Function to perform left joins 
join_datasets <- function(df1, df2) { left_join(df1, df2, by = "cropStrategy") } 

# Use reduce function in purrr packages to join all datasets 
metrics_combined <- reduce(metrics_list, join_datasets)

# save file
library(writexl)
write_xlsx(metrics_combined, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/metrics_allcrops_2024_12_16.xlsx")





#resave storage metrics
metrics_list2 <- list(seed_Perc, field_Perc, invitro_Perc, cryo_Perc, dna_Perc, 
                      otherstorage_Perc, nostorage_Perc, longterm_storage_Perc,
                      medterm_storage_Perc, shortterm_storage_Perc)
metrics_storage <- reduce(metrics_list2, join_datasets)
write_xlsx(metrics_storage, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/metrics_storage_redo.xlsx")











# Save whatever metrics calculations are ready for now
# later check everything, re-run and re-combined etc
combined_metrics5 <- combined_allcrops_sum %>% 
  left_join(uniqueInstitionsCount, by = "cropStrategy") %>% 
  left_join(cwrPerc, by = "cropStrategy") %>% 
  left_join(unique_taxa, by = "cropStrategy") %>%
  left_join(improvedvarPerc, by = "cropStrategy") %>% 
  left_join(othervarPerc, by = "cropStrategy") %>% 
  left_join(countryCount, by = "cropStrategy") %>%
  left_join(internationalInst_Perc, by = "cropStrategy") %>%
  left_join(notinternationalInst_Perc, by = "cropStrategy") %>%
  left_join(mlsI_Perc, by = "cropStrategy") %>%
  left_join(mlsN_Perc, by = "cropStrategy") %>%
  left_join(seed_Perc, by = "cropStrategy") %>%
  left_join(field_Perc, by = "cropStrategy") %>%
  left_join(invitro_Perc, by = "cropStrategy") %>%
  left_join(cryo_Perc, by = "cropStrategy") %>%
  left_join(dna_Perc, by = "cropStrategy") %>%
  left_join(otherstorage_Perc, by = "cropStrategy") %>%
  left_join(shortterm_storage_Perc, by = "cropStrategy") %>%
  left_join(medterm_storage_Perc, by = "cropStrategy") %>%
  left_join(longterm_storage_Perc, by = "cropStrategy") %>%
  left_join(safetydupl_Perc, by = "cropStrategy") %>%
  left_join(SGSV_dupl, by = "cropStrategy")

View(combined_metrics5)

# save combined metrics file
write_xlsx(combined_metrics5, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/prioritymetrics_2024_11_21.xlsx")














## Data exploration Notes:

# Subset the data and get distinct names from instName and instCode columns 

distinct_internationalinstnames2 <- combined_allcrops %>% 
  filter(internationalStatus == "Y") %>% 
  select(instName, instCode) %>% distinct

distinct_notinternationalinstnames2 <- combined_allcrops %>% 
  filter(internationalStatus == "N" | is.na(internationalStatus) ) %>% 
  select(instName, instCode) %>% distinct


distinct_notinternationalinstnames <- distinct_notinternationalinstnames %>% filter(!is.na(instName))

View(distinct_notinternationalinstnames)
write_xlsx(distinct_notinternationalinstnames2, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/distinct_NOTinternational_inst_list2.xlsx")

write_xlsx(distinct_internationalinstnames2, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/distinct_international_inst_list2.xlsx")



# filter for just Aroids

internationalinst_aroids <- combined_allcrops %>% 
  filter(cropStrategy == "Aroids" & internationalStatus == "Y") %>% 
  select(instName, instCode) %>% distinct()



institution_counts <- combined_allcrops %>% 
  filter(cropStrategy == "Aroids") %>% 
  group_by(instName, instCode) %>% 
  summarise(record_count = n(), .groups = 'drop') %>% 
  distinct()
write_xlsx(institution_counts, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/distinct_institutions_Aroids.xlsx")



# Filter rows where instName contains "CePaCT" 
cepact_rows <- combined_allcrops %>% 
  filter(instName == "CePaCT") 
# View the result 
View(cepact_rows)




