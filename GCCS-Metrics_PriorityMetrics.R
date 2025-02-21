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
#View(combined_allcrops_sum)

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
View(cwrCount)

# count as a % of the whole
cwrPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_100sampStat = sum(sampStat >= 100 & sampStat < 200, na.rm = TRUE), cwr_total_records = n()) %>%  
  mutate(percent_100sampStat = round((count_100sampStat / cwr_total_records) * 100, 2))
View(cwrPerc)


## metric 2, count the number of unique taxa items (all taxon)
unique_taxa_items <- combined_allcrops %>% 
  select(cropStrategy, acceptedName_TNRS) %>% 
  distinct() %>% # Get unique rows 
  group_by(cropStrategy) %>% 
  summarise( unique_taxa = list(unique(acceptedName_TNRS)), 
             # List of unique taxa 
             unique_taxa_count = n_distinct(acceptedName_TNRS) 
             # Count the number of unique taxa 
             )
View(unique_taxa_items)




# view the unique taxa items and the data source it came from to look for errors
unique_taxa_list <- combined_allcrops %>% 
  select(cropStrategy, acceptedName_TNRS, data_source) %>% 
  distinct() %>% # Get unique rows 
  group_by(cropStrategy) %>% 
  summarise(unique_taxa_items = list(unique(acceptedName_TNRS)), unique_taxa_count = n_distinct(acceptedName_TNRS)) %>% 
  unnest(cols = c(unique_taxa_items)) # Unnest the list of unique taxa items 
View(unique_taxa_list)

library("openxlsx")
write.xlsx(unique_taxa_items, file = "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/metrics_unique_taxa_list.xlsx", rowNames = FALSE)






##### METRIC:  Number of taxa  (that matches GRIN taxa list)
# Load GRIN taxa list data
GRINtaxa_list_allcropscombined <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GRINtaxa_list_allcropscombined.xlsx")
View(GRINtaxa_list_allcropscombined)

# Filter the GRINtaxa_list_allcropscombined dataset to find matches in the combined_allcrops dataset 
matches <- GRINtaxa_list_allcropscombined %>% 
  filter(`Taxa Clean` %in% combined_allcrops$acceptedName_TNRS) 

# Merge matches with combined_allcrops to get the cropStategy information 
merged_data <- merge(matches, combined_allcrops, by.x = "Taxa Clean", by.y = "acceptedName_TNRS") 

# Count the number of unique matches and group by cropStategy 
GRINmatch_count <- merged_data %>% 
  group_by(cropStrategy) %>% 
  summarise(GRINmatch_count = n_distinct(`Taxa Clean`))
View(GRINmatch_count)





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
View(weedyPerc)



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
View(landracePerc)



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
View(breedingmatPerc)



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
View(nosampStatPerc)



##### METRIC: Number of countries where germplasm has been collected ########################

# calculation has excluded sampStat rows of 400s, 500, 600 (improved varieties, etc)

# count- integer
# filter by crop
countryCount <- combined_allcrops %>% 
  filter(!(sampStat %in% c(400:499, 500, 600))) %>% 
  group_by(cropStrategy) %>% summarise(unique_countryCount = n_distinct(origCty))
View(countryCount)




### METRIC:  Number of accessions from primary region(s) of diversity
# filter by crop 
# isInPrimaryRegions = Y
# need integer and %

# count- integer
isInPrimaryRegionCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(isInPrimaryRegion_Y_count = sum(isInPrimaryRegions == "Y", na.rm = TRUE))
View(isInPrimaryRegionCount)


# calculate %
isInPrimaryRegionPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(isInPrimaryRegion_Y_count = sum(isInPrimaryRegions == "Y", na.rm = TRUE), primaryRegions_total_records = n() ) %>% 
  mutate( isInPrimaryRegion_Perc = round((isInPrimaryRegion_Y_count / primaryRegions_total_records) * 100, 2) )
View(isInPrimaryRegionPerc)





### METRIC:  Number of accessions from primary region(s) of diversity
# filter by crop 
# isInPrimaryandSecondaryRegions = Y
# need integer and %

# count- integer
isInPrimaryandSecondaryRegionsCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(isInPrimaryandSecondaryRegions_Y_count = sum(isInPrimaryandSecondaryRegions == "Y", na.rm = TRUE))
View(isInPrimaryandSecondaryRegionsCount)


# calculate %
isInPrimaryandSecondaryRegionsPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(isInPrimaryandSecondaryRegions_Y_count = sum(isInPrimaryandSecondaryRegions == "Y", na.rm = TRUE), primaryandSecondaryRegions_total_records = n() ) %>% 
  mutate( isInPrimaryandSecondaryRegions_Perc = round((isInPrimaryandSecondaryRegions_Y_count / primaryandSecondaryRegions_total_records) * 100, 2) )
View(isInPrimaryandSecondaryRegionsPerc)







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
View(internationalInst_count)


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
  summarise( internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE), 
             mlsStat_count = sum(mlsStat == "I", na.rm = TRUE), 
             mlsY_internationalInst_count = sum(mlsStat == "I" & internationalStatus == "Y", na.rm = TRUE), 
             total_mlsStat_count = n(), # Total number of rows including NA 
             percentage_mlsY_internationalInst = (mlsY_internationalInst_count / total_mlsStat_count) * 100)




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

SGSV_dupl_perc <- SGSV_dupl_count %>% 
  group_by(cropStrategy) %>%
  # Calculate the total count 
  mutate(total_count = sum(SGSVcount)) %>% 
  # Calculate the percentage of the whole 
   mutate(percentage = (SGSVcount / total_count) * 100)



# Calculate the total count of rows in combined_allcrops, grouped by cropStrategy 
total_count_combined <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(total_count = n()) 

# Join this total count with SGSV_dupl_count 
SGSV_dupl_perc <- SGSV_dupl_count %>% 
  left_join(total_count_combined, by = "cropStrategy") %>% 
  group_by(cropStrategy) %>% 
  mutate(percentage = (SGSVcount / total_count) * 100) 

View(SGSV_dupl_perc)





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
                                       "crop_use-faostat-food_supply-fat_supply_quantity_g",
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


# rename the column names to the names for Table 6
library(dplyr)
PTFTW_metrics <- PTFTW_metrics %>%
                  rename("Contribution to fat in global food supplies (g/capita/day) (average years 2015-2019)"= "crop_use-faostat-food_supply-fat_supply_quantity_g",
                         "Contribution to protein in global food supplies (g/capita/day) (average years 2015-2019)"= "crop_use-faostat-food_supply-protein_supply_quantity_g",
                         "Contribution to calories in global food supplies (kcal/capita/day) (average years 2015-2019)"="crop_use-faostat-food_supply-food_supply_kcal",
                         "Contribution to food weight in global food supplies (g/capita/day) (average years 2015-2019)"="crop_use-faostat-food_supply-food_supply_quantity_g",
                         "Harvested area worldwide (ha) (average years 2015-2019)"="crop_use-faostat-production-area_harvested_ha",
                         "Gross production value worldwide (current thousand USD) (average years 2015-2019)"="crop_use-faostat-production-gross_production_value_us",
                         "Total production worldwide (tonnes) (average years 2015-2019)"="crop_use-faostat-production-production_quantity_tonnes",
                         "Export quantity worldwide (tonnes) (average years 2015-2019)"="crop_use-faostat-trade-export_quantity_tonnes",
                         "Import quantity worldwide (tonnes) (average years 2015-2019)"="crop_use-faostat-trade-export_value_tonnes",
                         "Number of countries where significant contributor to fat in national food supply (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
                         "Number of countries where significant contributor to calories in national food supply (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal",
                         "Number of countries where significant contributor to food weight in national food supply (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
                         "Number of countries where significant contributor to protein in national food supply (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
                         "Number of countries where significant production occurs in terms of harvested area (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
                         "Number of countries where significant production occurs in terms of production value (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
                         "Number of countries where significant production occurs in terms of total production (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
                         "Number of countries where significant export occurs as measured by production quantity (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
                         "Number of countries where significant export occurs as measured by production value (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
                         "Number of countries where significant import occurs as measured by production quantity (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
                         "Number of countries where significant import occurs as measured by production value (average years 2015-2019)"="crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes",
                         "Number of public pageviews of taxon on Wikipedia during 2019"="crop_use-public_interest-wikipedia_pageviews-taxon",
                         "Number of publications listed in Google Scholar with taxon name in title published between 2009-2019"="crop_use-research_significance-google_scholar-taxon",
                         "Number of publications listed in PubMed Central with taxon name in text as of 2022"="crop_use-research_significance-pubmed_central-taxon",
                         "Number of accessions distributed worldwide as recorded in FAO WIEWS (average years 2014-2019)"="demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions",
                         "Number of samples distributed worldwide as recorded in FAO WIEWS (average years 2014-2019)"="demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples",
                         "Number of countries receiving germplasm as recorded in the Plant Treaty Data Store (average years 2015-2019)"="demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty",
                         "Number of samples distributed worldwide as recorded in the Plant Treaty Data Store (average years 2015-2019)"="demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty",
                         "Number of varietal registrations worldwide as recorded in UPOV's PLUTO Database (average years 2014-2018)"="demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon",
                         "Number of varietal releases worldwide as recorded in FAO WIEWS (average years 2015-2019)"="demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon",
                         "Number of genes as recorded in NCBI's Entrez database as of 2022"="supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene",
                         "Number of genomes as recorded in NCBI's Entrez database as of 2022"="supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome",
                         "Number of nucleotides as recorded in NCBI's Entrez database as of 2022"="supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide",
                         "Number of proteins as recorded in NCBI's Entrez database as of 2022"="supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein",
                         "Number of research materials as recorded in GBIF as of 2019"="supply-research_supply-research_supply_gbif-research_supply_gbif_taxon",
                         "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty"="demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty"
                                            )



# Transpose for Tables (Table 6) later 
PTFTW_metrics_transposed <- as.data.frame(t(PTFTW_metrics))

# Set the first row as the column names 
colnames(PTFTW_metrics_transposed) <- PTFTW_metrics_transposed[1, ] 
# Remove the first row since it's now the header 
PTFTW_metrics_transposed <- PTFTW_metrics_transposed[-1, ]

PTFTW_metrics_transposed_rch <- PTFTW_metrics_transposed %>% select(Rice, Chickpea) 
View(PTFTW_metrics_transposed)





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
                     safetydupl_Perc, SGSV_dupl_count, SGSV_dupl_perc, 
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









# Combine all metrics, 1/22/2025 SG

# Save whatever metrics calculations are ready for now
combined_metrics5 <- combined_allcrops_sum %>%  # table 1 
  left_join(unique_taxa_items, by = "cropStrategy") %>%
  left_join(GRINmatch_count, by = "cropStrategy") %>%
  left_join(uniqueInstitionsCount, by = "cropStrategy") %>% 
  left_join(cwrPerc, by = "cropStrategy") %>% 
  left_join(weedyPerc, by = "cropStrategy") %>%
  left_join(landracePerc, by = "cropStrategy") %>%
  left_join(breedingmatPerc, by = "cropStrategy") %>% 
  left_join(improvedvarPerc, by = "cropStrategy") %>% 
  left_join(othervarPerc, by = "cropStrategy") %>% 
  left_join(nosampStatPerc, by = "cropStrategy") %>%
  left_join(countryCount, by = "cropStrategy") %>%
  left_join(isInPrimaryRegionPerc, by = "cropStrategy") %>%
  left_join(isInPrimaryandSecondaryRegionsPerc, by = "cropStrategy") %>%
  left_join(internationalInst_Perc, by = "cropStrategy") %>%
  left_join(notinternationalInst_Perc, by = "cropStrategy") %>% 
  # table 2
  left_join(GLIS_dois_count, by = "cropStrategy") %>%                 
  left_join(mlsI_Perc, by = "cropStrategy") %>%
  left_join(mlsN_Perc, by = "cropStrategy") %>%
  left_join(mlsY_internationalInst_perc, by = "cropStrategy") %>%
  left_join(annex1_Perc, by = "cropStrategy") %>%
  # table 3
  left_join(seed_Perc, by = "cropStrategy") %>%
  left_join(field_Perc, by = "cropStrategy") %>%
  left_join(invitro_Perc, by = "cropStrategy") %>%
  left_join(cryo_Perc, by = "cropStrategy") %>%
  left_join(dna_Perc, by = "cropStrategy") %>%
  left_join(otherstorage_Perc, by = "cropStrategy") %>%
  left_join(nostorage_Perc, by = "cropStrategy") %>%
  left_join(shortterm_storage_Perc, by = "cropStrategy") %>%
  left_join(medterm_storage_Perc, by = "cropStrategy") %>%
  left_join(longterm_storage_Perc, by = "cropStrategy") %>%
  left_join(safetydupl_Perc, by = "cropStrategy") %>%
  left_join(SGSV_dupl_perc, by = "cropStrategy")
#View(combined_metrics5)

# List of datasets to combine (includes already calculated metrics)
metrics_list <- list(combined_metrics5, #metrics calculated from our dataset(s)
                     PTFTW_metrics, #PTFTW metrics dataset
                     WIEWS_indicator_metrics) # WIEWS indicator metrics dataset

# Function to perform left joins 
join_datasets <- function(df1, df2) { left_join(df1, df2, by = "cropStrategy") } 

# Use reduce function in purrr packages to join all datasets 
library(purrr)
metrics_combined <- reduce(metrics_list, join_datasets)


# Clean up dataset to just have the data we will use for Tables 
metrics_combined <- metrics_combined %>%
      select(-contains("total")) %>%     # delete all "total records" columns
      select(-unique_taxa, -mlsStat_count, -internationalInst_count) # delete extra fields we don't need 


# rename the fields to be the names for tables 
library(dplyr)
metrics_combined <- metrics_combined %>%
  rename("Crop"= "cropStrategy",
         "Total number of accessions in collections"= "cropcount",
         "Number of unique taxa items"="unique_taxa_count",  
         "Number of species/taxa in accessions"="GRINmatch_count",
         "Number of institutions holding crop germplasm"="unique_instCount",
         "Number of accessions of crop wild relatives (CWR)"="count_100sampStat",
         "Percent of accessions of crop wild relatives (CWR)"="percent_100sampStat",                                                                                             
         "Number of accessions of weedy"="count_200sampStat",                                                                                              
         "Percent of accessions of weedy"="percent_200sampStat",
         "Number of accessions of landraces"="count_300sampStat",                                                                                              
         "Percent of accessions of landraces"="percent_300sampStat",
         "Number of accessions of breeding materials"="count_400sampStat",                                                                                              
         "Percent of accessions of breeding materials"="percent_400sampStat",                                                                                             
         "Number of accessions of improved varieties"="count_500sampStat",                                                                                              
         "Percent of accesions of improved varieties"="percent_500sampStat", 
         "Number of accessions marked as 'other'"="count_999sampStat",
         "Percent of accessions marked as 'other'"="percent_999sampStat",
         "Number of accessions not marked with an improvement type"="count_NA_sampStat",
         "Percent of accessions not marked with an improvement type"="percent_NA_sampStat",
         "Number of countries where germplasm has been collected"="unique_countryCount",
         "Number of accessions from the primary region(s) of diversity"="isInPrimaryRegion_Y_count",
         "Percent of accessions from the primary region(s) of diversity"="isInPrimaryRegion_Perc",
         "Number of accessions from the primary and secondary region(s) of diversity"= "isInPrimaryandSecondaryRegions_Y_count",
         "Percent of accessions from the primary and secondary region(s) of diversity"= "isInPrimaryandSecondaryRegions_Perc",
         "Number of accessions in international institutions"="internationalIns_count",                                                                                            
         "Percent of accessions in international institutions"="internationalInst_Perc",                                                                                           
         "Number of accessions not in international institutions"="notinternationalInst_count",                                                                                        
         "Percent of accessions not in international institutions"="notinternationalInst_Perc",
         "Number of accessions included in the multilateral system (MLS)"="mlsIncluded_count",                                                                                                
         "Percent of accessions included in the multilateral system (MLS)"="mlsIncluded_Perc",                                                                                               
         "Number of accessions not included in the multilateral system (MLS)"="mlsNotIncluded_count",                                                                                             
         "Percent of accessions not included in the multilateral system (MLS)"="mlsNotIncluded_Perc",   
         "Number of accessions included in the multilateral system (MLS) that are in international collections"= "mlsY_internationalInst_count",
         "Percent of accessions included in the multilateral system (MLS) that are in international collections"="percentage_mlsY_internationalInst",
         "Number of accessions in Annex I"="annex1_count",
         "Percent of accessions in Annex I"="annex1_Perc",
         "Number of accessions held in seed storage"="seed_storage_count",                                                                                             
         "Percent of accessions held in seed storage"="seed_storage_Perc",                                                                                                
         "Number of accessions held in field storage"="field_storage_count",                                                                                            
         "Percent of accessions held in field storage"="field_storage_Perc",                                                                                               
         "Number of accessions held in in-vitro storage"="invitro_storage_count",                                                                                           
         "Percent of accessions held in in-vitro storage"="invitro_storage_Perc",                                                                                             
         "Number of accessions held in cryo storage"="cryo_storage_count",                                                                                              
         "Percent of accessions held in cryo storage"="cryo_storage_Perc",                                                                                                
         "Number of accessions held as DNA"="dna_storage_count",                                                                                               
         "Percent of accessions held as DNA"="dna_storage_Perc",                                                                                               
         "Number of accessions held in other storage"="other_storage_count",                                                                                              
         "Percent of accessions held in other storage"="other_storage_Perc",                                                                                                
         "Number of accessions held in short-term seed storage"="shortterm_storage_count",                                                                                       
         "Percent of accessions held in short-term seed storage"="shortterm_storage_Perc",                                                                                            
         "Number of accessions held in medium-term seed storage"="medterm_storage_count",                                                                                            
         "Percent of accessions held in medium-term seed storage"="medterm_storage_Perc",                                                                                           
         "Number of accessions held in long-term seed storage"="longterm_storage_count",                                                                                       
         "Percent of accessions held in long-term seed storage"="longterm_storage_Perc",
         "Number of accessions not marked with a storage type"="nostorage_count",
         "Percent of accessions not marked with a storage type"="nostorage_Perc",
         "Number of accessions safety duplicated"="safDuplSite_count",                                                                                                
         "Percent of accessions safety duplicated"="safDuplSite_Perc",                                                                                             
         "Number of accessions safety duplicated in Svalbard"="SGSVcount",                                                                                                     
         "Percent of accessions safety duplicated in Svalbard"="percentage",
         "Number of DOIs"="DOIs",
         "Number of accessions regenerated"="number_of_accessions_regenerated_and_or_multiplied",                                                              
         "Number of accessions in need of regeneration"="number_of_accessions_in_need_of_regeneration",                                                                     
         "Number of accessions in need of regeneration without budget for regeneration"="number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration",
         "Balance of recipient regions, gini index"= "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-gini_recipients_distributions_treaty"         
             )
# view(metrics_combined)



## Arrange columns in ORDER for ease of review and tables 
# Rearrange columns
metrics_combined <- metrics_combined %>%
  select("Crop",         
                   ## TABLE 1 ##
         "Total number of accessions in collections",                                                                      
         "Number of unique taxa items",                                                                                       
         "Number of species/taxa in accessions",                                                                           
         "Number of institutions holding crop germplasm",                                                                
         "Number of accessions of crop wild relatives (CWR)",                                                                
         "Percent of accessions of crop wild relatives (CWR)",                                                            
         "Number of accessions of weedy",                                                                                 
         "Percent of accessions of weedy",                                                                                    
         "Number of accessions of landraces",                                                                                 
         "Percent of accessions of landraces",                                                                                
         "Number of accessions of breeding materials",                                                                        
         "Percent of accessions of breeding materials",                                                                       
          "Number of accessions of improved varieties",                                                                        
         "Percent of accesions of improved varieties",                                                                       
         "Number of accessions marked as 'other'",                                                                            
         "Percent of accessions marked as 'other'",                                                                           
         "Number of accessions not marked with an improvement type",                                                          
         "Percent of accessions not marked with an improvement type",                                                         
         "Number of countries where germplasm has been collected",                                                            
         "Number of accessions from the primary region(s) of diversity",                                                      
         "Percent of accessions from the primary region(s) of diversity",                                                     
         "Number of accessions from the primary and secondary region(s) of diversity",                                        
         "Percent of accessions from the primary and secondary region(s) of diversity", 
         "Number of accessions in international institutions",                                                                
         "Percent of accessions in international institutions",                                                              
         "Number of accessions not in international institutions",                                                            
         "Percent of accessions not in international institutions",    
                              ## TABLE 2 ##
         "Number of DOIs",                                                                                                    
         "Number of accessions included in the multilateral system (MLS)",                                                    
         "Percent of accessions included in the multilateral system (MLS)",                                                   
         "Number of accessions not included in the multilateral system (MLS)",                                               
         "Percent of accessions not included in the multilateral system (MLS)",                                               
         "Number of accessions included in the multilateral system (MLS) that are in international collections",              
         "Percent of accessions included in the multilateral system (MLS) that are in international collections",             
         "Number of accessions in Annex I",                                                                                   
         "Percent of accessions in Annex I",
                              ## TABLE 3 ##       
         "Number of accessions held in seed storage",                                                                        
         "Percent of accessions held in seed storage",                                                                        
         "Number of accessions held in field storage",                                                                       
         "Percent of accessions held in field storage",                                                                       
         "Number of accessions held in in-vitro storage",                                                                     
         "Percent of accessions held in in-vitro storage",                                                                    
         "Number of accessions held in cryo storage",                                                                         
         "Percent of accessions held in cryo storage",                                                                        
         "Number of accessions held as DNA",                                                                                  
         "Percent of accessions held as DNA",                                                                                 
         "Number of accessions held in other storage",                                                                        
         "Percent of accessions held in other storage",                                                                      
         "Number of accessions not marked with a storage type",                                                               
         "Percent of accessions not marked with a storage type",                                                              
         "Number of accessions held in short-term seed storage",                                                              
         "Percent of accessions held in short-term seed storage",                                                            
         "Number of accessions held in medium-term seed storage",                                                             
         "Percent of accessions held in medium-term seed storage",                                                            
         "Number of accessions held in long-term seed storage",                                                               
         "Percent of accessions held in long-term seed storage",                                                             
         "Number of accessions safety duplicated",                                                                          
         "Percent of accessions safety duplicated",                                                                           
         "Number of accessions safety duplicated in Svalbard",                                                                
         "Percent of accessions safety duplicated in Svalbard", 
         "Number of accessions regenerated",                                                                                
         "Number of accessions in need of regeneration",                                                                    
         "Number of accessions in need of regeneration without budget for regeneration",
                               ## TABLE 4 ##
         "Number of genes as recorded in NCBI's Entrez database as of 2022",                                                 
         "Number of genomes as recorded in NCBI's Entrez database as of 2022",                                                
         "Number of nucleotides as recorded in NCBI's Entrez database as of 2022",                                            
         "Number of proteins as recorded in NCBI's Entrez database as of 2022",                                               
         "Number of research materials as recorded in GBIF as of 2019",  
                               ## TABLE 5 ##
         "Number of accessions distributed worldwide as recorded in FAO WIEWS (average years 2014-2019)",                     
         "Number of samples distributed worldwide as recorded in FAO WIEWS (average years 2014-2019)",                        
         "Number of countries receiving germplasm as recorded in the Plant Treaty Data Store (average years 2015-2019)",     
         "Number of samples distributed worldwide as recorded in the Plant Treaty Data Store (average years 2015-2019)",
         "Balance of recipient regions, gini index",
         "Number of varietal releases worldwide as recorded in FAO WIEWS (average years 2015-2019)",                         
         "Number of varietal registrations worldwide as recorded in UPOV's PLUTO Database (average years 2014-2018)",         
                               ## TABLE 6 ##
         "Contribution to fat in global food supplies (g/capita/day) (average years 2015-2019)",                              
         "Contribution to protein in global food supplies (g/capita/day) (average years 2015-2019)",                          
         "Contribution to calories in global food supplies (kcal/capita/day) (average years 2015-2019)",                      
         "Contribution to food weight in global food supplies (g/capita/day) (average years 2015-2019)",                     
         "Harvested area worldwide (ha) (average years 2015-2019)",                                                          
         "Gross production value worldwide (current thousand USD) (average years 2015-2019)",                                 
         "Export quantity worldwide (tonnes) (average years 2015-2019)",                                                    
         "Import quantity worldwide (tonnes) (average years 2015-2019)",                                                      
         "Number of countries where significant contributor to fat in national food supply (average years 2015-2019)",        
         "Number of countries where significant contributor to protein in national food supply (average years 2015-2019)",    
         "Number of countries where significant contributor to calories in national food supply (average years 2015-2019)",   
         "Number of countries where significant contributor to food weight in national food supply (average years 2015-2019)",
         "Number of countries where significant production occurs in terms of harvested area (average years 2015-2019)",      
         "Number of countries where significant production occurs in terms of production value (average years 2015-2019)",   
         "Number of countries where significant export occurs as measured by production quantity (average years 2015-2019)",  
         "Number of countries where significant export occurs as measured by production value (average years 2015-2019)",     
         "Number of countries where significant import occurs as measured by production quantity (average years 2015-2019)",  
         "Number of countries where significant import occurs as measured by production value (average years 2015-2019)",     
         "Number of public pageviews of taxon on Wikipedia during 2019",                                                      
         "Number of publications listed in Google Scholar with taxon name in title published between 2009-2019",              
         "Number of publications listed in PubMed Central with taxon name in text as of 2022"                                
                )


#### Transpose dataset of metrics for tables 
metrics_combined_transposed <- as.data.frame(t(metrics_combined))

# Set the first row as the column names 
colnames(metrics_combined_transposed) <- metrics_combined_transposed[1, ] 

# Remove the first row since it's now the header 
metrics_combined_transposed <- metrics_combined_transposed[-1, ]

# View columns as rows 
View(metrics_combined_transposed)





# make into tables with count and % set up side by side 
library(dplyr)

################# try out this 
# Assuming metrics_combined_transposed is your data frame and it already has row names

# List to store tables
tables_list <- list()

# Loop through each column and create a table with row names
for (col_name in names(metrics_combined_transposed)) {
  # Create a data frame with the original row names and the column data
  table_with_row_names <- data.frame(RowNames = rownames(metrics_combined_transposed),
                                     Data = metrics_combined_transposed[[col_name]])
  names(table_with_row_names)[2] <- col_name  # Rename the data column
  tables_list[[col_name]] <- table_with_row_names
}

# Print the tables
for (col_name in names(tables_list)) {
  print(paste("Table for column:", col_name))
  print(tables_list[[col_name]])
}

# View(tables_list)


# Add a column, Metric for row names and keep the original row names
metrics_combined_transposed <- metrics_combined_transposed %>%
  rownames_to_column(var = "Metric")


################# AROIDS data
# Select Aroids data, rename the Aroids column to Value, create an empty Percent column  
metrics_combined_transposed_Aroids <- metrics_combined_transposed %>% select(Metric, Aroids)
metrics_combined_transposed_Aroids <- metrics_combined_transposed_Aroids %>% rename(Value = Aroids)
metrics_combined_transposed_Aroids$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Aroids)) {
  if (grepl("Percent of", metrics_combined_transposed_Aroids$Metric[i])) {
    metrics_combined_transposed_Aroids$Percent[i-1] <- metrics_combined_transposed_Aroids$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Aroids <- metrics_combined_transposed_Aroids[!grepl("Percent of", metrics_combined_transposed_Aroids$Metric), ]

# Aroids, Tables:
Aroids_Table1 <- metrics_combined_transposed_Aroids[1:16, ]
Aroids_Table2 <- metrics_combined_transposed_Aroids[17:21, ]
Aroids_Table3 <- metrics_combined_transposed_Aroids[21:36, ]
Aroids_Table4 <- metrics_combined_transposed_Aroids[37:41, ]
Aroids_Table5 <- metrics_combined_transposed_Aroids[42:48, ]
Aroids_Table6 <- metrics_combined_transposed_Aroids[49:69, ]

# Save tables:
write_xlsx(Aroids_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Aroids_Table1.xlsx")
write_xlsx(Aroids_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Aroids_Table2.xlsx")
write_xlsx(Aroids_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Aroids_Table3.xlsx")
write_xlsx(Aroids_Table4, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Aroids_Table4.xlsx")
write_xlsx(Aroids_Table5, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Aroids_Table5.xlsx")
write_xlsx(Aroids_Table6, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Aroids_Table6.xlsx")



################# BARLEY data
# Select Barley data, rename the arley column to Value, create an empty Percent column  
metrics_combined_transposed_Barley <- metrics_combined_transposed %>% select(Metric, Barley)
metrics_combined_transposed_Barley <- metrics_combined_transposed_Barley %>% rename(Value = Barley)
metrics_combined_transposed_Barley$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Barley)) {
  if (grepl("Percent of", metrics_combined_transposed_Barley$Metric[i])) {
    metrics_combined_transposed_Barley$Percent[i-1] <- metrics_combined_transposed_Barley$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Barley <- metrics_combined_transposed_Barley[!grepl("Percent of", metrics_combined_transposed_Barley$Metric), ]

# Barley, Tables:
Barley_Table1 <- metrics_combined_transposed_Barley[1:16, ]
Barley_Table2 <- metrics_combined_transposed_Barley[17:21, ]
Barley_Table3 <- metrics_combined_transposed_Barley[21:36, ]
Barley_Table4 <- metrics_combined_transposed_Barley[37:41, ]
Barley_Table5 <- metrics_combined_transposed_Barley[42:48, ]
Barley_Table6 <- metrics_combined_transposed_Barley[49:69, ]

# save tables
write_xlsx(Barley_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Barley_Table1.xlsx")
write_xlsx(Barley_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Barley_Table2.xlsx")
write_xlsx(Barley_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Barley_Table3.xlsx")


################# BEANS data
# Select Beans data, rename the Beans column to Value, create an empty Percent column  
metrics_combined_transposed_Beans <- metrics_combined_transposed %>% select(Metric, Beans)
metrics_combined_transposed_Beans <- metrics_combined_transposed_Beans %>% rename(Value = Beans)
metrics_combined_transposed_Beans$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Beans)) {
  if (grepl("Percent of", metrics_combined_transposed_Beans$Metric[i])) {
    metrics_combined_transposed_Beans$Percent[i-1] <- metrics_combined_transposed_Beans$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Beans <- metrics_combined_transposed_Beans[!grepl("Percent of", metrics_combined_transposed_Beans$Metric), ]

# Beans, Tables:
Beans_Table1 <- metrics_combined_transposed_Beans[1:16, ]
Beans_Table2 <- metrics_combined_transposed_Beans[17:21, ]
Beans_Table3 <- metrics_combined_transposed_Beans[21:36, ]
Beans_Table4 <- metrics_combined_transposed_Beans[37:41, ]
Beans_Table5 <- metrics_combined_transposed_Beans[42:48, ]
Beans_Table6 <- metrics_combined_transposed_Beans[49:69, ]

# save tables
write_xlsx(Beans_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Beans_Table1.xlsx")
write_xlsx(Beans_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Beans_Table2.xlsx")
write_xlsx(Beans_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Beans_Table3.xlsx")




################# BREADFRUIT data
# Select Breadfruit data, rename the Breadfruit column to Value, create an empty Percent column  
metrics_combined_transposed_Breadfruit <- metrics_combined_transposed %>% select(Metric, Breadfruit)
metrics_combined_transposed_Breadfruit <- metrics_combined_transposed_Breadfruit %>% rename(Value = Breadfruit)
metrics_combined_transposed_Breadfruit$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Breadfruit)) {
  if (grepl("Percent of", metrics_combined_transposed_Breadfruit$Metric[i])) {
    metrics_combined_transposed_Breadfruit$Percent[i-1] <- metrics_combined_transposed_Breadfruit$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Breadfruit <- metrics_combined_transposed_Breadfruit[!grepl("Percent of", metrics_combined_transposed_Breadfruit$Metric), ]

# Breadfruit, Tables:
Breadfruit_Table1 <- metrics_combined_transposed_Breadfruit[1:16, ]
Breadfruit_Table2 <- metrics_combined_transposed_Breadfruit[17:21, ]
Breadfruit_Table3 <- metrics_combined_transposed_Breadfruit[21:36, ]
Breadfruit_Table4 <- metrics_combined_transposed_Breadfruit[37:41, ]
Breadfruit_Table5 <- metrics_combined_transposed_Breadfruit[42:48, ]
Breadfruit_Table6 <- metrics_combined_transposed_Breadfruit[49:69, ]

# save tables
write_xlsx(Breadfruit_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Breadfruit_Table1.xlsx")
write_xlsx(Breadfruit_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Breadfruit_Table2.xlsx")
write_xlsx(Breadfruit_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Breadfruit_Table3.xlsx")



################# CASSAVA data
# Select Cassava data, rename the Cassava column to Value, create an empty Percent column  
metrics_combined_transposed_Cassava <- metrics_combined_transposed %>% select(Metric, Cassava)
metrics_combined_transposed_Cassava <- metrics_combined_transposed_Cassava %>% rename(Value = Cassava)
metrics_combined_transposed_Cassava$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Cassava)) {
  if (grepl("Percent of", metrics_combined_transposed_Cassava$Metric[i])) {
    metrics_combined_transposed_Cassava$Percent[i-1] <- metrics_combined_transposed_Cassava$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Cassava <- metrics_combined_transposed_Cassava[!grepl("Percent of", metrics_combined_transposed_Cassava$Metric), ]

# Cassava, Tables:
Cassava_Table1 <- metrics_combined_transposed_Cassava[1:16, ]
Cassava_Table2 <- metrics_combined_transposed_Cassava[17:21, ]
Cassava_Table3 <- metrics_combined_transposed_Cassava[21:36, ]
Cassava_Table4 <- metrics_combined_transposed_Cassava[37:41, ]
Cassava_Table5 <- metrics_combined_transposed_Cassava[42:48, ]
Cassava_Table6 <- metrics_combined_transposed_Cassava[49:69, ]

# save tables
write_xlsx(Cassava_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Cassava_Table1.xlsx")
write_xlsx(Cassava_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Cassava_Table2.xlsx")
write_xlsx(Cassava_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Cassava_Table3.xlsx")


################# CHICKPEA data
# Select Chickpea data, rename the Chickpea column to Value, create an empty Percent column  
metrics_combined_transposed_Chickpea <- metrics_combined_transposed %>% select(Metric, Chickpea)
metrics_combined_transposed_Chickpea <- metrics_combined_transposed_Chickpea %>% rename(Value = Chickpea)
metrics_combined_transposed_Chickpea$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Chickpea)) {
  if (grepl("Percent of", metrics_combined_transposed_Chickpea$Metric[i])) {
    metrics_combined_transposed_Chickpea$Percent[i-1] <- metrics_combined_transposed_Chickpea$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Chickpea <- metrics_combined_transposed_Chickpea[!grepl("Percent of", metrics_combined_transposed_Chickpea$Metric), ]

# Chickpea, Tables:
Chickpea_Table1 <- metrics_combined_transposed_Chickpea[1:16, ]
Chickpea_Table2 <- metrics_combined_transposed_Chickpea[17:21, ]
Chickpea_Table3 <- metrics_combined_transposed_Chickpea[21:36, ]
Chickpea_Table4 <- metrics_combined_transposed_Chickpea[37:41, ]
Chickpea_Table5 <- metrics_combined_transposed_Chickpea[42:48, ]
Chickpea_Table6 <- metrics_combined_transposed_Chickpea[49:69, ]

# save tables
write_xlsx(Chickpea_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Chickpea_Table1.xlsx")
write_xlsx(Chickpea_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Chickpea_Table2.xlsx")
write_xlsx(Chickpea_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Chickpea_Table3.xlsx")



################# FABA BEAN data
# Select Fababean data, rename the Fababean column to Value, create an empty Percent column  
metrics_combined_transposed_Fababean <- metrics_combined_transposed %>% select(Metric, "Faba bean")
metrics_combined_transposed_Fababean <- metrics_combined_transposed_Fababean %>% rename(Value = "Faba bean")
metrics_combined_transposed_Fababean$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Fababean)) {
  if (grepl("Percent of", metrics_combined_transposed_Fababean$Metric[i])) {
    metrics_combined_transposed_Fababean$Percent[i-1] <- metrics_combined_transposed_Fababean$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Fababean <- metrics_combined_transposed_Fababean[!grepl("Percent of", metrics_combined_transposed_Fababean$Metric), ]

# Fababean, Tables:
Fababean_Table1 <- metrics_combined_transposed_Fababean[1:16, ]
Fababean_Table2 <- metrics_combined_transposed_Fababean[17:21, ]
Fababean_Table3 <- metrics_combined_transposed_Fababean[21:36, ]
Fababean_Table4 <- metrics_combined_transposed_Fababean[37:41, ]
Fababean_Table5 <- metrics_combined_transposed_Fababean[42:48, ]
Fababean_Table6 <- metrics_combined_transposed_Fababean[49:69, ]

# save tables
write_xlsx(Fababean_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Fababean_Table1.xlsx")
write_xlsx(Fababean_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Fababean_Table2.xlsx")
write_xlsx(Fababean_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Fababean_Table3.xlsx")


################# GRASSPEA data
# Select Grasspea data, rename the Grasspea column to Value, create an empty Percent column  
metrics_combined_transposed_Grasspea <- metrics_combined_transposed %>% select(Metric, Grasspea)
metrics_combined_transposed_Grasspea <- metrics_combined_transposed_Grasspea %>% rename(Value = Grasspea)
metrics_combined_transposed_Grasspea$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Grasspea)) {
  if (grepl("Percent of", metrics_combined_transposed_Grasspea$Metric[i])) {
    metrics_combined_transposed_Grasspea$Percent[i-1] <- metrics_combined_transposed_Grasspea$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Grasspea <- metrics_combined_transposed_Grasspea[!grepl("Percent of", metrics_combined_transposed_Grasspea$Metric), ]

# Grasspea, Tables:
Grasspea_Table1 <- metrics_combined_transposed_Grasspea[1:16, ]
Grasspea_Table2 <- metrics_combined_transposed_Grasspea[17:21, ]
Grasspea_Table3 <- metrics_combined_transposed_Grasspea[21:36, ]
Grasspea_Table4 <- metrics_combined_transposed_Grasspea[37:41, ]
Grasspea_Table5 <- metrics_combined_transposed_Grasspea[42:48, ]
Grasspea_Table6 <- metrics_combined_transposed_Grasspea[49:69, ]

# save tables
write_xlsx(Grasspea_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Grasspea_Table1.xlsx")
write_xlsx(Grasspea_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Grasspea_Table2.xlsx")
write_xlsx(Grasspea_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Grasspea_Table3.xlsx")



################# LENTIL data
# Select Lentil data, rename the Lentil column to Value, create an empty Percent column  
metrics_combined_transposed_Lentil <- metrics_combined_transposed %>% select(Metric, Lentil)
metrics_combined_transposed_Lentil <- metrics_combined_transposed_Lentil %>% rename(Value = Lentil)
metrics_combined_transposed_Lentil$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Lentil)) {
  if (grepl("Percent of", metrics_combined_transposed_Lentil$Metric[i])) {
    metrics_combined_transposed_Lentil$Percent[i-1] <- metrics_combined_transposed_Lentil$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Lentil <- metrics_combined_transposed_Lentil[!grepl("Percent of", metrics_combined_transposed_Lentil$Metric), ]

# Lentil, Tables:
Lentil_Table1 <- metrics_combined_transposed_Lentil[1:16, ]
Lentil_Table2 <- metrics_combined_transposed_Lentil[17:21, ]
Lentil_Table3 <- metrics_combined_transposed_Lentil[21:36, ]
Lentil_Table4 <- metrics_combined_transposed_Lentil[37:41, ]
Lentil_Table5 <- metrics_combined_transposed_Lentil[42:48, ]
Lentil_Table6 <- metrics_combined_transposed_Lentil[49:69, ]

# save tables
write_xlsx(Lentil_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Lentil_Table1.xlsx")
write_xlsx(Lentil_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Lentil_Table2.xlsx")
write_xlsx(Lentil_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Lentil_Table3.xlsx")



################# MAIZE data
# Select Maize data, rename the Maize column to Value, create an empty Percent column  
metrics_combined_transposed_Maize <- metrics_combined_transposed %>% select(Metric, Maize)
metrics_combined_transposed_Maize <- metrics_combined_transposed_Maize %>% rename(Value = Maize)
metrics_combined_transposed_Maize$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Maize)) {
  if (grepl("Percent of", metrics_combined_transposed_Maize$Metric[i])) {
    metrics_combined_transposed_Maize$Percent[i-1] <- metrics_combined_transposed_Maize$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Maize <- metrics_combined_transposed_Maize[!grepl("Percent of", metrics_combined_transposed_Maize$Metric), ]

# Maize, Tables:
Maize_Table1 <- metrics_combined_transposed_Maize[1:16, ]
Maize_Table2 <- metrics_combined_transposed_Maize[17:21, ]
Maize_Table3 <- metrics_combined_transposed_Maize[21:36, ]
Maize_Table4 <- metrics_combined_transposed_Maize[37:41, ]
Maize_Table5 <- metrics_combined_transposed_Maize[42:48, ]
Maize_Table6 <- metrics_combined_transposed_Maize[49:69, ]

# save tables
write_xlsx(Maize_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Maize_Table1.xlsx")
write_xlsx(Maize_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Maize_Table2.xlsx")
write_xlsx(Maize_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Maize_Table3.xlsx")


################# OAT data
# Select Oat data, rename the Oat column to Value, create an empty Percent column  
metrics_combined_transposed_Oat <- metrics_combined_transposed %>% select(Metric, Oat)
metrics_combined_transposed_Oat <- metrics_combined_transposed_Oat %>% rename(Value = Oat)
metrics_combined_transposed_Oat$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Oat)) {
  if (grepl("Percent of", metrics_combined_transposed_Oat$Metric[i])) {
    metrics_combined_transposed_Oat$Percent[i-1] <- metrics_combined_transposed_Oat$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Oat <- metrics_combined_transposed_Oat[!grepl("Percent of", metrics_combined_transposed_Oat$Metric), ]

# Oat, Tables:
Oat_Table1 <- metrics_combined_transposed_Oat[1:16, ]
Oat_Table2 <- metrics_combined_transposed_Oat[17:21, ]
Oat_Table3 <- metrics_combined_transposed_Oat[21:36, ]
Oat_Table4 <- metrics_combined_transposed_Oat[37:41, ]
Oat_Table5 <- metrics_combined_transposed_Oat[42:48, ]
Oat_Table6 <- metrics_combined_transposed_Oat[49:69, ]

# save tables
write_xlsx(Oat_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Oat_Table1.xlsx")
write_xlsx(Oat_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Oat_Table2.xlsx")
write_xlsx(Oat_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Oat_Table3.xlsx")


################# RICE data
# Select Rice data, rename the Rice column to Value, create an empty Percent column  
metrics_combined_transposed_Rice <- metrics_combined_transposed %>% select(Metric, Rice)
metrics_combined_transposed_Rice <- metrics_combined_transposed_Rice %>% rename(Value = Rice)
metrics_combined_transposed_Rice$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Rice)) {
  if (grepl("Percent of", metrics_combined_transposed_Rice$Metric[i])) {
    metrics_combined_transposed_Rice$Percent[i-1] <- metrics_combined_transposed_Rice$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Rice <- metrics_combined_transposed_Rice[!grepl("Percent of", metrics_combined_transposed_Rice$Metric), ]

# Rice, Tables:
Rice_Table1 <- metrics_combined_transposed_Rice[1:16, ]
Rice_Table2 <- metrics_combined_transposed_Rice[17:21, ]
Rice_Table3 <- metrics_combined_transposed_Rice[21:36, ]
Rice_Table4 <- metrics_combined_transposed_Rice[37:41, ]
Rice_Table5 <- metrics_combined_transposed_Rice[42:48, ]
Rice_Table6 <- metrics_combined_transposed_Rice[49:69, ]

# save tables
write_xlsx(Rice_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Rice_Table1.xlsx")
write_xlsx(Rice_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Rice_Table2.xlsx")
write_xlsx(Rice_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Rice_Table3.xlsx")



################# STRAWBERRY data
# Select Strawberry data, rename the Strawberry column to Value, create an empty Percent column  
metrics_combined_transposed_Strawberry <- metrics_combined_transposed %>% select(Metric, Strawberry)
metrics_combined_transposed_Strawberry <- metrics_combined_transposed_Strawberry %>% rename(Value = Strawberry)
metrics_combined_transposed_Strawberry$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Strawberry)) {
  if (grepl("Percent of", metrics_combined_transposed_Strawberry$Metric[i])) {
    metrics_combined_transposed_Strawberry$Percent[i-1] <- metrics_combined_transposed_Strawberry$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Strawberry <- metrics_combined_transposed_Strawberry[!grepl("Percent of", metrics_combined_transposed_Strawberry$Metric), ]

# Strawberry, Tables:
Strawberry_Table1 <- metrics_combined_transposed_Strawberry[1:16, ]
Strawberry_Table2 <- metrics_combined_transposed_Strawberry[17:21, ]
Strawberry_Table3 <- metrics_combined_transposed_Strawberry[21:36, ]
Strawberry_Table4 <- metrics_combined_transposed_Strawberry[37:41, ]
Strawberry_Table5 <- metrics_combined_transposed_Strawberry[42:48, ]
Strawberry_Table6 <- metrics_combined_transposed_Strawberry[49:69, ]

# save tables
write_xlsx(Strawberry_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Strawberry_Table1.xlsx")
write_xlsx(Strawberry_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Strawberry_Table2.xlsx")
write_xlsx(Strawberry_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Strawberry_Table3.xlsx")


################# SWEETPOTATO data
# Select Sweetpotato data, rename the Sweetpotato column to Value, create an empty Percent column  
metrics_combined_transposed_Sweetpotato <- metrics_combined_transposed %>% select(Metric, Sweetpotato)
metrics_combined_transposed_Sweetpotato <- metrics_combined_transposed_Sweetpotato %>% rename(Value = Sweetpotato)
metrics_combined_transposed_Sweetpotato$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Sweetpotato)) {
  if (grepl("Percent of", metrics_combined_transposed_Sweetpotato$Metric[i])) {
    metrics_combined_transposed_Sweetpotato$Percent[i-1] <- metrics_combined_transposed_Sweetpotato$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Sweetpotato <- metrics_combined_transposed_Sweetpotato[!grepl("Percent of", metrics_combined_transposed_Sweetpotato$Metric), ]

# Sweetpotato, Tables:
Sweetpotato_Table1 <- metrics_combined_transposed_Sweetpotato[1:16, ]
Sweetpotato_Table2 <- metrics_combined_transposed_Sweetpotato[17:21, ]
Sweetpotato_Table3 <- metrics_combined_transposed_Sweetpotato[21:36, ]
Sweetpotato_Table4 <- metrics_combined_transposed_Sweetpotato[37:41, ]
Sweetpotato_Table5 <- metrics_combined_transposed_Sweetpotato[42:48, ]
Sweetpotato_Table6 <- metrics_combined_transposed_Sweetpotato[49:69, ]

# save tables
write_xlsx(Sweetpotato_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Sweetpotato_Table1.xlsx")
write_xlsx(Sweetpotato_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Sweetpotato_Table2.xlsx")
write_xlsx(Sweetpotato_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Sweetpotato_Table3.xlsx")



################# TROPICAL AND SUBTROPICAL FORAGES data
# Select Tropical and Subtropical Forages data, rename the Tropical and Subtropical Forages column to Value, create an empty Percent column  
metrics_combined_transposed_Forages <- metrics_combined_transposed %>% select(Metric, `Tropical and subtropical forages`)
metrics_combined_transposed_Forages <- metrics_combined_transposed_Forages %>% rename(Value = `Tropical and subtropical forages`)
metrics_combined_transposed_Forages$Percent <- NA

# Iterate over the rows
for (i in 2:nrow(metrics_combined_transposed_Forages)) {
  if (grepl("Percent of", metrics_combined_transposed_Forages$Metric[i])) {
    metrics_combined_transposed_Forages$Percent[i-1] <- metrics_combined_transposed_Forages$Value[i]
  }
}
# Remove Percent rows
metrics_combined_transposed_Forages <- metrics_combined_transposed_Forages[!grepl("Percent of", metrics_combined_transposed_Forages$Metric), ]

# Tropical and Subtropical Forages, Tables:
Forages_Table1 <- metrics_combined_transposed_Forages[1:16, ]
Forages_Table2 <- metrics_combined_transposed_Forages[17:21, ]
Forages_Table3 <- metrics_combined_transposed_Forages[21:36, ]
Forages_Table4 <- metrics_combined_transposed_Forages[37:41, ]
Forages_Table5 <- metrics_combined_transposed_Forages[42:48, ]
Forages_Table6 <- metrics_combined_transposed_Forages[49:69, ]


# save tables
write_xlsx(Forages_Table1, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Forages_Table1.xlsx")
write_xlsx(Forages_Table2, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Forages_Table2.xlsx")
write_xlsx(Forages_Table3, "C:/Users/sgora/Desktop/GCCS-Metrics/Metrics/Results_Tables/Forages_Table3.xlsx")















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




