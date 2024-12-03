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

# filter by crop and data source 
combined_allcrops_sum2 <- combined_allcrops %>%
  group_by(data_source, cropStrategy) %>%
  summarise(count = n())




##### METRIC: Number of institutions holding crop germplasm ##############################
# filter by crop 
uniqueInstitionsCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(unique_instCount = n_distinct(instCode))




##### METRIC: Number of accessions of Crop Wild Relatives ##############################
# metric 1, count # of cwrs by code, 
# metric 2, count/ list cwr taxon names 
# filter by crop 
# sampStat = 100 
# need integer and %

## metric 1, count # of cwrs by code

# count- integer
cwrCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_100sampStat = sum(sampStat == 100, na.rm = TRUE))

# Count as a % of the whole 
cwrPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_100sampStat = sum(sampStat == 100, na.rm = TRUE), cwr_total_records = n() ) %>% 
  mutate( percent_100sampStat = round((count_100sampStat / cwr_total_records) * 100, 2) )


## metric 2, count/ list unique taxon 
unique_taxa <- combined_allcrops %>% 
  filter(isCWR == "Y") %>% 
  select(cropStrategy, acceptedName_TNRS) %>% 
  distinct() %>% # Get unique rows 
  group_by(cropStrategy) %>%
  summarise(unique_taxa = list(unique(acceptedName_TNRS)))

# Create the list of unique taxa and count them 
unique_taxa <- combined_allcrops %>% 
  filter(isCWR == "Y") %>% 
  select(cropStrategy, acceptedName_TNRS) %>% 
  distinct() %>% # Get unique rows 
  group_by(cropStrategy) %>% 
  summarise( unique_taxa = list(unique(acceptedName_TNRS)), unique_taxa_count = n_distinct(acceptedName_TNRS) )

# example list of taxa names 
desired_crop_strategy <- "Aroids" 
unique_taxa_for_strategy <- unique_taxa %>% filter(cropStrategy == desired_crop_strategy) 
# Extract the list of unique taxa 
taxa_list <- unique_taxa_for_strategy$unique_taxa[[1]] 
# View the list of unique taxa 
print(taxa_list)



##### METRIC: Number of accessions of Landraces ############################################
# filter by crop 
# sampStat = 300 
# need integer and %

# count- integer
# filter by crop
landraceCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_300sampStat = sum(sampStat == 300, na.rm = TRUE))

# Count as a % of the whole 
landracePerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_300sampStat = sum(sampStat == 300, na.rm = TRUE), landrace_total_records = n() ) %>% 
  mutate( percent_300sampStat = round((count_300sampStat / landrace_total_records) * 100, 2) )



##### METRIC: Number of accessions of Improved Varieties ######################################
# filter by crop 
# sampStat = 500 
# need integer and %

# count- integer
# filter by crop
improvedvarCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_500sampStat = sum(sampStat == 500, na.rm = TRUE))

### calculate % 
improvedvarPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_500sampStat = sum(sampStat == 500, na.rm = TRUE), improvedvar_total_records = n() ) %>% 
  mutate( percent_500sampStat = round((count_500sampStat / improvedvar_total_records) * 100, 2) )




##### METRIC: Number of accessions of Other Varieties ######################################
# filter by crop 
# sampStat = 999 
# need integer and %

# count- integer
# filter by crop
othervarCount <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(count_999sampStat = sum(sampStat == 999, na.rm = TRUE))

# calculate %
othervarPerc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( count_999sampStat = sum(sampStat == 500, na.rm = TRUE), othervar_total_records = n() ) %>% 
  mutate( percent_999sampStat = round((count_999sampStat / othervar_total_records) * 100, 2) )




##### METRIC: Number of countries where germplasm has been collected ########################

# data used: CWR , landraces, and other 
# (removed improved varieties from dataset used)
# sampStat field keep 100, 300, 999
combined_allcrops_impmatrmv <- subset(combined_allcrops, sampStat == "100" | sampStat == "300" | sampStat == "999")

# count- integer
# filter by crop
countryCount <- combined_allcrops_impmatrmv %>% 
  group_by(cropStrategy) %>% 
  summarise(unique_countryCount = n_distinct(origCty))


### METRIC:  Number of accessions from primary region(s) of diversity
# use guide file







### METRIC:  What kind of institutions #####################################################
## categorize as international vs national 
## need integer and % 

## 2 metrics-
# number of international
# number of national/other


# number of institutions categorized as international 
# count- integer
internationalInst_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE))

## number of institutions categorized as not international or other 
# count- integer 
notinternationalInst_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(notinternationalInst_count = sum(mlsStat == "N" | is.na(mlsStat)))

# number of institutions categorized as international 
# calculate %
internationalInst_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(internationalIns_count = sum(internationalStatus == "Y", na.rm = TRUE), internationalInst_total_records = n() ) %>% 
  mutate( internationalInst_Perc = round((internationalIns_count / internationalInst_total_records) * 100, 2) )

# number of institutions categorized as not international 
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
  summarise( internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE), mlsStat_count = sum(mlsStat == "Y", na.rm = TRUE))

# calculate % of accessions in the MLS held by international collections
mlsY_internationalInst_perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise( internationalInst_count = sum(internationalStatus == "Y", na.rm = TRUE), mlsStat_count = sum(mlsStat == "Y", na.rm = TRUE), 
             mlsY_internationalInst_count = sum(mlsStat == "Y" & internationalStatus == "Y", na.rm = TRUE), 
             percentage_mlsY_internationalInst = (mlsY_internationalInst_count / mlsStat_count) * 100 )




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
  summarise(seed_storage_count = sum(storage == "10" | storage == "11" | storage == "12" | storage == "13", na.rm = TRUE))

# calculate %
seed_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(seed_storage_count = sum(storage == "10" | storage == "11" | storage == "12" | storage == "13", na.rm = TRUE), seedstorage_total_records = n()) %>% 
  mutate( seed_storage_Perc = round((seed_storage_count / seedstorage_total_records) * 100, 2) )



## number of accessions from field collections 
field_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(field_col_count = sum(storage == "20", na.rm = TRUE))

field_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(field_storage_count = sum(storage == "20", na.rm = TRUE), fieldstorage_total_records = n()) %>% 
  mutate( field_storage_Perc = round((field_storage_count / fieldstorage_total_records) * 100, 2) )

## number of accessions in in-vitro collections 
invitro_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(invitro_count = sum(storage == "30", na.rm = TRUE))

invitro_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(invitro_storage_count = sum(storage == "30", na.rm = TRUE), invitrostorage_total_records = n()) %>% 
  mutate( invitro_storage_Perc = round((invitro_storage_count / invitrostorage_total_records) * 100, 2) )

## number of accessions in cryo collections 
cryo_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(cryo_col_count = sum(storage == "40", na.rm = TRUE))

cryo_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(cryo_storage_count = sum(storage == "40", na.rm = TRUE), cryostorage_total_records = n()) %>% 
  mutate( cryo_storage_Perc = round((cryo_storage_count / cryostorage_total_records) * 100, 2) )

## number of accessions in DNA collections 
dna_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(dna_col_count = sum(storage == "50", na.rm = TRUE))

dna_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(dna_storage_count = sum(storage == "50", na.rm = TRUE), dna_storage_total_records = n()) %>% 
  mutate( dna_storage_Perc = round((dna_storage_count / dna_storage_total_records) * 100, 2) )


## number of accessions in other collections 
other_col_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(other_col_count = sum(storage == "99", na.rm = TRUE))

otherstorage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(other_storage_count = sum(storage == "99", na.rm = TRUE), other_storage_total_records = n()) %>% 
  mutate( other_storage_Perc = round((other_storage_count / other_storage_total_records) * 100, 2) )




### METRIC: Number of accessions held in Long-term storage 
# storage = 13
## number of accessions in other collections 
longterm_storage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(longterm_storage_count = sum(storage == "13", na.rm = TRUE))

longterm_storage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(longterm_storage_count = sum(storage == "13", na.rm = TRUE), longterm_storage_total_records = n()) %>% 
  mutate( longterm_storage_Perc = round((longterm_storage_count / longterm_storage_total_records) * 100, 2) )


### METRIC: Number of accessions held in Medium-term storage 
## storage = 12
medterm_storage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(medterm_storage_count = sum(storage == "12", na.rm = TRUE))

medterm_storage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(medterm_storage_count = sum(storage == "12", na.rm = TRUE), medterm_storage_total_records = n()) %>% 
  mutate( medterm_storage_Perc = round((medterm_storage_count / medterm_storage_total_records) * 100, 2) )


### METRIC: Number of accessions held in Short-term storage 
## storage = 11
shortterm_storage_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(shortterm_storage_count = sum(storage == "13", na.rm = TRUE))

shortterm_storage_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(shortterm_storage_count = sum(storage == "13", na.rm = TRUE), shortterm_storage_total_records = n()) %>% 
  mutate( shortterm_storage_Perc = round((shortterm_storage_count / shortterm_storage_total_records) * 100, 2) )


### METRIC: Number of accessions safety duplicated ########################################
# count everything that has a value in it, everything but the NAs
safetydupl_count <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(safDuplSite_count = sum(!is.na(duplSite)))

safetydupl_Perc <- combined_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(safDuplSite_count = sum(!is.na(duplSite)), safDuplSite_total_records = n()) %>% 
  mutate( safDuplSite_Perc = round((safDuplSite_count / safDuplSite_total_records) * 100, 2) )



### METRIC: Number of accessions safety duplicated in SGSV ##################################

### use SGSV dataset
View(SGSV_allcrops)

SGSV_dupl <- SGSV_allcrops %>%
  group_by(cropStrategy) %>%
  summarise(SGSVcount = n())

SGSV_dupl_Perc <- SGSV_allcrops %>% 
  group_by(cropStrategy) %>% 
  summarise(safDuplSite_count = sum(!is.na(duplSite)), safDuplSite_total_records = n()) %>% 
  mutate( safDuplSite_Perc = round((safDuplSite_count / safDuplSite_total_records) * 100, 2) )


## METRIC : Number of accessions from primary region(s) of diversity


## make a new column, isinPrimaryRegion = Y/N
## subset data from combined dataset 
## only include data with crops and CWRS (do not include info from landraces and Other)
## calculate based on country of origin (origCty) field and primary region field (from guidefile)











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




