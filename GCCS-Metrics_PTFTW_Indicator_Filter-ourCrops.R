### Project: Global Crop Conservation Strategies Metrics ###
### Data sources cleaning individually and Join 
### GCCS-Metrics_PTFTW_Indicator.R
### by Sarah Gora
### Date created: 2024_11_27


#### Set working directory ####
setwd()
setwd("C:/Users/sgora/Desktop/GCCS-Metrics/Code/DataCleaning_and_Join/GCCS-Metrics_PTFTW_Indicator.R")


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



## Data read in:
## Plants That Feed the World dataset
## This is the RE-RUN USING THE indicator_averages FILE FROM THE DRIVE
library(readr)
PTFTW_indicator_average <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/PlantsThatFeedTheWorld/indicator_average.csv")
View(PTFTW_indicator_average)

# replace "Rice (Asian)" in crop field to be "Rice"
PTFTW_indicator_average['crop'][PTFTW_indicator_average['crop'] == "Rice (Asian)"] <- "Rice"
# replace "Chickpeas" in crop field to be "Chickpea"
PTFTW_indicator_average['crop'][PTFTW_indicator_average['crop'] == "Chickpeas"] <- "Chickpea"

# filter by our crops first
library(readxl)
croplist <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GCCS-Metrics_croplist.xlsx")
View(croplist)

# Subset relevant columns and drop rows with NA in PlantsthatFeedtheWorld_name 
PlantsthatFeedtheWorld_ourcrops <- croplist %>% 
  select(PlantsthatFeedtheWorld_name, CropStrategy, Genera_primary, Taxa_main) %>% 
  filter(PlantsthatFeedtheWorld_name != "NA")

# rename crop field in Plants That Feed the World dataset to PlantsthatFeedtheWorld_name
PTFTW_indicator_average <- PTFTW_indicator_average %>% 
  rename(PlantsthatFeedtheWorld_name = crop)

## join PlantsthatFeedtheWorld_ourcrops to PlantsThatFeedTheWorld_rawdata
PTFTW_indicator_average <- PlantsthatFeedtheWorld_ourcrops %>% 
  left_join(PTFTW_indicator_average, by = "PlantsthatFeedtheWorld_name")



## select which relevant fields to keep for the indicator file for our summaries and metrics

PlantsThatFeedTheWorld_indicator_relevantfields <- subset(PTFTW_indicator_average, 
                                                          select = c( "PlantsthatFeedtheWorld_name",
                                                                      "CropStrategy",
                                                                      "Genera_primary",
                                                                      "Taxa_main", 
                                                                      "crop_use-faostat-food_supply-fat_supply_quantity_g",
                                                                      "crop_use-faostat-food_supply-food_supply_kcal",
                                                                      "crop_use-faostat-food_supply-food_supply_quantity_g",
                                                                      "crop_use-faostat-food_supply-protein_supply_quantity_g",
                                                                      "crop_use-faostat-production-area_harvested_ha",
                                                                      "crop_use-faostat-production-gross_production_value_us",
                                                                      "crop_use-faostat-production-production_quantity_tonnes",
                                                                      "crop_use-faostat-trade-export_quantity_tonnes",
                                                                      "crop_use-faostat-trade-export_value_tonnes",
                                                                      "crop_use-faostat_count_countries-count_countries_food_supply-fat_supply_quantity_g",
                                                                      "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_kcal",
                                                                      "crop_use-faostat_count_countries-count_countries_food_supply-food_supply_quantity_g",
                                                                      "crop_use-faostat_count_countries-count_countries_food_supply-protein_supply_quantity_g",
                                                                      "crop_use-faostat_count_countries-count_countries_production-area_harvested_ha",
                                                                      "crop_use-faostat_count_countries-count_countries_production-gross_production_value_us",
                                                                      "crop_use-faostat_count_countries-count_countries_production-production_quantity_tonnes",
                                                                      "crop_use-faostat_count_countries-count_countries_trade-export_quantity_tonnes",
                                                                      "crop_use-faostat_count_countries-count_countries_trade-export_value_tonnes",
                                                                      "crop_use-faostat_count_countries-count_countries_trade-import_quantity_tonnes",
                                                                      "crop_use-faostat_count_countries-count_countries_trade-import_value_tonnes",
                                                                      "crop_use-public_interest-wikipedia_pageviews-taxon",
                                                                      "crop_use-research_significance-google_scholar-taxon",
                                                                      "crop_use-research_significance-pubmed_central-taxon",
                                                                      "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_accessions",
                                                                      "demand-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews-genebank_distributions_fao_wiews_samples",
                                                                      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-count_of_countries_recipients_distributions_treaty",
                                                                      "demand-germplasm_distributions_treaty-germplasm_distributions_treaty-germplasm_distributions_treaty",
                                                                      "demand-varietal_registrations_upov-varietal_registrations_upov-varietal_registrations_upov_taxon",
                                                                      "demand-varietal_release_fao_wiews-varietal_release_fao_wiews-varietal_release_fao_wiews_taxon",
                                                                      "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_gene",
                                                                      "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_genome",
                                                                      "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_nucleotide",
                                                                      "supply-digital_sequence_supply-digital_sequence_supply-digital_sequence_supply_protein" ))                         

# save file as Indicator file
library(writexl)
write_xlsx(PlantsThatFeedTheWorld_indicator_relevantfields, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/PlantsThatFeedTheWorld/PTFTW_indicator_avg.xlsx")





