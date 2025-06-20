### Project: Global Crop Conservation Strategies Metrics ###
### Data sources cleaning individually and Join 
### GCCS-Metrics_WIEWS_Indicator_Filter-ourCrops
### by Sarah Gora
### Date updated: 2025_06_16


#### Install packages ####
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)

## Data read in: Wiews indicator 22 file 
WIEWS_Indicator22 <- read_csv("C:/Users/sarah/Desktop/Wiews_Indicator22_regeneration_allcrops.csv")

# Select relevant columns 
WIEWS_Indicator22 <- WIEWS_Indicator22 %>%
  select("Crop/crop group name",
         # "Country (ISO3)",  # use this for calc later
         # "Stakeholder (Instcode)", # use this for calc later
         "Total number of accessions in the national genebank(s)",
         "Number of accessions regenerated and/or multiplied",
         "Number of accessions in need of regeneration",
         "Number of accessions in need of regeneration without a budget for regeneration")

# Filter by our crops
croplist <- read_excel("C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/croplist_PG.xlsx")

# Write function to check for crop matches across multiple columns 
find_crop_strategy <- function(common_name, croplist) { match_row <- croplist %>% 
  filter(str_detect(CropStrategy, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(CommonName_primary, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(CommonName_synonym, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(Genera_primary, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(Genera_synonyms, fixed(common_name, ignore_case = TRUE)) | 
           str_detect(Taxa_main, fixed(common_name, ignore_case = TRUE))) 
if (nrow(match_row) > 0) { 
  return(match_row$CropStrategy[1]) 
} else { 
  return(NA) 
} 
}    

# Use function to check for crop and add CropStrategy to WIEWS_Indicator22 
WIEWS_Indicator22 <- WIEWS_Indicator22 %>% 
  mutate(cropStrategy = sapply(`Crop/crop group name`, find_crop_strategy, croplist = croplist))

# Keep rows where CropStrategy is filled out, ~ 313 rows 
WIEWS_Indicator22 <- WIEWS_Indicator22 %>% filter(!is.na(cropStrategy) & cropStrategy != "")
# sum data by crop (some rows have multiple entries per crop)
# combine name field as a list and separate by a semicolon: "Crop/crop group name"
# sum the numbers across crops
# removed irrelvant columns and renamed relevant columns to standardize
WIEWS_Indicator22_ourCrops <- WIEWS_Indicator22 %>% 
  group_by(cropStrategy) %>% 
  summarise("Crop/crop group name" = paste(unique(na.omit(`Crop/crop group name`)), collapse = "; "), 
            across(where(is.numeric), ~ ifelse(all(is.na(.x)), NA, sum(.x, na.rm = TRUE)))) %>%
  select(-"Crop/crop group name") %>%  #drop WIEWS crop name column
  select (-"Total number of accessions in the national genebank(s)") %>%  #drop column, have more up to date data on this
  rename( number_of_accessions_regenerated_and_or_multiplied = "Number of accessions regenerated and/or multiplied",
          number_of_accessions_in_need_of_regeneration= "Number of accessions in need of regeneration",
          number_of_accessions_in_need_of_regeneration_without_budget_for_regeneration= "Number of accessions in need of regeneration without a budget for regeneration") 

# save file as Indicator file
write_xlsx(WIEWS_Indicator22_ourCrops, "C:/Users/sarah/Desktop/ex_situ_PGRFA_metrics/data_SG/WIEWS_indicator_ourcrops_2025-06-16.xlsx")

