### Project: Global Crop Conservation Strategies Metrics ###
### Data Read in: Join all species for all data sources individually
### GCCS-Metrics_DataReadIn.R
### by Sarah Gora
### Date created: 2024_11_06


#### Set working directory ####
setwd()
setwd("C:/Users/sgora/Desktop/GCCS-Metrics/Code/DataCleaning_and_Join/GCCS-Metrics_DataReadIn.R")

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
install.packages("purrr")
library(purrr)


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


#### Read in all database data for all crops ####


### (1). BGCI Plant Search data
library(purrr)
library(readr)
path <- "C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/FullReport_counts_and_origin"
BGCI_allcrops_unformatted <- list.files(
  path = path,
  pattern = "*.csv",
  full.names = TRUE,
  recursive = TRUE
) %>%
  set_names() %>%
  map_dfr(read_csv, .id = "Source")
View(BGCI_allcrops_unformatted)

# save raw unformatted data to Excel file
install.packages('writexl')
library(writexl)
write_xlsx(BGCI_allcrops_unformatted, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/BGCI_allcrops_unformatted.xlsx")




### (2) FAO WIEWS data, all data
## Load WIEWS data (all genera data included in this extraction from Stefano)
WIEWS_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/SDBGExtractRequest/WIEWS_allcrops_unformatted.csv")




### (3). GBIF data, all crops

## didnt work at first, had to run again
path <- "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/Living records"
GBIF_allcrops_unformatted <- list.files(
  path = path,
  pattern = "*.xlsx",
  full.names = TRUE,
  recursive = TRUE
) %>%
  set_names() %>%
  map_dfr(read_excel, .id = "Source")

# save raw unformatted data to csv file, had data loss when saved to an excel file
install.packages("writecsv")
library(writecsv)
write_csv(GBIF_allcrops_unformatted, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/GBIF_allcrops_unformatted.csv")





### (4). Genesys PGR data for all crops
path <- "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys"
Genesys_allcrops_unformatted <- list.files(
  path = path,
  pattern = "*.xlsx",
  full.names = TRUE,
  recursive = TRUE
) %>%
  set_names() %>%
  map_dfr(read_excel, .id = "Source")
View(Genesys_allcrops_unformatted)

#save all crops Genesys data, unformatted 
write_xlsx(Genesys_allcrops_unformatted, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.xlsx")
# could not save as an excel file because it had over 1M rows, so had to save as a csv instead
write.csv(Genesys_allcrops_unformatted, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")




### (5). SGSV data for all crops
path <- "C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/Deposits"
SGSV_allcrops_unformatted <- list.files(
  path = path,
  pattern = "*.xlsx",
  full.names = TRUE,
  recursive = TRUE
) %>%
  set_names() %>%
  map_dfr(read_excel, .id = "Source")
View(SGSV_allcrops_unformatted)

#save all crops SGSV data, unformatted 
write_xlsx(SGSV_allcrops_unformatted, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/SGSV_allcrops_unformatted.xlsx")



