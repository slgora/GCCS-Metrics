
### Project: Global Crop Conservation Strategies Metrics ###
### Priority Metrics Calculations 
### Degree of Duplication
### GCCS-Metrics_PDCI_PGscript.R
### by Sarah Gora
### Date transformed: 1/31/2024


### Peter Giovannini Python script transformed to R script using Git copilot

# Load packages and libraries
library(dplyr)
library(tidyr)
library(readr)


# Peter example was using Genesys 
# load starting Genesys dataset 

# load starting Genesys dataset and re-name "df"
Genesys_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")
df <- Genesys_allcrops_unformatted


get_PDCI <- function(df) {
  # Initialize PDCI_genesys column with 0
  df$PDCI_genesys <- 0
  
  # Apply conditions and update PDCI_genesys
  df$PDCI_genesys <- ifelse(!is.na(df$GENUS), df$PDCI_genesys + 120, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !df$SPECIES %in% c('sp', 'sp.', 'spp.', 'spp'), df$PDCI_genesys + 80, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !is.na(df$SPAUTHOR), df$PDCI_genesys + 5, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !is.na(df$SUBTAXA), df$PDCI_genesys + 40, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$GENUS) & !is.na(df$SPECIES) & !is.na(df$SUBTAXA) & !is.na(df$SUBTAUTHOR), df$PDCI_genesys + 5, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$CROPNAME), df$PDCI_genesys + 45, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ACQDATE), df$PDCI_genesys + 10, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$SAMPSTAT), df$PDCI_genesys + 80, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DONORCODE), df$PDCI_genesys + 40, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DONORNAME) & is.na(df$DONORCODE), df$PDCI_genesys + 20, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DONORNUMB) & (!is.na(df$DONORCODE) | !is.na(df$DONORNAME)), df$PDCI_genesys + 40, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DONORNUMB) & is.na(df$DONORCODE) & is.na(df$DONORNAME), df$PDCI_genesys + 20, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$OTHERNUMB), df$PDCI_genesys + 35, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DUPLSITE), df$PDCI_genesys + 30, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$STORAGE), df$PDCI_genesys + 15, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ACCEURL), df$PDCI_genesys + 40, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$MLSSTAT), df$PDCI_genesys + 15, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$ORIGCTY) & df$SAMPSTAT %/% 100 < 4, df$PDCI_genesys + 80, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ORIGCTY) & (df$SAMPSTAT %/% 100 >= 4 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 40, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 < 3 & (is.na(df$DECLATITUDE) | is.na(df$DECLONGITUDE)), df$PDCI_genesys + 70, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 == 3 & (is.na(df$DECLATITUDE) | is.na(df$DECLONGITUDE)), df$PDCI_genesys + 45, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 < 3 & !is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE), df$PDCI_genesys + 20, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSITE) & df$SAMPSTAT %/% 100 == 3 & !is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE), df$PDCI_genesys + 15, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSITE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)) & !is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE), df$PDCI_genesys + 10, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSITE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)) & (is.na(df$DECLATITUDE) | is.na(df$DECLONGITUDE)), df$PDCI_genesys + 20, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE) & df$SAMPSTAT %/% 100 < 3, df$PDCI_genesys + 120, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 80, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$DECLATITUDE) & !is.na(df$DECLONGITUDE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 30, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$ELEVATION) & df$SAMPSTAT %/% 100 < 3, df$PDCI_genesys + 20, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ELEVATION) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 15, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ELEVATION) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 5, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$COLLDATE) & df$SAMPSTAT %/% 100 < 4, df$PDCI_genesys + 30, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLDATE) & (df$SAMPSTAT %/% 100 == 9 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 10, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$BREDCODE) & df$SAMPSTAT %/% 100 == 4, df$PDCI_genesys + 110, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$BREDCODE) & df$SAMPSTAT %/% 100 == 5, df$PDCI_genesys + 80, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$BREDCODE) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 10, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$ANCEST) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 10, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ANCEST) & df$SAMPSTAT %/% 100 == 4, df$PDCI_genesys + 150, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ANCEST) & df$SAMPSTAT %/% 100 == 5, df$PDCI_genesys + 100, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ANCEST) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 40, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 < 3, df$PDCI_genesys + 30, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 50, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 == 4, df$PDCI_genesys + 20, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSRC) & df$SAMPSTAT %/% 100 == 5, df$PDCI_genesys + 20, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLSRC) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 25, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$ACCENAME) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 50, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ACCENAME) & df$SAMPSTAT %/% 100 == 4, df$PDCI_genesys + 80, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ACCENAME) & df$SAMPSTAT %/% 100 == 5, df$PDCI_genesys + 160, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$ACCENAME) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 40, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$COLLNUMB) & df$SAMPSTAT %/% 100 < 3, df$PDCI_genesys + 60, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLNUMB) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 40, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLNUMB) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 20, df$PDCI_genesys)
  
  df$PDCI_genesys <- ifelse(!is.na(df$COLLCODE) & df$SAMPSTAT %/% 100 < 3, df$PDCI_genesys + 40, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLCODE) & df$SAMPSTAT %/% 100 == 3, df$PDCI_genesys + 30, df$PDCI_genesys)
  df$PDCI_genesys <- ifelse(!is.na(df$COLLCODE) & (df$SAMPSTAT %/% 100 > 5 | is.na(df$SAMPSTAT)), df$PDCI_genesys + 20, df$PDCI_genesys)
  
  # Normalize PDCI_genesys by dividing by 100
  df$PDCI_genesys <- df$PDCI_genesys / 100
  
  return(df)
}

df <- get_PDCI(df)
View(df)




##### Re-run code using GCCS-metrics data, combined_allcrops

# read in combined data set (this does not include SGSV)
combined_allcrops <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")

# combined dataset
View(combined_allcrops)


df <- combined_allcrops

df <- combined_allcrops %>%
  filter(data_source %in% c("Genesys", "WIEWS"))


# Convert sampStat to numeric
df$sampStat <- as.numeric(as.character(df$sampStat))


#PDCI function takes into account 28 columns (genesys):
# Column name synonyms:

# `GENUS` = genus
# `SPECIES`= species
- `SPAUTHOR` = spAuthor
- `SUBTAXA`=subTaxa
- `SUBTAUTHOR`= subTAuthor
# `CROPNAME`= cropName
- `ACQDATE`= acqDate
# `SAMPSTAT`= sampStat, 
- `DONORCODE`= donorCode
- `DONORNAME`= donorName
- `DONORNUMB`=  donorNumb
- `OTHERNUMB`= otherNumb
# `DUPLSITE` = duplSite
# `STORAGE` = storage
- `ACCEURL`= acceUrl
# `MLSSTAT`= mlsStat
# `ORIGCTY`= origCty
- `COLLSITE`= collSite
#`DECLATITUDE` = latitude
# `DECLONGITUDE`= longitude
- `ELEVATION` = elevation
- `COLLDATE`= collDate
- `BREDCODE`= bredCode
- `ANCEST`= ancest
- `COLLSRC`= collSrc
# `ACCENAME`= acceNumb
- `COLLNUMB`= collNumb
- `COLLCODE`= collCode


get_PDCI <- function(df) {
  # Initialize PDCI column with 0
  df$PDCI <- 0
  
  # Function to print debug information
  print_debug_info <- function(df, step) {
    cat(paste("After step:", step, "\n"))
    print(head(df[, c("genus", "species", "storage", "origCty", "sampStat", "acceUrl", "mlsStat", "ancest", "latitude", "longitude", "collDate", "bredCode", "PDCI")], 5))
  }
  
  # Print initial state of the data
  print_debug_info(df, "initial")
  
  # Apply conditions and update PDCI
  df$PDCI <- ifelse(!is.na(df$genus), df$PDCI + 120, df$PDCI)
  print_debug_info(df, "genus")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !df$species %in% c('sp', 'sp.', 'spp.', 'spp'), df$PDCI + 80, df$PDCI)
  print_debug_info(df, "genus and species")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !is.na(df$spAuthor), df$PDCI + 5, df$PDCI)
  print_debug_info(df, "species author")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !is.na(df$subTaxa), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "subTaxa")
  
  df$PDCI <- ifelse(!is.na(df$genus) & !is.na(df$species) & !is.na(df$subTaxa) & !is.na(df$subTAuthor), df$PDCI + 5, df$PDCI)
  print_debug_info(df, "subTaxa author")
  
  df$PDCI <- ifelse(!is.na(df$cropName), df$PDCI + 45, df$PDCI)
  print_debug_info(df, "cropName")
  
  df$PDCI <- ifelse(!is.na(df$acqDate), df$PDCI + 10, df$PDCI)
  print_debug_info(df, "acqDate")
  
  df$PDCI <- ifelse(!is.na(df$sampStat), df$PDCI + 80, df$PDCI)
  print_debug_info(df, "sampStat")
  
  df$PDCI <- ifelse(!is.na(df$donorCode), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "donorCode")
  
  df$PDCI <- ifelse(!is.na(df$donorName) & is.na(df$donorCode), df$PDCI + 20, df$PDCI)
  print_debug_info(df, "donorName without donorCode")
  
  df$PDCI <- ifelse(!is.na(df$donorNumb) & (!is.na(df$donorCode) | !is.na(df$donorName)), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "donorNumb with donorCode or donorName")
  
  df$PDCI <- ifelse(!is.na(df$donorNumb) & is.na(df$donorCode) & is.na(df$donorName), df$PDCI + 20, df$PDCI)
  print_debug_info(df, "donorNumb without donorCode or donorName")
  
  df$PDCI <- ifelse(!is.na(df$otherNumb), df$PDCI + 35, df$PDCI)
  print_debug_info(df, "otherNumb")
  
  df$PDCI <- ifelse(!is.na(df$duplSite), df$PDCI + 30, df$PDCI)
  print_debug_info(df, "duplSite")
  
  df$PDCI <- ifelse(!is.na(df$storage), df$PDCI + 15, df$PDCI)
  print_debug_info(df, "storage")
  
  df$PDCI <- ifelse(!is.na(df$acceUrl), df$PDCI + 40, df$PDCI)
  print_debug_info(df, "acceUrl")
  
  df$PDCI <- ifelse(!is.na(df$mlsStat), df$PDCI + 15, df$PDCI)
  print_debug_info(df, "mlsStat")
  
  # Handle origCty and sampStat conditions, considering NA values for sampStat
  df$PDCI <- ifelse(!is.na(df$origCty) & (is.na(df$sampStat) | df$sampStat %/% 100 < 4), df$PDCI + 80, df$PDCI)
  print_debug_info(df, "origCty with sampStat < 4")
  
  df$PDCI <- ifelse(!is.na(df$origCty) & !is.na(df$sampStat) & df$sampStat %/% 100 >= 4, df$PDCI + 40, df$PDCI)
  print_debug_info(df, "origCty with sampStat >= 4")
  
  # Check latitude and longitude conditions
  df$PDCI <- ifelse(!is.na(df$latitude) & !is.na(df$longitude), df$PDCI + 120, df$PDCI)
  print_debug_info(df, "latitude and longitude")
  
  # Check elevation condition
  df$PDCI <- ifelse(!is.na(df$elevation), df$PDCI + 20, df$PDCI)
  print_debug_info(df, "elevation")
  
  # Check collection date condition
  df$PDCI <- ifelse(!is.na(df$collDate), df$PDCI + 30, df$PDCI)
  print_debug_info(df, "collDate")
  
  # Check breeding code condition
  df$PDCI <- ifelse(!is.na(df$bredCode), df$PDCI + 110, df$PDCI)
  print_debug_info(df, "bredCode")
  
  # Check ancestry condition
  df$PDCI <- ifelse(!is.na(df$ancest), df$PDCI + 150, df$PDCI)
  print_debug_info(df, "ancest")
  
  # Normalize PDCI by dividing by 100 
  df$PDCI <- df$PDCI / 100 
  
  # Ensure PDCI is between 0 and 10
  df$PDCI[is.na(df$PDCI)] <- 0
  df$PDCI <- pmin(pmax(df$PDCI, 0), 10)
  
  return(df) 
} 

# Example usage
df <- get_PDCI(df)
View(df)

 

# Checks:
# Filter rows where PDCI is equal to 0
pdci_zero_rows <- df %>% filter(PDCI == 0)

# Print the rows with PDCI equal to 0, should be zero rows
View(pdci_zero_rows)



# Group by cropStrategy and calculate the mean, median, mode PDCI
# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Group by cropStrategy and calculate the mean, median, mode, and range PDCI
summary_pdci <- df %>%
  group_by(cropStrategy) %>%
  summarise(
    mean_PDCI = mean(PDCI, na.rm = TRUE),
    median_PDCI = median(PDCI, na.rm = TRUE),
    mode_PDCI = get_mode(PDCI),
    range_PDCI = paste(min(PDCI, na.rm = TRUE), max(PDCI, na.rm = TRUE), sep = " - ")
  )

View(summary_pdci)



### Actual metric calulation we want: 
### PG suggested to use median 

# Group by cropStrategy and calculate the median PDCI
summary_pdci <- df %>%
  group_by(cropStrategy) %>%
  summarise(
    median_PDCI = median(PDCI, na.rm = TRUE)
  )

View(summary_pdci)

