### Project: Global Crop Conservation Strategies Metrics ###
### Priority Metrics Calculations 
### Degree of Duplication
### GCCS-Metrics_Degree_of_Duplication_PGupdatedscript.R
### by Sarah Gora
### Date transformed: 2024_02_13


### Peter Giovannini Python updated script transformed to R script using Git copilot

# intall/load packages
library(dplyr)
library(tidyr)

# load starting Genesys dataset and re-name "df2"
Genesys_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")
df2 <- Genesys_allcrops_unformatted

# Prepare dataset 
df2$INSTCODE <- as.character(df2$INSTCODE)  # df2 is the starting Genesys dataset
df2$ACCENUMB <- as.character(df2$ACCENUMB)
df2$ID <- paste0(df2$INSTCODE, df2$ACCENUMB)  # Create unique ID for each accession using accession number and institute code
df3 <- df2 %>% distinct(ID, .keep_all = TRUE) # Drop potential duplicates
df3 <- df3 %>% mutate(holding_country = substr(INSTCODE, 1, 3)) # Create a column for holding country ISO code using first three letters of institute code

# Use value in DUPLSITE to create a list with each institute code as item
df3 <- df3 %>% mutate(DUPLSITE_LIST = strsplit(ifelse(is.na(DUPLSITE), '', DUPLSITE), ';'))

duplicates_out_country <- function(site, pat) {
  res <- 0
  for (i in site) {
    if (i %in% c('NOR051', '', 'nan') || substr(i, 1, 3) == pat) {
      next
    } else {
      return(1)
    }
  }
  return(res)
}

duplicates_in_country <- function(site, pat, holder) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      next
    } else if (i == holder) {
      next
    } else if (grepl(pat, i)) {
      return(1)
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_SDGV <- function(site) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      return(1)   
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_only_in_country <- function(site, pat, holder) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      return(0) 
    } else if (i == holder) {
      next
    } else if (grepl(pat, i)) {
      res <- 1    
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      return(0)
    }
  }
  return(res)
}

estimated_sd_genesys <- function(df, grouping_by = 'GENUS') {
  sd <- df %>% filter(DUPLSITE != 'NOR051') %>% group_by(!!sym(grouping_by)) %>% summarise(SD = n())
  sgsv <- df %>% filter(grepl('NOR051', DUPLSITE, na.rm = TRUE)) %>% group_by(!!sym(grouping_by)) %>% summarise(SGSV = n())
  result <- sd %>% full_join(sgsv, by = grouping_by)
  grouped <- df %>% group_by(!!sym(grouping_by)) %>% summarise(Genesys_all = n())
  result <- result %>% full_join(grouped, by = grouping_by)
  result <- result %>% replace_na(list(SD = 0, SGSV = 0, Genesys_all = 0)) %>% mutate_if(is.numeric, as.integer)
  return(result)
}

safety_duplication_complete <- function(df, groupby = 'GENUS') {
  df <- df %>%
    mutate(
      SD_out_country = mapply(duplicates_out_country, DUPLSITE_LIST, holding_country),
      SD_in_country = mapply(duplicates_in_country, DUPLSITE_LIST, holding_country, INSTCODE),
      SD_SDGV = mapply(duplicates_SDGV, DUPLSITE_LIST),
      SD_only_in_country = mapply(duplicates_only_in_country, DUPLSITE_LIST, holding_country, INSTCODE),
      all_sd = SD_in_country + SD_SDGV + SD_out_country,
      no_SD = all_sd < 1
    )
  
  grouped <- df %>% group_by(!!sym(groupby))
  results <- grouped %>%
    summarise(
      SD_in_country = sum(SD_in_country),
      SD_out_country = sum(SD_out_country),
      SD_only_in_country = sum(SD_only_in_country),
      SD_SDGV = sum(SD_SDGV),
      no_SD = sum(no_SD),
      accessions_total = n()
    ) %>%
    arrange(desc(accessions_total)) %>%
    mutate(
      SD_in_country_per = 100 * (SD_in_country / accessions_total),
      SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
      SD_out_country_per = 100 * (SD_out_country / accessions_total),
      SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
      no_SD_per = 100 * (no_SD / accessions_total)
    ) %>%
    rename(
      `SD_in_country` = SD_in_country,
      `SD_out_country` = SD_out_country,
      `SD_only_in_country` = SD_only_in_country,
      `SD_SDGV` = SD_SDGV,
      `no_SD` = no_SD,
      `accessions_total` = accessions_total,
      `SD_in_country_per` = SD_in_country_per,
      `SD_only_in_country_per` = SD_only_in_country_per,
      `SD_out_country_per` = SD_out_country_per,
      `SD_SDGV_per` = SD_SDGV_per,
      `no_SD_per` = no_SD_per
    )
  
  return(results)
}

# Run functions 
by_genus <- safety_duplication_complete(df3)
by_genebank <- safety_duplication_complete(df3, groupby = 'INSTCODE')

# Save results in CSV files
write.csv(by_genus, 'Genesys_rerun_results_by_genus.csv', row.names = FALSE)
write.csv(by_genebank, 'Genesys_rerun_results_by_genebanks.csv', row.names = FALSE)

View(by_genus)
View(by_genebank)














##### re-run code using GCCS-metrics data, combined_allcrops

# Column name synonyms:
# INSTCODE = instCode
# ACCENUMB = acceNumb
# GENUS = acceptedGenus_TNRS
# DUPLSITE = duplSite

# Read in our dataset using all data sources (not SGSV) and rename "df2"
library(readr)
library(dplyr)
library(tidyr)

combined_allcrops <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/Combined_allsources/combined_allcrops.csv")
df2 <- combined_allcrops

# Prepare dataset 
df2$instCode <- as.character(df2$instCode)
df2$acceNumb <- as.character(df2$acceNumb)
df2$ID <- paste0(df2$instCode, df2$acceNumb)
df3 <- df2 %>% distinct(ID, .keep_all = TRUE)
df3 <- df3 %>% mutate(holding_country = substr(instCode, 1, 3))

# Calculate accurate accession counts separately
accession_counts <- df2 %>% group_by(cropStrategy) %>% summarise(accessions_total = n())

# Use value in duplSite to create a list with each institute code as item
df3 <- df3 %>% mutate(duplSite_LIST = strsplit(ifelse(is.na(duplSite), '', duplSite), ';'))

duplicates_out_country <- function(site, pat) {
  res <- 0
  for (i in site) {
    if (i %in% c('NOR051', '', 'nan') || substr(i, 1, 3) == pat) {
      next
    } else {
      return(1)
    }
  }
  return(res)
}

duplicates_in_country <- function(site, pat, holder) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      next
    } else if (i == holder) {
      next
    } else if (grepl(pat, i)) {
      return(1)
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_SDGV <- function(site) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      return(1)   
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_only_in_country <- function(site, pat, holder) {
  res <- 0
  for (i in site) {
    if (grepl('NOR051', i)) {
      return(0) 
    } else if (i == holder) {
      next
    } else if (grepl(pat, i)) {
      res <- 1    
    } else if (i %in% c('', 'nan')) {
      next
    } else {
      return(0)
    }
  }
  return(res)
}

# Function to estimate safety duplications in combined dataset
estimated_sd <- function(df, grouping_by = 'cropStrategy') {
  sd <- df %>% filter(duplSite != 'NOR051') %>% group_by(!!sym(grouping_by)) %>% summarise(SD = n())
  sgsv <- df %>% filter(grepl('NOR051', duplSite, na.rm = TRUE)) %>% group_by(!!sym(grouping_by)) %>% summarise(SGSV = n())
  result <- sd %>% full_join(sgsv, by = grouping_by)
  grouped <- df %>% group_by(!!sym(grouping_by)) %>% summarise(data_sources_all = n())
  result <- result %>% full_join(grouped, by = grouping_by)
  result <- result %>% replace_na(list(SD = 0, SGSV = 0, data_sources_all = 0)) %>% mutate_if(is.numeric, as.integer)
  return(result)
}

# Function to perform safety duplication analysis
safety_duplication_complete <- function(df, groupby = 'cropStrategy') {
  df <- df %>%
    mutate(
      SD_out_country = mapply(duplicates_out_country, duplSite_LIST, holding_country),
      SD_in_country = mapply(duplicates_in_country, duplSite_LIST, holding_country, instCode),
      SD_SDGV = mapply(duplicates_SDGV, duplSite_LIST),
      SD_only_in_country = mapply(duplicates_only_in_country, duplSite_LIST, holding_country, instCode),
      all_sd = SD_in_country + SD_SDGV + SD_out_country,
      no_SD = all_sd < 1
    )
  
  grouped <- df %>% group_by(!!sym(groupby)) %>%
    summarise(
      SD_out_country = sum(SD_out_country),
      SD_in_country = sum(SD_in_country),
      SD_only_in_country = sum(SD_only_in_country),
      no_SD = sum(no_SD),
      SD_SDGV = sum(SD_SDGV)
    ) %>%
    left_join(accession_counts, by = groupby) %>%
    arrange(desc(accessions_total)) %>%
    mutate(
      SD_in_country_per = 100 * (SD_in_country / accessions_total),
      SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
      SD_out_country_per = 100 * (SD_out_country / accessions_total),
      SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
      no_SD_per = 100 * (no_SD / accessions_total)
    ) %>%
    select(cropStrategy, SD_in_country, SD_out_country, SD_only_in_country, SD_SDGV, no_SD, accessions_total,
           SD_in_country_per, SD_only_in_country_per, SD_out_country_per, SD_SDGV_per, no_SD_per)
  
  results_genebank <- df %>% group_by(instCode) %>%
    summarise(
      SD_out_country = sum(SD_out_country),
      SD_in_country = sum(SD_in_country),
      SD_only_in_country = sum(SD_only_in_country),
      no_SD = sum(no_SD),
      SD_SDGV = sum(SD_SDGV),
      accessions_total = n()
    ) %>%
    arrange(desc(accessions_total)) %>%
    mutate(
      SD_in_country_per = 100 * (SD_in_country / accessions_total),
      SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
      SD_out_country_per = 100 * (SD_out_country / accessions_total),
      SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
      no_SD_per = 100 * (no_SD / accessions_total)
    ) %>%
    select(instCode, SD_in_country, SD_out_country, SD_only_in_country, SD_SDGV, no_SD, accessions_total,
           SD_in_country_per, SD_only_in_country_per, SD_out_country_per, SD_SDGV_per, no_SD_per)
  
  return(list(results_genus = grouped, results_genebank = results_genebank))
}

# Run functions 
results <- safety_duplication_complete(df3)

by_cropStrategy <- results$results_genus
by_genebank <- results$results_genebank

# Save results in CSV files
write.csv(by_cropStrategy, 'allcrops_SD_debugged_results_by_cropStrategy.csv', row.names = FALSE)
write.csv(by_genebank, 'all_crops_SD_debugged_results_by_genebank.csv', row.names = FALSE)





### Check SD function

# note from PG:
# To check if the function safety_duplication_complete runs properly,
# could you create a modified version 
# where the function returns dataframe at the accessions level (df in your code), 
# and send me the dataframe in a CSV file? 
# basically to check the output before the grouping function. 

# Install and load the dplyr package
install.packages("dplyr")
library(dplyr)

# Modified function to perform safety duplication analysis and return the dataframe at accessions level
safety_duplication_complete_modified <- function(df, groupby = 'cropStrategy') {
  df <- df %>%
    mutate(
      SD_out_country = mapply(duplicates_out_country, duplSite_LIST, holding_country),
      SD_in_country = mapply(duplicates_in_country, duplSite_LIST, holding_country, instCode),
      SD_SDGV = mapply(duplicates_SDGV, duplSite_LIST),
      SD_only_in_country = mapply(duplicates_only_in_country, duplSite_LIST, holding_country, instCode),
      all_sd = SD_in_country + SD_SDGV + SD_out_country,
      no_SD = all_sd < 1
    )
  
  # Convert list columns to character strings
  df$duplSite_LIST <- sapply(df$duplSite_LIST, function(x) paste(x, collapse = ";"))
  
  # Save the intermediate dataframe to a CSV file
  write.csv(df, "SD_check_by_accession.csv", row.names = FALSE)
  
  return(df)
}

# Run the modified function
intermediate_df <- safety_duplication_complete_modified(df3)
View(intermediate_df)




# 

