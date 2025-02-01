### Project: Global Crop Conservation Strategies Metrics ###
### Priority Metrics Calculations 
### Degree of Duplication
### GCCS-Metrics_Degree_of_Duplication_PGscript.R
### by Sarah Gora
### Date transformed: 2024_12_19


### Peter Giovannini Python script transformed to R script using Git copilot

# intall/load packages
library(dplyr)
library(tidyr)

# load starting Genesys dataset and re-name "df2"
Genesys_allcrops_unformatted <- read_csv("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/Genesys_allcrops_unformatted.csv")
df2 <- Genesys_allcrops_unformatted

# Prepare dataset 
df2$INSTCODE <- as.character(df2$INSTCODE)  # df2 is the starting Genesys dataset
df2$ACCENUMB <- as.character(df2$ACCENUMB)
df2$ID <- paste(df2$INSTCODE, df2$ACCENUMB, sep = "")  # Create unique ID for each accession using accession number and institute code
df3 <- df2 %>% distinct(ID, .keep_all = TRUE) # Drop potential duplicates
df3$holding_country <- substr(df3$INSTCODE, 1, 3) # Create a column for holding country ISO code using first three letters of institute code

# Use value in DUPLSITE to create a list with each institute code as item
df3$DUPLSITE_LIST <- strsplit(replace_na(df3$DUPLSITE, ""), ";")

duplicates_out_country <- function(site, pat) {
  # Function to check for duplicates out of the country
  res <- 0
  for (i in site) {
    if ("NOR051" %in% i) {
      next
    } else if (pat %in% i) {
      next
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 1
      return(res)
    }
  }
  return(res)
}

duplicates_in_country <- function(site, pat, holder) {
  # Function to check for duplicates in the country
  res <- 0
  for (i in site) {
    if ("NOR051" %in% i) {
      next
    } else if (i == holder) {
      next
    } else if (pat %in% i) {
      res <- 1
      return(res)
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_SDGV <- function(site) {
  # Function to check if there are duplicates at NOR051 (SGSV)
  res <- 0
  for (i in site) {
    if ("NOR051" %in% i) {
      res <- 1
      return(res)
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_only_in_country <- function(site, pat, holder) {
  # Function to check for duplicates only in the country
  res <- 0
  for (i in site) {
    if ("NOR051" %in% i) {
      res <- 0
      return(res)
    } else if (i == holder) {
      next
    } else if (pat %in% i) {
      res <- 1
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 0
      return(res)
    }
  }
  return(res)
}

estimated_sd_genesys <- function(df, grouping_by = 'GENUS') {
  # Function to estimate safety duplications in Genesys dataset
  sd <- df %>% filter(DUPLSITE != "NOR051") %>% group_by_at(grouping_by) %>% summarise(SD = n())
  sgsv <- df %>% filter(grepl("NOR051", DUPLSITE, na.rm = TRUE)) %>% group_by_at(grouping_by) %>% summarise(SGSV = n())
  grouped <- df %>% group_by_at(grouping_by) %>% summarise(`Genesys all` = n())
  result <- full_join(sd, sgsv, by = grouping_by) %>% full_join(grouped, by = grouping_by) %>% replace_na(list(SD = 0, SGSV = 0))
  return(result)
}

safety_duplication_complete <- function(df, groupby = 'GENUS') {
  # Function to perform safety duplication analysis
  df <- df %>%
    mutate(SD_out_country = mapply(duplicates_out_country, DUPLSITE_LIST, holding_country),
           SD_in_country = mapply(duplicates_in_country, DUPLSITE_LIST, holding_country, INSTCODE),
           SD_SDGV = mapply(duplicates_SDGV, DUPLSITE_LIST),
           SD_only_in_country = mapply(duplicates_only_in_country, DUPLSITE_LIST, holding_country, INSTCODE)) %>%
    mutate(all_sd = SD_in_country + SD_SDGV + SD_out_country,
           no_SD = all_sd < 1)
  
  grouped <- df %>% group_by_at(groupby)
  
  SD_out <- grouped %>% summarise(SD_out_country = sum(SD_out_country))
  SD_in <- grouped %>% summarise(SD_in_country = sum(SD_in_country))
  SD_only_in <- grouped %>% summarise(SD_only_in_country = sum(SD_only_in_country))
  no_sd <- grouped %>% summarise(no_SD = sum(no_SD))
  SD_SGSV <- grouped %>% summarise(SD_SDGV = sum(SD_SDGV))
  acc <- grouped %>% summarise(accessions_total = n())
  
  results <- SD_in %>%
    full_join(SD_out, by = groupby) %>%
    full_join(SD_only_in, by = groupby) %>%
    full_join(SD_SGSV, by = groupby) %>%
    full_join(no_sd, by = groupby) %>%
    full_join(acc, by = groupby) %>%
    arrange(desc(accessions_total)) %>%
    mutate(SD_in_country_per = 100 * (SD_in_country / accessions_total),
           SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
           SD_out_country_per = 100 * (SD_out_country / accessions_total),
           SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
           no_SD_per = 100 * (no_SD / accessions_total)) %>%
    select(SD_in_country, SD_out_country, SD_only_in_country, SD_SDGV, no_SD, accessions_total,
           SD_in_country_per, SD_only_in_country_per, SD_out_country_per, SD_SDGV_per, no_SD_per)
  
  return(results)
}

# Run functions
by_genus <- safety_duplication_complete(df3)
by_genebank <- safety_duplication_complete(df3, groupby = 'INSTCODE')



# Save results in CSV files
write.csv(by_genus, 'Genesys_results_by_genus.csv', row.names = FALSE)
write.csv(by_genebank, 'Genesys_results_by_genebanks.csv', row.names = FALSE)








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
df2$ID <- paste(df2$instCode, df2$acceNumb, sep = "")
df3 <- df2 %>% distinct(ID, .keep_all = TRUE)
df3$holding_country <- substr(df3$instCode, 1, 3)

# Calculate accurate accession counts separately
accession_counts <- df2 %>% group_by(cropStrategy) %>% summarise(accessions_total = n())

# Use value in duplSite to create a list with each institute code as item
df3$duplSite_LIST <- strsplit(replace_na(df3$duplSite, ""), ";")

duplicates_out_country <- function(site, pat) {
  res <- 0 # Function to check for duplicates out of the country
  for (i in site) {
    if ("NOR051" %in% i) {
      next
    } else if (pat %in% i) {
      next
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 1
      return(res)
    }
  }
  return(res)
}

duplicates_in_country <- function(site, pat, holder) {
  res <- 0  # Function to check for duplicates in the country
  for (i in site) {
    if ("NOR051" %in% i) {
      next
    } else if (i == holder) {
      next
    } else if (pat %in% i) {
      res <- 1
      return(res)
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_SDGV <- function(site) {
  res <- 0 # Function to check if there are duplicates at NOR051 (SGSV)
  for (i in site) {
    if ("NOR051" %in% i) {
      res <- 1
      return(res)
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 0
    }
  }
  return(res)
}

duplicates_only_in_country <- function(site, pat, holder) {
  res <- 0 # Function to check for duplicates only in the country
  for (i in site) {
    if ("NOR051" %in% i) {
      res <- 0
      return(res)
    } else if (i == holder) {
      next
    } else if (pat %in% i) {
      res <- 1
    } else if (i == "" || i == "nan") {
      next
    } else {
      res <- 0
      return(res)
    }
  }
  return(res)
}

# Function to estimate safety duplications in Genesys dataset
estimated_sd_genesys <- function(df, grouping_by = 'cropStrategy') {
  sd <- df %>% filter(duplSite != "NOR051") %>% group_by_at(grouping_by) %>% summarise(SD = n())
  sgsv <- df %>% filter(grepl("NOR051", duplSite, na.rm = TRUE)) %>% group_by_at(grouping_by) %>% summarise(SGSV = n())
  grouped <- df %>% group_by_at(grouping_by) %>% summarise(`Genesys all` = n())
  result <- full_join(sd, sgsv, by = grouping_by) %>% full_join(grouped, by = grouping_by) %>% replace_na(list(SD = 0, SGSV = 0))
  return(result)
}

# Function to perform safety duplication analysis
safety_duplication_complete <- function(df, groupby = 'cropStrategy') {
  df <- df %>%
    mutate(SD_out_country = mapply(duplicates_out_country, duplSite_LIST, holding_country),
           SD_in_country = mapply(duplicates_in_country, duplSite_LIST, holding_country, instCode),
           SD_SDGV = mapply(duplicates_SDGV, duplSite_LIST),
           SD_only_in_country = mapply(duplicates_only_in_country, duplSite_LIST, holding_country, instCode)) %>%
    mutate(all_sd = SD_in_country + SD_SDGV + SD_out_country,
           no_SD = all_sd < 1)
  
  grouped <- df %>% group_by_at(groupby) %>%
    summarise(SD_out_country = sum(SD_out_country),
              SD_in_country = sum(SD_in_country),
              SD_only_in_country = sum(SD_only_in_country),
              no_SD = sum(no_SD),
              SD_SDGV = sum(SD_SDGV)) %>%
    left_join(accession_counts, by = groupby) %>%
    arrange(desc(accessions_total)) %>%
    mutate(SD_in_country_per = 100 * (SD_in_country / accessions_total),
           SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
           SD_out_country_per = 100 * (SD_out_country / accessions_total),
           SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
           no_SD_per = 100 * (no_SD / accessions_total)) %>%
    select(cropStrategy, SD_in_country, SD_out_country, SD_only_in_country, SD_SDGV, no_SD, accessions_total,
           SD_in_country_per, SD_only_in_country_per, SD_out_country_per, SD_SDGV_per, no_SD_per)
  
  results_genebank <- df %>% group_by(instCode) %>%
    summarise(SD_out_country = sum(SD_out_country),
              SD_in_country = sum(SD_in_country),
              SD_only_in_country = sum(SD_only_in_country),
              no_SD = sum(no_SD),
              SD_SDGV = sum(SD_SDGV),
              accessions_total = n()) %>%
    arrange(desc(accessions_total)) %>%
    mutate(SD_in_country_per = 100 * (SD_in_country / accessions_total),
           SD_only_in_country_per = 100 * (SD_only_in_country / accessions_total),
           SD_out_country_per = 100 * (SD_out_country / accessions_total),
           SD_SDGV_per = 100 * (SD_SDGV / accessions_total),
           no_SD_per = 100 * (no_SD / accessions_total)) %>%
    select(instCode, SD_in_country, SD_out_country, SD_only_in_country, SD_SDGV, no_SD, accessions_total,
           SD_in_country_per, SD_only_in_country_per, SD_out_country_per, SD_SDGV_per, no_SD_per)
  
  return(list(results_genus = grouped, results_genebank = results_genebank))
}

# Run functions 
results <- safety_duplication_complete(df3)

by_cropStrategy <- results$results_genus
by_genebank <- results$results_genebank

# Save results in CSV files
write.csv(by_cropStrategy, 'allcrops_results_by_cropStrategy.csv', row.names = FALSE)
write.csv(by_genebank, 'all_crops_results_by_genebank.csv', row.names = FALSE)


