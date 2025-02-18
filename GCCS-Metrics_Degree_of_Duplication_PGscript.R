### Project: Global Crop Conservation Strategies Metrics ###
### Priority Metrics Calculations 
### Estimates of Safety Duplication
### GCCS-Metrics_Degree_of_Duplication_PGscript.R
### by Sarah Gora and Peter Giovannini
### Date transformed: 2024_12_19
### debugged on 18-Feb-2025

### Peter Giovannini Python script about Safety Duplication translated into R script 

# intall/load packages
library(dplyr)
library(tidyr)
library(tidyverse)  ### added by Peter G.

# define functions before anything else
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


safety_duplication_complete <- function(df, groupby = 'genus') {
  df <- df %>%
    mutate(
      SD_out_country = mapply(duplicates_out_country, duplSite_LIST, holding_country),
      SD_in_country  = mapply(duplicates_in_country,  duplSite_LIST, holding_country, instCode),
      SD_SDGV = mapply(duplicates_SDGV, duplSite_LIST),
      SD_only_in_country = mapply(duplicates_only_in_country, duplSite_LIST, holding_country, instCode),
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
  #return(df) # line used only for debugging
}

### note, before running the analysis you need to drop genebanks that only old safety duplicates 
### such as NOR051 and BRA003 from the datasets 

# read dataset 
df2<- read.csv("combined_allcrops_2025_02_14.csv", header = TRUE )

# Prepare dataset 
df2$instCode <- as.character(df2$instCode)
df2$acceNumb <- as.character(df2$acceNumb)
df2$ID <- paste0(df2$instCode, df2$acceNumb)
df2$duplSite <- as.character(df2$duplSite) # added while debugging
df2  <- df2  %>% mutate(duplSite = str_replace_all(duplSite, " ", "")) # added while debugging because found some duplsite entries with whitespaces
df3 <- df2 %>% distinct(ID, .keep_all = TRUE)
df3 <- df3 %>% mutate(holding_country = substr(instCode, 1, 3))

# Calculate accurate accession counts separately
accession_counts <- df2 %>% group_by(cropStrategy) %>% summarise(accessions_total = n())

# Use value in duplSite to create a list with each institute code as item
df3 <- df3 %>% mutate(duplSite_LIST = strsplit(ifelse(is.na(duplSite), '', duplSite), ';'))

# Run functions to determine SD by cropStrategy
by_crop <- safety_duplication_complete(df3, groupby = 'cropStrategy')
# Run functions to determine SD by instCode (i.e. genebank)
by_genebank <- safety_duplication_complete(df3, groupby = 'instCode')

# Save results in CSV files
by_crop <- apply(by_crop,2,as.character)
write.csv(by_crop, 'results_by_crop.csv', row.names = FALSE)
by_genebank <- apply(by_genebank,2,as.character)
write.csv(by_genebank, 'rerun_results_by_genebanks.csv', row.names = FALSE)

######### run same analysis for Genesys dataset   ############
#1 read dataset 
gen<- read.csv("Genesys_allcrops_unformatted.csv", header = TRUE )
#2 keep fields needed and change names according to functions used 
gen <- subset(gen, select = c(INSTCODE, ACCENUMB, GENUS, SPECIES, DUPLSITE))
colnames(gen) <- c("instCode","acceNumb","genus","species","duplSite")

#3 Prepare dataset 
gen$instCode <- as.character(gen$instCode)
gen$acceNumb <- as.character(gen$acceNumb)
gen$ID <- paste0(gen$instCode, gen$acceNumb)
gen$duplSite <- as.character(gen$duplSite) # added while debugging
gen  <- gen  %>% mutate(duplSite = str_replace_all(duplSite, " ", "")) # added while debugging because found some duplsite entries with whitespaces
gen2 <- gen %>% distinct(ID, .keep_all = TRUE) # maybe whitespaces should be eliminated from ID before this
gen2 <- gen2 %>% mutate(holding_country = substr(instCode, 1, 3))

#4 Use value in duplSite to create a list with each institute code as item
gen2 <- gen2 %>% mutate(duplSite_LIST = strsplit(ifelse(is.na(duplSite), '', duplSite), ';'))

#5 Run functions to determine SD by genus
by_genus_genesys <- safety_duplication_complete(gen2, groupby = 'genus')

#6 Run functions to determine SD by instCode (i.e. genebank)
by_genebank_genesys <- safety_duplication_complete(gen2, groupby = 'instCode')

#7 Save results in CSV files
by_genus_genesys <- apply(by_genus_genesys,2,as.character)
write.csv(by_genus_genesys, 'genesys_results_by_crop.csv', row.names = FALSE)
by_genebank_genesys <- apply(by_genebank_genesys,2,as.character)
write.csv(by_genebank_genesys, 'genesys_rerun_results_by_genebanks.csv', row.names = FALSE)
