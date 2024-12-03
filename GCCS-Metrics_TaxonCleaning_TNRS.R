### Project: Global Crop Conservation Strategies Metrics ###
### Data sources Taxon Cleaning TNRS
### GCCS-Metrics_TaxonCleaning_TNRS.R
### by Sarah Gora
### Date created: 2024_11_29


#### Set working directory ####
setwd()
setwd("C:/Users/sgora/Desktop/GCCS-Metrics/Code/DataCleaning_and_Join/GCCS-Metrics_TNRS.R")

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
install.packages("TNRS")
library(TNRS)
####################################################################################################
########################### Standardize full taxon names with TNRS  ################################
install.packages("TNRS")
library(TNRS)


## set function and parameters
#' Resolve plant taxonomic names
#'
#' Resolve plant taxonomic names.
#' @param taxonomic_names Data.frame containing two columns: 1) Row number, 2) Taxonomic names to be resolved (or parsed). Note that these two columns must be in this order. Alternatively, a character vector of names can be supplied.
#' @param sources Character. Taxonomic sources to use. Default is c("wcvp", "wfo"). Options include "wfo", "wcvp", and "cact". Use TNRS_sources() for more information.
#' @param classification Character. Family classification to use. Currently options include "wfo" (the default).
#' @param mode Character.  Options are "resolve" and "parse". Default option is "resolve"
#' @param matches Character. Should all matches be returned ("all") or only the best match ("best", the default)?
#' @param accuracy numeric.  If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned. If left NULL, the default threshold will be used.
#' @param skip_internet_check Should the check for internet connectivity be skipped? Default is FALSE.
#' @param name_limit Numeric. The maximum number of names to check in one batch.  The default is 5000 and is usually the fastest option.  This cannot exceed 5000.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing TNRS results.
#' @note wfo = World Flora Online, wcvp = World Checklist of Vascular Plants, cact = Cactaceae at Caryophyllales.org.
#' @note For queries of more than 5000 names, the function will automatically divide the query into batches of 5000 names and then run the batches one after the other. Thus, for very large queries this may take some time. When this is the case, a progress bar will be displayed.
#' @note IMPORTANT: Note that parallelization of queries is automatically handled by the API, and so there is no need to further parallelize in R (in fact, doing so may actually slow things down!).
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples \dontrun{
#' # Take a subset of the testfile to speed up runtime
#' tnrs_testfile <- tnrs_testfile[1:20, ]
#'
#' results <- TNRS(taxonomic_names = tnrs_testfile)
#'
#' # Inspect the results
#' head(results, 10)
#' }
#'
TNRS <- function(taxonomic_names,
                 sources = c("wcvp", "wfo"),
                 classification = "wfo",
                 mode = "resolve",
                 matches = "best",
                 accuracy = NULL,
                 skip_internet_check = FALSE,
                 name_limit = 5000,
                 ...) {
  # Check for internet access
  if (!skip_internet_check) {
    if (!check_internet()) {
      message("This function requires internet access, please check your connection.")
      return(invisible(NULL))
    }
  }
  
  # If taxonomic names are supplied as a character string, make them into a data.frame
  
  if (inherits(x = taxonomic_names, what = "character")) {
    taxonomic_names <- as.data.frame(cbind(1:length(taxonomic_names), taxonomic_names))
  }
  
  
  # Specify the limit of names for the TNRS
  
  if (name_limit > 5000) {
    message("name_limit cannot exceed 5000, fixing")
    name_limit <- 5000
  }
  
  # Check that accuracy makes sense
  
  if (!class(accuracy) %in% c("NULL", "numeric")) {
    stop("accuracy should be either numeric between 0 and 1, or NULL")
  }
  
  # Check that sources are valid
  if (!all(sources %in% c("wfo", "wcvp", "cact"))) {
    message("Invalid source(s) specified. Current options are: wfo, wcvp, cact ")
    return(invisible(NULL))
  }
  
  
  # Check that classification is valid
  if (length(classification) != 1 | !classification %in% c("wfo")) {
    message("Invalid classification specified. Current options are: wfo ")
    return(invisible(NULL))
  }
  
  # Check that mode is valid
  if (length(mode) != 1 | !mode %in% c("resolve", "parse")) {
    message("Invalid mode specified. Current options are: resolve, parse ")
    return(invisible(NULL))
  }
  
  # Check that matches is valid
  if (length(matches) != 1 | !matches %in% c("best", "all")) {
    message("Invalid mode specified. Current options are: best, all ")
    return(invisible(NULL))
  }
  
  # reformat sources to match API input
  sources <- paste0(sources, collapse = ",")
  
  
  
  # If there are less than the max number of names allowable, send them to the base package
  if (nrow(taxonomic_names) <= name_limit) {
    return(TNRS_base(
      taxonomic_names = taxonomic_names,
      sources = sources,
      classification = classification,
      mode = mode,
      matches = matches,
      accuracy = accuracy,
      skip_internet_check = skip_internet_check,
      ...
    ))
  } #
  
  # If there are more than the max number of records, divide them into chunks and process the chunks
  
  
  
  if (nrow(taxonomic_names) > name_limit) {
    nchunks <- ceiling(nrow(taxonomic_names) / name_limit)
    
    # set up progress bar
    pb <- txtProgressBar(
      min = 0, # Minimum value of the progress bar
      max = nchunks, # Maximum value of the progress bar
      style = 3, # Progress bar style (also available style = 1 and style = 2)
      # width = 50,   # Progress bar width. Defaults to getOption("width")
      char = "="
    ) # Character used to create the bar
    
    
    for (i in 1:nchunks) {
      # Use the first batch of results to set up the output file
      if (i == 1) {
        results <- TNRS_base(
          taxonomic_names = taxonomic_names[(((i - 1) * name_limit) + 1):(i * name_limit), ],
          sources = sources,
          classification = classification,
          mode = mode,
          matches = matches,
          accuracy = accuracy,
          skip_internet_check = skip_internet_check,
          ...
        )
        
        # results<-matrix(nrow = nrow(taxonomic_names),ncol = ncol(results_i))
        # $results <- as.data.frame(results,stringsAsFactors = F)
        # colnames(results)<-colnames(results_i)
        # results[(((i-1)*name_limit)+1):(i*name_limit),]<-results_i
        # rm(results_i)
      } # for first batch
      
      
      # For last batch
      if (i == nchunks) {
        results <- rbind(
          results,
          TNRS_base(
            taxonomic_names = taxonomic_names[(((i - 1) * name_limit) + 1):(nrow(taxonomic_names)), ],
            sources = sources,
            classification = classification,
            mode = mode,
            matches = matches,
            accuracy = accuracy,
            skip_internet_check = skip_internet_check,
            ...
          )
        )
      } # last batch
      
      
      # middle bits
      if (i != nchunks & i != 1) {
        results <- rbind(
          results,
          TNRS_base(
            taxonomic_names = taxonomic_names[(((i - 1) * name_limit) + 1):(i * name_limit), ],
            sources = sources,
            classification = classification,
            mode = mode,
            matches = matches,
            accuracy = accuracy,
            skip_internet_check = skip_internet_check,
            ...
          )
        )
      } # middle bits
      
      setTxtProgressBar(pb, i)
    } # i loop
  } # if more than 10k
  
  
  close(pb)
  return(results)
} # fx



## TNRS Taxon Cleaning for data sources:
# Dataset containing two columns: scientific name and row number
# run TNRS by fullTaxa field 
#            -full taxonomic name with as much information as possible
#            -found in all data sources
## SG note: TNRS would not run on combined dataset, im guessing too large of processing 
# attempted twice and ran overnight 
# will have to clean taxon by individual data source to break up processing into chunks


## BGCI
BGCI_allcrops_fullTaxa <- BGCI_allcrops[c("fullTaxa")]
TNRS_BGCI_cropdata <- BGCI_allcrops_fullTaxa
head(TNRS_BGCI_cropdata, n = 20)

# Note that there are a variety of formats represented here, sometimes including
# scientific name only
# genus only
# family and genus
# family, scientific name, and author

# run TNRS, may take 30 min to several hours
results <- TNRS(taxonomic_names = TNRS_BGCI_cropdata)

# Inspect the results
head(results, 10)

# The output includes information on the name submitted, the match score (how close the match is), the name matched, the status of the matched name, and the accepted name.
View(results)
TNRS_BGCI_results <- results
write_xlsx(TNRS_BGCI_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/TNRS_BGCI_results.xlsx")



## WIEWS (duplication removed data)
WIEWS_allcrops_fullTaxa <- WIEWS_allcrops_duprmv[c("fullTaxa")]
TNRS_WIEWS_cropdata <- WIEWS_allcrops_fullTaxa
head(TNRS_WIEWS_cropdata, n = 20)
## run TNRS taxon clean
results <- TNRS(taxonomic_names = TNRS_WIEWS_cropdata)
## view results
head(results, 10)
View(results)
## save results
TNRS_WIEWS_results <- results
write_xlsx(TNRS_WIEWS_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/TNRS_WIEWS_results.xlsx")



## Genesys
Genesys_allcrops_fullTaxa <- Genesys_allcrops[c("fullTaxa")]
TNRS_Genesys_cropdata <- Genesys_allcrops_fullTaxa
head(TNRS_Genesys_cropdata, n = 20)
## run TNRS taxon clean 
results <- TNRS(taxonomic_names = TNRS_Genesys_cropdata) #### something is not right here, only Oryza was cleaned
## view results
head(results, 10)
View(results)
## save results
TNRS_Genesys_results <- results
write_xlsx(TNRS_Genesys_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/TNRS_Genesys_results.xlsx")



## GBIF
GBIF_allcrops_fullTaxa <- GBIF_allcrops[c("fullTaxa")]
TNRS_GBIF_cropdata <- GBIF_allcrops_fullTaxa
head(TNRS_GBIF_cropdata, n = 20)
## run TNRS taxon clean 
results <- TNRS(taxonomic_names = TNRS_GBIF_cropdata)
## view results
head(results, 10)
View(results)
## save results
TNRS_GBIF_results <- results
write_xlsx(TNRS_GBIF_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/TNRS_GBIF_results.xlsx")



## SGSV
SGSV_allcrops_fullTaxa <- SGSV_allcrops[c("fullTaxa")]
TNRS_SGSV_cropdata <- SGSV_allcrops_fullTaxa
head(TNRS_SGSV_cropdata, n = 20)
## run TNRS taxon clean 
results <- TNRS(taxonomic_names = TNRS_SGSV_cropdata)
## view results
head(results, 10)
View(results)
## save results
TNRS_SGSV_results <- results
write_xlsx(TNRS_SGSV_results, "C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/TNRS_SGSV_results.xlsx")






#### View TNRS datasets
#### Add accepted taxon names (TNRS) to datasets in main code file
#### join to original data sources in main code file

library(readr)
TNRS_BGCI_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/BGCIPlantSearch_data/TNRS_BGCI_results.xlsx")
TNRS_WIEWS_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/TNRS_WIEWS_results.xlsx")
#### genesys didnt work properly? instead just use the Genesys_allcrops file (already includes Grin cleaned taxon names)
# TNRS_Genesys_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GenesysPGR_data/All_Crops/TNRS_Genesys_results.xlsx")
TNRS_GBIF_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/GBIF_data/Living Records/All_Crops/TNRS_GBIF_results.xlsx")
TNRS_SGSV_results <- read_excel("C:/Users/sgora/Desktop/GCCS-Metrics/Data/SGSV_data/TNRS_SGSV_results.xlsx")


View(TNRS_BGCI_results)
View(TNRS_WIEWS_results)
View(TNRS_GBIF_results)
View(TNRS_SGSV_results)


