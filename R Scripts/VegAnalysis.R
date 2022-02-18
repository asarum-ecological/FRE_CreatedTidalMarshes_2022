# Analysis script for FRE Compensation Sites


# libraries ---------------------------------------------------------------

library(tidyverse)
library(vegan)

# load veg functions
source("./Scripts/VegFunctions.R")


# file paths --------------------------------------------------------------

year = "2021" # SET THIS VARIABLE MANUALLY

if (year == "2021"){
  datapath <- "./FieldData/2021/"
}

if (year == "2015"){
  datapath <- "./FieldData/2015/csv/"
}

files <- list.files(datapath)

resultpath <-  paste("./Results/", year, "/", sep = "")


# main loop ---------------------------------------------------------------

ProjectResults <- data.frame()

for (i in 1:length(files)) {
  # import each csv and transform to wide format
  # only community 1 (marsh) is considered
  vegLong <- VegImport(paste(datapath, files[i], sep = ""), year = year)
  vegWide <- VegLongToWide(vegLong)
  
  # unique species list for each origin, used in other calculations.
  plants <- VegUniqueSpecies(vegLong)
  
  if (length(plants) == 0){
    siteStats <- rep(NA, times = 18)
    ProjectResults <- rbind(ProjectResults, siteStats)
  } else {
      # veg percent cover summary stats
      pcStats <- VegPercentCoverStats(vegWide)
      write.csv(pcStats, paste(resultpath, "PC_Stats/", "PC_Stats_", files[i], sep = ""))
      
      # site veg community statistics
      siteStats <- VegStats(vegLong, vegWide, plants)
      relativeAbundance <- VegRelativeAbundance(vegWide, plants)
      siteStats <- c(siteStats, relativeAbundance)
      
      # add new row for each site
      ProjectResults <- rbind(ProjectResults, siteStats)
  }
}


# data cleanup and export -------------------------------------------------

ProjectResults$Site_ID <- files %>%
  str_remove(".csv")

ProjectResults$RA_Sum <- ProjectResults %>%
  select(c("n_ra", "e_ra", "i_ra", "u_ra")) %>%
  rowSums()

write.csv(ProjectResults, paste(resultpath, "VegDataResults_", year, ".csv", sep = ""))
