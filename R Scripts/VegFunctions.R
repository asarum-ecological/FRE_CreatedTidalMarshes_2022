# Function definitions for vegetation analysis
# To be sourced by 2021 or 2015 analysis scripts.
# Functions expect attributes "SPECIES_CODE", "PERCENT_COVER", and "ORIGIN".

VegImport <- function(filepath, year){
  # import vegetation data from a csv file
  df <- read.csv(filepath, fileEncoding = "UTF-8-BOM")
  df$PERCENT_COVER <- as.numeric(df$PERCENT_COVER)
  df <- subset(df, COMMUNITY == 1)
  
  if(year == "2015"){
    df <- df[1:8] # reduce issues with 2015 data importing
    df$MAX_LH_CM <- df$MAX_LH # rename Lyngbye height column
  }
 
  return(df)
}

VegLongToWide <- function(data){
  # convert veg data from long to wide format
  wideData <- data %>%
    select(c(PLOT, SPECIES_CODE, PERCENT_COVER)) %>%
    spread(SPECIES_CODE, PERCENT_COVER)
  wideData[is.na(wideData)] <- 0
  wideData <- wideData[-1]
  return(wideData)
}

VegUniqueSpecies <- function(data){
  # returns lists of unique species of each origin type.
  plants <- tapply(data$SPECIES_CODE, data$ORIGIN, FUN = unique)
  return(plants)
}

VegPercentCoverStats <- function(data){
  # takes wide veg data and returns percent cover summary stats
  pcMean <- sapply(data, mean, na.rm = TRUE)
  pcSD <- sapply(data, sd, na.rm = TRUE)
  pcMax <- sapply(data, max, na.rm = TRUE)
  pcMin <- sapply(data, min, na.rm = TRUE)
  result <- data.frame(pcMean, pcSD, pcMax, pcMin)
  return(result)
}

VegRelativeAbundance <- function(wideData, plantLists){
  # takes wide data and plants list and returns relative abundance stats.
  
  n <- select(wideData, plantLists$N)
  e <- select(wideData, plantLists$E)
  i <- select(wideData, plantLists$I)
  u <- select(wideData, plantLists$U)
  total <- select(wideData, -plantLists$S)
  
  n_ra <- mean(rowSums(n)/rowSums(total) * 100, na.rm = TRUE)
  n_sd <- sd(rowSums(n)/rowSums(total) * 100, na.rm = TRUE)
  e_ra <- mean(rowSums(e)/rowSums(total) * 100, na.rm = TRUE)
  e_sd <- sd(rowSums(e)/rowSums(total) * 100, na.rm = TRUE)
  i_ra <- mean(rowSums(i)/rowSums(total) * 100, na.rm =TRUE)
  i_sd <- sd(rowSums(i)/rowSums(total) * 100, na.rm =TRUE)
  u_ra <- mean(rowSums(u)/rowSums(total) * 100, na.rm = TRUE)
  u_sd <- sd(rowSums(u)/rowSums(total) * 100, na.rm = TRUE)
  
  result <- data.frame(n_ra, n_sd,
                       e_ra, e_sd,
                       i_ra, i_sd,
                       u_ra, u_sd)
  
  return(result)
}

VegStats <- function(longData, wideData, plantLists){
  
  lyngbyeMean <- mean(longData$MAX_LH_CM, na.rm = TRUE)
  lyngbyeSD <- sd(longData$MAX_LH_CM, na.rm = TRUE)
  
  richnessMean <- mean(specnumber(select(wideData, -plantLists$S)))
  richnessSD <- sd(specnumber(select(wideData, -plantLists$S)))
  NRichnessMean <- mean(specnumber(select(wideData, plantLists$N)))
  NRichnessSD <- sd(specnumber(select(wideData, plantLists$N)))
  
  if(length(plantLists$N) == 0){
    shannon <- NA
    shannonSD <- NA
    simpson <- NA
    simpsonSD <- NA
  } else {
    shannon <- mean(diversity(select(wideData, plantLists$N), index = "shannon"))
    shannonSD <- sd(diversity(select(wideData, plantLists$N), index = "shannon"))
    simpson <- mean(diversity(select(wideData, plantLists$N), index = "simpson"))
    simpsonSD <- sd(diversity(select(wideData, plantLists$N), index = "simpson"))
  }
  
  result <- data.frame(lyngbyeMean, lyngbyeSD,
                       richnessMean, richnessSD,
                       NRichnessMean, NRichnessSD,
                       shannon, shannonSD, simpson, simpsonSD)
  return(result)
}

logCover <- function(wideData){
  if (any(names(wideData) == "WOOD" | "LOG")){
    meanWoodyCover <- wideData %>% 
      select(any(c(WOOD, LOG))) %>% 
      rowSums() %>% 
      mean(na.rm = TRUE)
    sdWoodyCover <- wideData %>% 
      select(any(c(WOOD, LOG))) %>% 
      rowSums() %>% 
      sd(na.rm = TRUE)
  } else {
    meanWoodyCover <- 0
    sdWoodyCover <- 0
  }
  return(c(meanWoodyCover,sdWoodyCover))
}

plantFreq <- function(wideData, plants_of_interest){
  result <- array()
  for(i in 1:length(plants_of_interest)){
    if(any(names(wideData) == plants_of_interest[i])){
      presence <- wideData %>% 
      select(plants_of_interest[i]) != 0
      result[i] <-  presence %>% 
      as.integer() %>% 
      sum() / length(presence)
    }else{
      result[i] <- 0
    }
  }
return(result)
}





