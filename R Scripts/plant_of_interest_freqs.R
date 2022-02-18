# calculate frequency for select plants


# libraries ---------------------------------------------------------------

library(tidyverse)


# global variables --------------------------------------------------------

year <- "2021"

if(year == "2021"){
  filepath <- "./FieldData/2021/"
  plants_of_interest <- c("CARELYN", "IRISPSE", "LYTHSAL",
                        "PHALARU", "EXO_TYPHA")
}

if(year == "2015"){
  filepath <- "./FieldData/2015/csv/"
  plants_of_interest <- c("Lyngbye's sedge", "yellow iris", "purple loosestrife",
                          "reed canarygrass","EXO_TYPHA")
}

files <- list.files(filepath)

freq_results <- data.frame()

# function definitions ----------------------------------------------------

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


VegImport <- function(filepath, year){
  # import vegetation data from a csv file
  df <- read.csv(filepath, fileEncoding = "UTF-8-BOM")
  df$PERCENT_COVER <- as.numeric(df$PERCENT_COVER)
  
  if(year == "2015"){
    df <- df[1:8] %>% 
      mutate(Site_Number = SITE_ID) %>% 
      select(-SITE_ID)
  }
  
  # create PLOT_CODE column for unique plot identifiers. Filter out Riparian.
  df <- df %>% 
    mutate(PERCENT_COVER = as.numeric(PERCENT_COVER),
           PLOT_CODE = paste(Site_Number, COMMUNITY, PLOT, sep = "-")) %>% 
    filter(COMMUNITY != "RIP")
  
  return(df)
}


VegLongToWide <- function(data, remove_codes = TRUE){
  # convert veg data from long to wide format
  wideData <- data %>%
    select(c(PLOT_CODE, SPECIES_CODE, PERCENT_COVER)) %>%
    spread(SPECIES_CODE, PERCENT_COVER)
  wideData[is.na(wideData)] <- 0
  wideData <- wideData
  
  if (year == "2021"){
    wideData$EXO_TYPHA <- wideData %>% 
      select(any_of(c("TYPHANG", "TYPHGLA"))) %>% 
      rowSums()
  }
  
  if (year == "2015"){
    wideData$EXO_TYPHA <- wideData %>% 
      select(any_of(c("lesser cattail", "blue cattail"))) %>% 
      rowSums()
  }
  
  if (remove_codes == TRUE){
    return(wideData %>% select(-PLOT_CODE))
  } else {
    return(wideData)
  }
  
  return(wideData)
}


# main loop ---------------------------------------------------------------

# relative frequencies for plants of interest
for(i in 1:length(files)){
  site <- VegImport(paste(filepath, files[i], sep = ""), year) %>%
    filter(COMMUNITY == 1) %>% 
    VegLongToWide()
  
  freqs <- plantFreq(site, plants_of_interest = plants_of_interest)
  
  freq_results <- rbind(freq_results, freqs)
}

names(freq_results) <- plants_of_interest

freq_results$SITE <- str_remove(files, ".csv")

