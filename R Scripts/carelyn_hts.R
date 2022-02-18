# assign plot-level CARELYN mean height from long data.


# load libraries ----------------------------------------------------------

library(tidyverse)


# global variables --------------------------------------------------------

year = "2021" # SET THIS VARIABLE MANUALLY

# set file path for FielData based on year
if (year == "2021"){
  datapath <- "./FieldData/2021/"
}
if (year == "2015"){
  datapath <- "./FieldData/2015/csv/"
}

# list all the site data.
files <- list.files(datapath)

# set path for the final results
resultpath <-  paste("./Results/", year, "/", sep = "")

carelyn_results <- data.frame()


# define functions --------------------------------------------------------

read_data <- function(filepath){
  read.csv(filepath, fileEncoding = "UTF-8-BOM") %>% 
    {if (year == "2015"){ # fix 2015 naming discrepancies
      mutate(., MAX_LH_CM = MAX_LH, Site_Number = SITE_ID)
    }else{
      .
    }} %>% 
    filter(COMMUNITY != "RIP") %>% # no riparian 
    mutate(PLOT_CODE = paste(Site_Number, COMMUNITY, PLOT, sep = "-")) %>%
    select(c(PLOT_CODE, MAX_LH_CM))
}


carelyn_analysis <- function(data){
  # join list of max-sedge-height data to list of unique plot codes.
  carelyn <- right_join(filter(data, !is.na(data$MAX_LH_CM)),
           data.frame(PLOT_CODE = data$PLOT_CODE %>% unique()),
           by = 'PLOT_CODE',
           all = TRUE)

  carelyn <- carelyn[order(carelyn$PLOT_CODE),] # ordered by plot code
  
  return(carelyn)
}


# main loop ---------------------------------------------------------------

for(i in 1:length(files)){
  filepath <- paste(datapath, files[i], sep = "") # filepath for each site
  
  veg <- read_data(filepath) # load site data
  
  site_carelyn <- carelyn_analysis(veg) # get unique carelyn heights for each plot
  
  carelyn_results <- rbind(carelyn_results, site_carelyn) # add row-wise to project results.
}


# check for duplicate plots
n_occur <- data.frame(table(carelyn_results$PLOT_CODE))
n_occur[n_occur$Freq > 1,]

# fix duplicates (take max from each plot)
carelyn_results<- data.frame(PLOT_CODE = carelyn_results$PLOT_CODE %>% unique(),
                             CARELYN = tapply(carelyn_results$MAX_LH_CM, carelyn_results$PLOT_CODE, max))

# write out results
# write.csv(carelyn_results, paste(resultpath, "carelyn_hts_", year, ".csv", sep = ""))
