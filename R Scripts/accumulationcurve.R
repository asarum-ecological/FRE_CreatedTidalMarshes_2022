# Species accumulation curves for a given comp site and the two nearest reference sites.


# import libraries
library("BiodiversityR")
library("tidyverse")
library("vegan")


### IMPORT DATA ###

# edit file paths as necessary

# Compensation Site
Comp <- read.csv("./FieldData/2021/11-014.csv", fileEncoding = "UTF-8-BOM")
Comp$PERCENT_COVER = as.numeric(Comp$PERCENT_COVER)
Comp <- subset(Comp, COMMUNITY == 1)
Comp <- subset(Comp, ORIGIN == "N", select = c(-ORIGIN, -Site_Number, -COMMENTS, -MAX_LH_CM, -COMMUNITY)) # select native species only and remove extra columns

# Reference Site 1 - USE THIS TEMPLATE FOR 2015 DATA
Ref1 <- read.csv("./FieldData/2015/csv/REF-11-2015.csv", fileEncoding = "UTF-8-BOM")
Ref1$PERCENT_COVER <- as.numeric(Ref1$PERCENT_COVER)
Ref1 <- subset(Ref1, COMMUNITY == 1)
Ref1 <- subset(Ref1, ORIGIN == "N", select = c(-ORIGIN, -SITE_ID, -MAX_LH, -Scientific, -COMMUNITY))

# Reference Site 2 - USE THIS AS A TEMPLATE FOR 2021 DATA
Ref2 <- read.csv("./FieldData/2021/REF-07.csv", fileEncoding = "UTF-8-BOM")
Ref2$PERCENT_COVER <- as.numeric(Ref2$PERCENT_COVER)
Ref2 <- subset(Ref2, COMMUNITY == 1)
Ref2 <- subset(Ref2, ORIGIN == "N", select = c(-ORIGIN, -Site_Number, -COMMENTS, -MAX_LH_CM, -COMMUNITY))


### TRANSFROM DATA FROM LONG TO WIDE FORMAT ###

CompWIDE <- Comp %>%
  spread(SPECIES_CODE, PERCENT_COVER)
CompWIDE[is.na(CompWIDE)] <- 0
CompWIDE <- CompWIDE[-1]

Ref1WIDE <- Ref1 %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Ref1WIDE[is.na(Ref1WIDE)] <- 0
Ref1WIDE <- Ref1WIDE[-1]

Ref2WIDE <- Ref2 %>%
  spread(SPECIES_CODE, PERCENT_COVER)
Ref2WIDE[is.na(Ref2WIDE)] <- 0
Ref2WIDE <- Ref2WIDE[-1]

### CREATE SPECIES ACCUMULATION CURVES (SAC)
CompRichness <- specaccum(CompWIDE)
Ref1Richness <- specaccum(Ref1WIDE)
Ref2Richness <- specaccum(Ref2WIDE)


### PLOT SAC
# plot comp site SAC
plot(CompRichness, ci.type = "poly", lwd = 2, ci.lty = 0, ci.col = rgb(1,0,0,0.1), col = "red", 
     xlab = "# of plots sampled", ylab = "native species richness", ylim = c(0,50), xlim = c(0,30))

# plot reference site SAC
lines(Ref1Richness, ci.type = "poly", ci.col = rgb(0.3,1,0.3,0.1), col = rgb(0,0.7,0), lwd = 2, ci.lty = 0, lty = 2)
lines(Ref2Richness, ci.type = "poly", ci.col = rgb(0.3,0.3,1,0.1), col = "blue", lwd = 2, ci.lty = 0, lty = 2)

# legend
legend(1, 38, legend = c("11-014", "REF-11-2015", "REF-07"),
       col = c("red", rgb(0,0.7,0), "blue"),
       lty = c(1,2,2))




