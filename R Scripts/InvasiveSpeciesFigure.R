library("ggplot2")
library("tidyverse")
library("tidyr")
library("cowplot")

#this code was used to create Figure 9 of the discussion

#LOADING FREQFIG DATA .CSV 
FREQFIG <- read.csv("~/Documents/R/CompSites/FieldData/FrequencyFigure.csv") 
FRESITES <- FREQFIG %>%
  filter(RIVER == "Fraser"| RIVER == "Pitt") 
FRECOMPSITES <- FRESITES %>%
  filter(REFERENCE == "NO") 

#Converting to long form for figures
#all sites
FRELONG <- gather(FRESITES,SPECIES,PERCENT_FREQ, CARELYN:TYPHGLA)
FRELONG$SPECIES <- as.factor(FRELONG$SPECIES)

#compsites
COMPLONG <- gather(FRECOMPSITES,SPECIES,PERCENT_FREQ, CARELYN:TYPHGLA)
COMPLONG$SPECIES <- as.factor(COMPLONG$SPECIES)

#colour blind color palette for figure
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Figure 1: Scatter plot showing % frequency of invasives with distance upriver
PLOT1 <- ggplot(FRELONG, aes(x=DIST_UPRIVER_KM,y=PERCENT_FREQ, colour = SPECIES)) +
  geom_point(aes(colour = SPECIES),alpha = .5) +
  geom_smooth(aes(colour = SPECIES),alpha = .3) +
  ylim(0,100) +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette,labels =c("Lyngbye's Sedge","Yellow Iris","Purple Loosestrife","Reed Canarygrass","Non-Native Cattail")) +
    labs(x ="Distance Upriver (km)", y = "% Frequency of Plots") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#LOADING FREQFIG DATA .CSV 
DOMFIG <- read.csv("~/Documents/R/CompSites/FieldData/InvDomFigure.csv") 
DOMFIG[DOMFIG== 0] <- NA
summary(DOMFIG$TYPHGLA)
sd(DOMFIG$TYPHGLA,na.rm=TRUE)


FREDOM <- DOMFIG %>%
  filter(RIVER == "Fraser"| RIVER == "Pitt") 

DOMLONG <- gather(FREDOM,SPECIES,RELATIVE_COV, IRISPSE:TYPHGLA) 
DOMLONG$RELATIVE_COV = as.numeric(DOMLONG$RELATIVE_COV)
DOMLONG[DOMLONG == 0] <- NA

#Figure 2: Scatterplot showing % frequency of invasives with distance upriver

PLOT2 <- ggplot(DOMLONG, aes(x=SPECIES,y=RELATIVE_COV, colour= SPECIES)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  labs(x ="Species", y = "Relative % Cover") +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette,labels =c("Yellow Iris","Purple Loosestrife","Reed Canarygrass","Non-Native Cattail")) +
  labs(x ="Species", y = "Relative % Cover") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank())
 
PLOT2B <- ggplot(DOMLONG, aes(x=SPECIES,y=RELATIVE_COV, colour= SPECIES)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  labs(x ="Species", y = "Relative % Cover") +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette,labels =c("Yellow Iris","Purple Loosestrife","Reed Canarygrass","Non-Native Cattail")) +
  labs(x ="Species", y = "Relative % Cover (when present)") +
  theme(legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

LEGEND <-get_legend(PLOT2B)


cowplot::plot_grid(PLOT1,LEGEND,PLOT2, ncol = 3, rel_widths = c(1,.5,1))

  