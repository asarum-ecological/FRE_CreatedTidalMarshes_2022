library("ggplot2")
library("tidyverse")
library("car")
library("visreg")
library("robustHD")
library("sjPlot")
library("lme4")
library("GGally")
library("knitr")
library("piecewiseSEM")
library("cowplot")
library("MuMIn")
library("LMERConvenienceFunctions")
library("reshape2")
library("rstatix")
library("lmerTest")
library("fitdistrplus")
library("ggpubr")
library("emmeans")
#Loading Libraries


#NOTE: THIS IS A WORK IN PROGRESS!!!

#colour blind color palette for figure
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#LOADING FREQFIG DATA .CSV 
FREFIG <- read.csv("~/Documents/R/CompSites/FieldData/InvDomFigure.csv") 
FRESITES <- FREFIG %>%
  filter(RIVER == "Fraser"| RIVER == "Pitt") 

#Converting to long form for figures
#all sites
FRELONG <- gather(FRESITES,SPECIES,PERCENT_FREQ, IRISPSE:TYPHGLA)
FRELONG <- FRELONG[complete.cases(FRELONG$PERCENT_FREQ),]
FRELONG$SPECIES <- as.factor(FRELONG$SPECIES)
FRELONG <- FRELONG[(FRELONG$PERCENT_FREQ > 0.0),]

PLOT1 <- ggplot(FRELONG, aes(x=KM_UPRIVER,y=ELEVATION, colour = SPECIES)) +
  geom_point(aes(colour = SPECIES),alpha = .5) +
  geom_smooth(aes(colour = SPECIES),alpha = .3) +
  ylim(-1,3) +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette,labels =c("Yellow Iris","Purple Loosestrife","Reed Canarygrass","Non-Native Cattail")) +
  labs(x ="Distance Upriver (km)", y = "Elevation (m)") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

PLOT2 <- ggplot(FRELONG, aes(x=SPECIES,y=ELEVATION, colour= SPECIES)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  labs(x ="Species", y = "Relative % Cover") +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette,labels =c("Yellow Iris","Purple Loosestrife","Reed Canarygrass","Non-Native Cattail")) +
  labs(x ="Species", y = "") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank())

PLOT2B <- ggplot(FRELONG, aes(x=SPECIES,y=ELEVATION, colour= SPECIES)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  labs(x ="Species", y = "Relative % Cover") +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette,labels =c("Yellow Iris","Purple Loosestrife","Reed Canarygrass","Non-Native Cattail")) +
  labs(x ="Species", y = "") +
  theme(legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank())

LEGEND <- get_legend(PLOT2B)


cowplot::plot_grid(PLOT1,LEGEND,PLOT2, ncol = 3, rel_widths = c(1,.5,1))

