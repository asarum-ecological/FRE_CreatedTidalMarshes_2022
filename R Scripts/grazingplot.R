library("ggplot2")
library("tidyverse")
library("car")
library("visreg") #for model visualisation
library("robustHD") #for model visualisation
library("sjPlot") #for model visualisation
library("cowplot") #for panel plots


#This code was developed to create a grazing figure (currently Fig. 7)

#Loading Data for Plot #1
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/MODEL2_DATACLEAN.csv") 

##Creating Subset Objects for Later Models 
#All Fraser Only 
FRESITES <- MASTERDATA %>%
  filter(RIVER == "Fraser") 
#Fraser Comp Site Subset (No REF Sites) 
FRECOMPSITES <- FRESITES %>%
  filter(REFERENCE == "NO") 
#Fraser Ref Site Subset (No Comp Sites)
FREREFSITES <- FRESITES %>%
  filter(REFERENCE == "YES") 

#PLOT1: Lyngbye's Sedge Max Height
LyngbyePlot <- ggplot(FRECOMPSITES, aes(x=INLAND,y=CARELYN_MH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = 'lm') +
  labs(x ="Inland Site", y = "Lyngbye's Sedge Max Height/plot (cm)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#Loading Data for Plot #2 (different dataset)
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/SiteData_Master.csv") 
FRESITES <- MASTERDATA %>%
  filter(RIVER == "Fraser") 
FRESITES$GRAZING = as.factor(FRESITES$GRAZING)
FRESITES$INLAND = as.factor(FRESITES$INLAND)
FRESITES=FRESITES[!is.na(FRESITES$GRAZING),]

#Plot 2: Project type and herbivory grazing
InlandGraze <- ggplot(FRESITES, aes(x=GRAZING, fill = INLAND)) +
  geom_bar() +
  labs(x ="Grazing Intensity", y="# of sites") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  scale_x_discrete(labels=c("0" = "None", "1" = "Low",
                            "2" = "Moderate","3" = "High")) +
 scale_fill_discrete(name = "Inland Site")

InlandGrazeL <- ggplot(FRESITES, aes(x=GRAZING, fill = INLAND)) +
  geom_bar() +
  labs(x ="Grazing Intensity", y="# of sites") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("0" = "None", "1" = "Low",
                            "2" = "Moderate","3" = "High")) +
  scale_fill_discrete(name = "Inland Design")

LEGEND <- get_legend(InlandGrazeL)

cowplot::plot_grid(InlandGraze,LEGEND,LyngbyePlot, align = "h",ncol = 3, rel_widths = c(1,.5,1))
