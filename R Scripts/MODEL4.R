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


#LOADING MASTER DATA .CSV 
MASTERDATA <- read.csv("~/Documents/R/CompSites/FieldData/MODELS2-4/PLOTDATA_MASTER.csv") 

#Ensuring Sample Year, Reference, and Project Type are factors, and ordering dummy variables as preferred
MASTERDATA$SAMPLE_YEAR <- as.factor(MASTERDATA$SAMPLE_YEAR)
MASTERDATA$REFERENCE <- as.factor(MASTERDATA$REFERENCE)
MASTERDATA$SITE <- as.factor(MASTERDATA$SITE)
MASTERDATA$ELEAVATION <- as.numeric(MASTERDATA$ELEVATION)
MASTERDATA$RC_Invasive <- as.numeric(MASTERDATA$RC_Invasive)
MASTERDATA$TYPE <- factor(MASTERDATA$TYPE, levels = c("Other", "Basin", "Embayment", "Inline", "Protruding"))

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

###RESEARCH QUESTION #2: What factors affect the health of existing marshes?
#Non-Native Richness

#Exploratory Plots

#reference
M4.1 <- ggplot(FRESITES, aes(x=REFERENCE,y=NN_RICH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  labs(x ="Reference Site", y = "") +
  ylim(0,14)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#arm
M4.2 <- ggplot(FRESITES, aes(x=ARM,y=NN_RICH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  labs(x ="River Arm", y = "") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#inland
M4.3 <- ggplot(FRESITES, aes(x=INLAND,y=NAT_RICH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  ylim(0,14) +
  labs(x ="Closed Embayment", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#proximity
M4.4 <- ggplot(FRESITES, aes(x=PROX_CHAN,y=NN_RICH)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  labs(x ="Channel Proximity (m)", y = "Non-Native Richness/Plot") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#km upstream
M4.5 <- ggplot(FRESITES, aes(x=KM_UPRIVER,y=NN_RICH)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  labs(x ="Distance Upriver (km)", y = "") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#elevation
M4.6 <- ggplot(FRESITES, aes(x=ELEV_ADJ,y=NN_RICH)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  labs(x ="Elevation (m)", y = "") +
  ylim(0,14)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#interaction of elevation an distance upriver
#first have to calculate mean, and mean +/- sd for visualisation
FRESITES$ELEV_ADJ_2tile <- ntile(FRESITES$ELEV_ADJ, 2)
FRESITES$ELEV_ADJ_3tile <- ntile(FRESITES$ELEV_ADJ, 3)
x <- FRESITES$ELEV_ADJ

FRESITES$ELEV_ADJ3group <-
  case_when(x > mean(x)+sd(x) ~ "high",
            x < mean(x)+sd(x) & x > mean(x)-sd(x) ~ "average",
            x < mean(x)-sd(x) ~ "low")

count(FRESITES,FRESITES$ELEV_ADJ3group)
FRESITES$ELEV_ADJ3group <- factor(FRESITES$ELEV_ADJ3group, levels = c("high", "average", "low"))

#plot 
M4.7 <- FRESITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = NN_RICH, group = ELEV_ADJ3group, color = ELEV_ADJ3group, fill =ELEV_ADJ3group) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "lm") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "", color = "Elevation") 

M4.7.2 <- FRESITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = NN_RICH, color = ELEV_ADJ3group) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), legend.box.margin = margin(0,0,10,25),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "left",
        legend.title = element_text(size=9), legend.text = element_text(size=9),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "", color = "Elevation") 

#legend: note that margin order is "top", "right", "bottom", "left"
M4.7Legend <- get_legend(M4.7.2) 


#interaction of channel proximity and elevation
M4.8 <- FRESITES %>%
  ggplot() +
  aes(x = PROX_CHAN, y = NN_RICH, group = ELEV_ADJ3group, color = ELEV_ADJ3group, fill =ELEV_ADJ3group) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "lm") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Channel Proximity (m)", y = "", color = "Elevation") 

###MODEL 4:
#currently two interactions are included: elevation*distance upriver and arm*distance upriver
#elevation*distance upriver is under the assumption that elevation-related stresses are most pronounced at estuary mouth
#arm*distance upriver is under the assumption that salinity/tide related stressors are more pronounced in the North Arm than Main
# Formula for same model, sans cattail-present sites 
MODEL4 <- lmer(NN_RICH~(INLAND + ARM + REFERENCE + PROX_CHAN + KM_UPRIVER*ELEV_ADJ) + (1|SITE) + (1|SAMPLE_YEAR),data = FRESITES, REML = TRUE)

#SUMMARY DATA
summary(MODEL4)

#MODEL DIAGNOSTICS
r.squaredGLMM(MODEL4) #evaluating model fit
plot(MODEL4) 
qqnorm(resid(MODEL4)) 
qqline(resid(MODEL4)) 
vif(MODEL4)#checking variable inflation factor (VIF)

#MODEL VISUALISATIONS: LIKELY FOR SUPPLEMENTAL MATERIAL 
#plotting how the expected value of the outcome (non-native richness) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL4, points.par = list(pch = 16, cex = 0.8, col = "red"),type="contrast","ELEV_ADJ", xlab=("Elevation (m)"), ylab="Non-Native Richness/Plot")

#plotting interaction effect
visreg(MODEL4,"KM_UPRIVER", by = "ELEV_ADJ", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("Distance Upriver (km)") + ylab("Non-Native Richness/Plot") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#ploting model coefficients
#names(MODEL2A1$coefficients) <- c('Intercept','Reference Site','Sample Year','North Arm', 'Channel Proximity','Distance Upriver','Elevation', 'Distance Upriver:Elevation')
plot_model(MODEL4, show.values = TRUE, value.offset = .3, title = "Non-Native Richness/plot", ci.lvl = .95,sort.est = TRUE,
           axis.labels = c('Reference [Yes]',"Closed Embayment [Yes]",'Distance Upriver: Elevation','Channel Proximity (m)','Arm [North]','Distance Upriver (km)','Elevation (m)')) +
  ylim(-2,2)

#table for appendix
tab_model(MODEL4)

