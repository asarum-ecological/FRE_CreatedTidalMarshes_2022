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
library("performance")

citation("performance")
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
#Native Richness

#Exploratory Plots

#reference
M3.1 <- ggplot(FRESITES, aes(x=REFERENCE,y=NAT_RICH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  ylim(0,14) +
  labs(x ="Reference Site", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#arm
M3.2 <- ggplot(FRESITES, aes(x=ARM,y=NAT_RICH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  ylim(0,14) +
  labs(x ="River Arm", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#embayment
M3.3 <- ggplot(FRESITES, aes(x=INLAND,y=NAT_RICH)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  ylim(0,14) +
  labs(x ="Closed Embayment", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#proximity
M3.4 <- ggplot(FRESITES, aes(x=PROX_CHAN,y=NAT_RICH)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  ylim(0,14) +
  labs(x ="Channel Proximity (m)", y = "Native Richness/Plot") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#km upstream
M3.5 <- ggplot(FRESITES, aes(x=KM_UPRIVER,y=NAT_RICH)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  ylim(0,14) +
  labs(x ="Distance Upriver (km)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#elevation
M3.6 <- ggplot(FRESITES, aes(x=ELEV_ADJ,y=NAT_RICH)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = 'lm') +
  labs(x ="Elevation (m)", y = "") +
  ylim(0,14) +
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

count(FRESITES,FRESITES$ELEVATION3group)
FRESITES$ELEV_ADJ3group <- factor(FRESITES$ELEV_ADJ3group, levels = c("high", "average", "low"))

#plot 
M3.7 <- FRESITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = NAT_RICH, group = ELEV_ADJ3group, color = ELEV_ADJ3group, fill =ELEV_ADJ3group) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "lm") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "", color = "Elevation") 

M3.7.2 <- FRESITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = NAT_RICH, color = ELEV_ADJ3group) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "lm") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), legend.box.margin = margin(-60,0,0,-10),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "left",
        legend.title = element_text(size=9), legend.text = element_text(size=9),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "", color = "Elevation") 

#legend: note that margin order is "top", "right", "bottom", "left"
M3.7Legend <- get_legend(M3.7.2) 


#interaction of elevation an distance upriver
#M3.7 <- FRESITES %>%
  ggplot() +
  aes(x = PROX_CHAN, y = NAT_RICH, group = ELEV_ADJ3group, color = ELEV_ADJ3group, fill =ELEV_ADJ3group) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "lm") +
  ylim(0,14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Channel Proximity (m)", y = "", color = "Elevation") 

###MODEL 3:
#elevation*distance upriver is under the assumption that elevation-related stresses are most pronounced at estuary mouth
MODEL3 <- lmer(NAT_RICH~(INLAND + ARM + REFERENCE + PROX_CHAN + KM_UPRIVER*ELEV_ADJ) + (1|SITE) + (1|SAMPLE_YEAR),data = FRESITES)

#SUMMARY DATA
summary(MODEL3)

#MODEL DIAGNOSTICS
r.squaredGLMM(MODEL3) #evaluating model fit
plot(MODEL3) #checking assumptions
qqnorm(resid(MODEL3)) 
  qqline(resid(MODEL3)) 
  r.squaredGLMM(MODEL3)
vif(MODEL3)#checking variable inflation factor (VIF)

#MODEL VISUALISATIONS: LIKELY FOR SUPPLEMENTAL MATERIAL 
#plotting how the expected value of the outcome (native richness) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL3, points.par = list(pch = 16, cex = 0.8, col = "red"),type="contrast","ELEV_ADJ", xlab = "Elevation (m)",ylab = "Native Richness/plot")

#plotting interaction effect
visreg(MODEL3,"KM_UPRIVER", by = "ELEV_ADJ", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("Distance Upriver (km)") + ylab("Native Richness/plot") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#ploting model coefficients
#names(MODEL2A1$coefficients) <- c('Intercept','Reference Site','Sample Year','North Arm', 'Channel Proximity','Distance Upriver','Elevation', 'Distance Upriver:Elevation')
plot_model(MODEL3, show.values = TRUE, value.offset = .3, title = "Native Richness/plot", ci.lvl = .95,sort.est = TRUE,
           axis.labels = c('Closed Embayment [Yes]','Distance Upriver:Elevation','Reference [Yes]',"Channel Proximity (m)",'Distance Upriver (km)','Arm [North]','Elevation (m)')) +
  ylim(-2,2)
  
#table for appendix
tab_model(MODEL3)


