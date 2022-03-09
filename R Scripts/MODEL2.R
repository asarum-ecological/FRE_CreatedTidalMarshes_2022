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
library("MuMIn")


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
  filter(REFERENCE == "No")
#Fraser Ref Site Subset (No Comp Sites)
FREREFSITES <- FRESITES %>%
  filter(REFERENCE == "Yes") 

###RESEARCH QUESTION #2: What factors affect the health of existing marshes?
#for the first angle we are looking at proportional dominance of native species

##Exploratory Plots for Review
#inland
M2.1 <- ggplot(FRECOMPSITES, aes(x=INLAND,y=RC_Native)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Closed Embayment", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#river arm 
M2.2 <- ggplot(FRECOMPSITES, aes(x=ARM,y=RC_Native)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="River Arm", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#project age
M2.3 <- ggplot(FRESITES, aes(x=SAMPLING_AGE,y=RC_Native)) +
  geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Project Age (years)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#proximity
M2.4 <- ggplot(FRECOMPSITES, aes(x=PROX_CHAN,y=RC_Native)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Channel Proximity (m)", y = "Relative % Cover Native") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#km upstream
M2.5 <- ggplot(FRECOMPSITES, aes(x=KM_UPRIVER,y=RC_Native)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Distance Upriver (km)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#elevation
M2.6 <- ggplot(FRECOMPSITES, aes(x=ELEV_ADJ,y=RC_Native)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Elevation (m)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#interaction of elevation an distance upriver
#first have to calculate mean, and mean +/- sd for visualisation
FRECOMPSITES$ELEVATION_2tile <- ntile(FRECOMPSITES$ELEV_ADJ, 2)
FRECOMPSITES$ELEVATION_3tile <- ntile(FRECOMPSITES$ELEV_ADJ, 3)
x <- FRECOMPSITES$ELEV_ADJ

FRECOMPSITES$ELEV_ADJgroup <-
  case_when(x > mean(x)+sd(x) ~ "high",
            x < mean(x)+sd(x) & x > mean(x)-sd(x) ~ "average",
            x < mean(x)-sd(x) ~ "low")

count(FRECOMPSITES,FRECOMPSITES$ELEV_ADJgroup)
FRECOMPSITES$ELEV_ADJgroup <- factor(FRECOMPSITES$ELEV_ADJgroup, levels = c("high", "average", "low"))

#plot 
M2.7 <- FRECOMPSITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = RC_Native, group = ELEV_ADJgroup, color = ELEV_ADJgroup, fill = ELEV_ADJgroup) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "", color = "Elevation") 

M2.7.2 <- FRECOMPSITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = RC_Native, group = ELEV_ADJgroup, color = ELEV_ADJgroup) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), legend.box.margin = margin(-60,-10,0,-80), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "left",
        legend.title = element_text(size=9), legend.text = element_text(size=9),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "Relative % Cover Native", color = "Elevation") 

#legend: note that margin order is "top", "right", "bottom", "left"
M2.7Legend <- get_legend(M2.7.2) 

#creation of panel figure for paper!
M2TopRow <- cowplot::plot_grid(M2.1, M2.2, M2.3, align = "h", axis = "l", ncol =3)
M2MidRow <- cowplot::plot_grid(M2.4,M2.5,M2.6,align = "h",axis = "l", ncol =3)
M2BotRow <- cowplot::plot_grid("",M2.7,M2.7Legend,align = "h",axis = "l", ncol =3)
cowplot::plot_grid(M2TopRow , M2MidRow,M2BotRow, ncol = 1, align = "h")

###MODEL 2:
#elevation*distance upriver is under the assumption that elevation-related stresses are most pronounced at estuary mouth
MODEL2 <- lmer(RC_Native~(INLAND + ARM + SAMPLING_AGE + KM_UPRIVER*ELEV_ADJ + PROX_CHAN) + (1|SITE) + (1|SAMPLE_YEAR),data = FRECOMPSITES)

#SUMMARY DATA
summary(MODEL2) #summary table

#MODEL DIAGNOSTICS
r.squaredGLMM(MODEL2) #evaluating model fit
plot(MODEL2) #checking assumptions
qqnorm(resid(MODEL2)) 
  qqline(resid(MODEL2))
vif(MODEL2) #checking variable inflation factor (VIF)

#MODEL VISUALISATIONS: LIKELY FOR SUPPLEMENTAL MATERIAL 
#plotting how the expected value of the outcome (dominance of natives) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL2, points.par = list(pch = 16, cex = 0.8, col = "red"),type="contrast","PROX_CHAN",xlab = "Channel Proximity (m)", ylab = "Relative % Cover Native")

#plotting interaction effects
visreg(MODEL2,"KM_UPRIVER", by = "ELEV_ADJ", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("Distance Upriver (km)") + ylab("Relative % Cover Native") +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#plotting model coefficients
#names(MODEL2A1$coefficients) <- c('Intercept','Reference Site','Sample Year','North Arm', 'Channel Proximity','Distance Upriver','Elevation', 'Distance Upriver:Elevation')
plot_model(MODEL2, show.values = TRUE, value.offset = .3, title = "Relative % Cover Native", ci.lvl = .95,sort.est = TRUE,
           axis.labels = c('Closed Embayment [Yes]','Elevation (m)',"Distance Upriver (km)",'Project Age (Years)','Arm [North]','Channel Proximity (m)',"Distance Upriver:Elevation")) 

#produce model summary table html that can be copied into report 
tab_model(MODEL2)
