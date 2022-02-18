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
  filter(REFERENCE == "NO") 
#Fraser Ref Site Subset (No Comp Sites)
FREREFSITES <- FRESITES %>%
  filter(REFERENCE == "YES") 

###RESEARCH QUESTION #2: What factors affect the health of existing marshes?
#for the first angle we are looking at proportional dominance of native species

##Exploratory Plots for Review
#inland
M2.1 <- ggplot(FRECOMPSITES, aes(x=INLAND,y=RC_Native)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Closed Embayment", y = "Relative % Cover Native") +
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
M2.6 <- ggplot(FRECOMPSITES, aes(x=ELEVATION,y=RC_Native)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  labs(x ="Elevation (m)", y = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#interaction of elevation an distance upriver
#first have to calculate mean, and mean +/- sd for visualisation
FRECOMPSITES$ELEVATION_2tile <- ntile(FRECOMPSITES$ELEVATION, 2)
FRECOMPSITES$ELEVATION_3tile <- ntile(FRECOMPSITES$ELEVATION, 3)
x <- FRECOMPSITES$ELEVATION

FRECOMPSITES$ELEVATION3group <-
  case_when(x > mean(x)+sd(x) ~ "high",
            x < mean(x)+sd(x) & x > mean(x)-sd(x) ~ "average",
            x < mean(x)-sd(x) ~ "low")

count(FRECOMPSITES,FRECOMPSITES$ELEVATION3group)
FRECOMPSITES$ELEVATION3group <- factor(FRECOMPSITES$ELEVATION3group, levels = c("high", "average", "low"))

#plot 
M2.7 <- FRECOMPSITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = RC_Native, group = ELEVATION3group, color = ELEVATION3group, fill =ELEVATION3group) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "Relative % Cover Native", color = "Elevation") 

M2.7.2 <- FRECOMPSITES %>%
  ggplot() +
  aes(x = KM_UPRIVER, y = RC_Native, group = ELEVATION3group, color = ELEVATION3group) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), legend.box.margin = margin(-60,-10,0,-80), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "left",
        legend.title = element_text(size=9), legend.text = element_text(size=9),legend.background = element_blank()) +
  labs(x ="Distance Upriver (km)", y = "Relative % Cover Native", color = "Elevation") 

#legend: note that margin order is "top", "right", "bottom", "left"
M2.7Legend <- get_legend(M2.7.2) 

#channel proximity and elevation interaction
M2.8 <- FRECOMPSITES %>%
  ggplot() +
  aes(x = PROX_CHAN, y = RC_Native, group = ELEVATION3group, color = ELEVATION3group, fill =ELEVATION3group) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",
        legend.title = element_text(size=10), legend.text = element_text(size=10),legend.background = element_blank()) +
  labs(x ="Channel Proximity (m)", y = "", color = "Elevation") 


#creation of panel figure for paper!
M2TopRow <- cowplot::plot_grid(M2.1, M2.2, M2.3, align = "h", axis = "l", ncol =3)
M2MidRow <- cowplot::plot_grid(M2.4,M2.5,M2.6,align = "h",axis = "l", ncol =3)
M2BotRow <- cowplot::plot_grid(M2.7,M2.8,M2.7Legend,align = "h",axis = "l", ncol =3)
cowplot::plot_grid(M2TopRow , M2MidRow,M2BotRow, ncol = 1, align = "h")

###MODEL 2:
#currently two interactions are included: elevation*distance upriver and arm*distance upriver
#elevation*distance upriver is under the assumption that elevation-related stresses are most pronounced at estuary mouth
#arm*distance upriver is under the assumption that salinity/tide related stressors are more pronounced in the North Arm than Main
# Formula for same model, sans cattail-present sites 
MODEL2 <- lmer(RC_Native~(INLAND + ARM + SAMPLING_AGE + KM_UPRIVER*ELEVATION + PROX_CHAN*ELEVATION) + (1|SITE) + (1|SAMPLE_YEAR),data = FRECOMPSITES)
MODEL3 <- lmer(NAT_RICH~(INLAND + ARM + REFERENCE + PROX_CHAN + KM_UPRIVER*ELEVATION) + (1|SITE) + (1|SAMPLE_YEAR),data = FRESITES)
#SUMMARY DATA
summary(MODEL2) #summary table


#getting model fit data
r.squaredGLMM(MODEL2) 

#CHECKING MODEL ASSUMPTIONS
plot(MODEL2) #looks good, no patterns evident
qqnorm(resid(MODEL2)) 
  qqline(resid(MODEL2)) #points fall along line, look good

#checking variable inflation factor (VIF)
vif(MODEL2)

#MODEL VISUALISATIONS: LIKELY FOR SUPPLEMENTAL MATERIAL 
#plotting how the expected value of the outcome (% marsh) changes as a function of x, with all other variables in the model held fixed.
visreg(MODEL2, points.par = list(pch = 16, cex = 0.8, col = "red"),type="contrast","INLAND",xlab = "Closed Embayment", ylab = "Relative % Cover Native")

#plotting interaction effects
visreg(MODEL2,"KM_UPRIVER", by = "ELEVATION", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("Distance Upriver (km)") + ylab("Relative % Cover Native") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
visreg(MODEL2,"PROX_CHAN", by = "ELEVATION", overlay=TRUE,partial = FALSE, gg=TRUE) + 
  theme_bw()+
  xlab("Channel Proximity (m)") + ylab("Relative % Cover Native") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#COEFFICIENT PLOT
set_theme(base = theme_classic()) #To remove the background color and the grids
#plotting model coefficients
#names(MODEL2A1$coefficients) <- c('Intercept','Reference Site','Sample Year','North Arm', 'Channel Proximity','Distance Upriver','Elevation', 'Distance Upriver:Elevation')
plot_model(MODEL2, show.values = TRUE, value.offset = .3, title = "Relative % Cover Native", ci.lvl = .95,sort.est = TRUE,
           axis.labels = c('Closed Embayment [Y]','Elevation (m)',"Distance Upriver (km)",'Project Age (Years)','Elevation:Channel Proximity','Arm [North]',"Distance Upriver:Elevation",'Channel Proximity (m)')) 

#produce model summary table html that can be copied into report 
tab_model(MODEL2)


#OLD CODE (may need later)
#STANDARDIZING DATA FOR MODELS

#standardize continuous variables to be centered on the mean (mean becomes 0) using the standardize function from robustHD  
FRECOMPSITES$SAMPLING_AGEs <-standardize(FRECOMPSITES$SAMPLING_AGE, centerFun = mean, scaleFun = sd)
FRECOMPSITES$AREA_MAPPEDs <-standardize(FRECOMPSITES$AREA_MAPPED, centerFun = mean, scaleFun = sd)
FRECOMPSITES$KM_UPRIVERs <-standardize(FRECOMPSITES$KM_UPRIVER, centerFun = mean, scaleFun = sd)
FRECOMPSITES$ELEVATIONs <-standardize(FRECOMPSITES$ELEVATION, centerFun = mean, scaleFun = sd)
FRECOMPSITES$PROX_CHANs <-standardize(FRECOMPSITES$PROX_CHAN, centerFun = mean, scaleFun = sd)
#FRECOMPSITES$CARELYN_MH <-standardize(FRECOMPSITES$CARELYN_MH, centerFun = mean, scaleFun = sd)

#standardize continuous variables to be centered on the mean (mean becomes 0) using the standardize function from robustHD  
FRESITES$SAMPLING_AGEs <-standardize(FRESITES$SAMPLING_AGE, centerFun = mean, scaleFun = sd)
FRESITES$AREA_MAPPEDs <-standardize(FRESITES$AREA_MAPPED, centerFun = mean, scaleFun = sd)
FRESITES$KM_UPRIVERs <-standardize(FRESITES$KM_UPRIVER, centerFun = mean, scaleFun = sd)
FRESITES$ELEVATIONs <-standardize(FRESITES$ELEVATION, centerFun = mean, scaleFun = sd)
FRESITES$PROX_CHANs <-standardize(FRESITES$PROX_CHAN, centerFun = mean, scaleFun = sd)
FRESITES$CARELYN_MHs <-standardize(FRESITES$CARELYN_MH, centerFun = mean, scaleFun = sd)


