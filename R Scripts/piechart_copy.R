#Pie Charts for Site Descriptions

# import libraries
library("ggplot2")
library("tidyverse")
library("scales")

### IMPORT DATA ###
# edit file paths as necessary

# Compensation Site Data
Comp <- read.csv("~/Documents/R/CompSites/Results/2021/09-003-results.csv", fileEncoding = "UTF-8-BOM")

#Converting to Long Format for Chart Creation
CompLong <- select(Comp,natives, exotics, invasives) 
CompLong <-  gather(CompLong, Origin, Percent_Cover, natives:invasives, factor_key=TRUE)
CompLong$Percent_Cover=CompLong$Percent_Cover*100

#pie chart
chart1 <- ggplot(CompLong, aes(x="", y=Percent_Cover, fill = Origin)) +
  geom_bar(width = 1, stat = "identity")
chart1
piechart <- chart1 + coord_polar("y", start = 0)
piechart

#Create Blank Theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=15, face="bold")
  )

#Final version of pie chart with blank theme and blue, colour-blind-friendly theme
piechart + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank(), 
        legend.title = element_blank(),
        legend.text= element_text(size=14))+
  geom_text(aes(y = Percent_Cover/3 + c(0, cumsum(Percent_Cover)[-length(Percent_Cover)]), 
                label = percent(Percent_Cover/100)), size=5)