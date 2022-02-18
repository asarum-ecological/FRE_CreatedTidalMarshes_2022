#Pie Charts for Site Descriptions

# Import Libraries --------------------------------------------------------

library("ggplot2")
library("ggrepel")
library("tidyverse")
library("scales")


# Data Import -------------------------------------------------------------

# Set this value to your desired site:
site_id <- ""


# Load comp site data
Comp <- read.csv("./Results/2021/VegDataResults_2021.csv", fileEncoding = "UTF-8-BOM")  %>% 
  subset(Site_ID == site_id)


# Data Management ---------------------------------------------------------

# convert to long-format for pie plotting the chart.
CompLong <- Comp %>%
  select(c("n_ra", "e_ra", "i_ra", "u_ra")) %>%
  transmute(Native = n_ra, Introduced = e_ra, Invasive = i_ra, Unknown = u_ra) %>%
  gather(Origin, Percent_Cover, Native:Unknown, factor_key=TRUE) %>%
  arrange(-Percent_Cover)

CompLong$Origin <- factor(CompLong$Origin,
                          levels = CompLong$Origin[order(CompLong$Percent_Cover, decreasing = TRUE)])

CompLong <- CompLong %>%
 mutate(pos = (cumsum(c(0, CompLong$Percent_Cover)) + c(CompLong$Percent_Cover / 2, .01))[1:nrow(CompLong)])


# Create the figure -------------------------------------------------------

# pie chart
piechart <- ggplot(CompLong, aes(x = "", y = Percent_Cover, fill = Origin)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  coord_polar("y")

# Create Blank Theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=15, face="bold")
  )

# Final version of pie chart with blank theme and blue, colour-blind-friendly theme
piechart + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank(),
        legend.title = element_blank(),
        legend.text= element_text(size=14))+
  geom_text_repel(aes(x = 1.4, y = pos, label = percent(Percent_Cover/100, accuracy = 0.1)), 
                  nudge_x = .3, 
                  segment.size = .7, 
                  show.legend = FALSE,
                  size = 5)
