### Froese Length Indicators ###
# Developed by Froese, R. in 2004 (Source: https://core.ac.uk/download/pdf/11897517.pdf)

# Uses length frequencies in commercial fishery to estimate condition indicators  # 
# that provide information on the different important ones for population renewal #

############################################################################################
### With scallop length composition ########################################################
############################################################################################

# This script was adapted from https://github.com/MathBoud/DLM.ReferencePoint/blob/master/R/FroeseLengthIndicator.R

rm(list=ls())

#### Indicators with length data from DFO scallop survey ####

# Load require packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggpubr)

# set all ggplot themes
theme_set(ggpubr::theme_pubclean())

# Load number-at-length data
NAL <- read.csv("data/scallop-sizes.csv")

# Bubble plot of length frequency ##############################################

# plot the full distribution of counts
NAL %>%
  ggplot(aes(x = Year, y = Length, size = Count)) +
  geom_point(pch = 21, alpha = 1, fill = "seagreen3") +
  scale_size(range = c(1, 15), name = "Nb of specimen") +
  labs(x = "Year", y = "Length (cm)", size = "Nb of specimen")

# summed counts
NALsum <- NAL %>% 
  group_by(Year, Length) %>%
  summarise(Count = sum(Count, na.rm = TRUE))

# plot the summed counts
NALsum %>%
  ggplot(aes(x = Year, y = Length, size = Count, fill = Count)) +
  geom_point(pch = 21, alpha = 1) +
  labs(x = "Year", y = "Length (cm)", size = "Nb of specimen") +
  # make sure the sizes represent the correct area scale
  ggplot2::scale_size_area(name = "Nb of specimen") +
  scale_fill_viridis_c()

# Histogram of the number of specimen by length for each year ##################
ggplot(NAL,
       aes(x = Length, y = Count)) +
  geom_bar(stat = "identity") +
  ylab("Frequency") +
  facet_wrap(~ Year, strip.position = "top")

#### Froese Sustainability Indicators ####

# Requires known values of length at maturity (Lmat) and maximum length (Lmax) in cm
Lmax = 110 # or 113. 120 may be an error 
Lmat = 40 # at 5 years!   

#### % of mature scallops in DFO survey length sampling ####

# count of mature scallops
DFO.PopMat.Year <- NAL %>% filter(Length > Lmat) %>% 
  group_by(Year) %>%
  summarize(total.mat=sum(Count)) %>%
  as.data.frame()
# count of total scallops
DFO.PopTot.Year <- NAL %>% 
  group_by(Year) %>%
  summarize(total=sum(Count)) %>%
  as.data.frame()
# join together
DFO.Mat <- left_join(DFO.PopTot.Year, DFO.PopMat.Year, by = "Year")
# calculate % mature scallops
DFO.Mat$PourcPopMat <- DFO.Mat$total.mat/DFO.Mat$total*100

# regression
fit <- lm(PourcPopMat ~ Year, data = DFO.Mat)

# plot them
ggplot(data = DFO.Mat) +
  geom_line(aes(y = PourcPopMat, x = Year)) +
  geom_smooth(aes(y = PourcPopMat, x = Year), method = "lm") +
  coord_cartesian(ylim = c(50,100)) +
  labs(y = "% Scallops at Maturity",
       caption = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
ggsave("figures/Percent_MatureScallops.png", width = 6, height = 4)