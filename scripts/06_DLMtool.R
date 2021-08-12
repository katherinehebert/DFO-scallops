# Script to run DLMtool on the scallop dataset

# Setup ------------------------------------------------------------------------

rm(list=ls())

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(DLMtool)  
library(ggplot2)

theme_set(ggpubr::theme_pubclean())

setup()


# Data -------------------------------------------------------------------------

# read the prepared data (generated with 01_dataprep.R)
cpue <- read_csv("data/scallop-cpue.csv")

# make matrix of nsim rows and nyears columns for catch data
catch_matrix <- pivot_wider(subset(cpue, select = c(Year, CPUE_hm)), 
                            names_from = Year,
                            values_from = CPUE_hm,
                            values_fn = function(x) {sum(x, na.rm = TRUE)}) %>% 
  as.matrix()

# make average catch matrix
avc_matrix <- group_by(cpue, Year) %>% 
  summarise(Avc = mean(CPUE_hm, na.rm = TRUE))

# make effort matrix of nsim rows and nyears columns
effort_matrix <- pivot_wider(subset(cpue, select = c(Year, Effort_hm)), 
                             names_from = Year,
                             values_from = Effort_hm,
                             values_fn = function(x) {mean(x, na.rm = TRUE)}) %>% 
  as.matrix()


# create data object
scallops <- new('Data')
# populate the data file
scallops@Name <- "Data"
scallops@Common_Name <- "Scallop"
scallops@LHYear <- max(cpue$Year)
scallops@Units <- "kg/hm"
scallops@Effort <- effort_matrix 
scallops@nareas <- length(unique(cpue$Div))  
scallops@Year <- unique(cpue$Year)
scallops@Cat <- catch_matrix
scallops@AvC <- avc_matrix$Avc
scallops@t <- length(unique(cpue$Year))
scallops@MPeff <- 1 # set to today's effort for comparison
# From the length frequency datase
scallops@L95 <- 40 # length at maturity from FroeseLengthIndicator-scallop.R script
scallops@vbLinf <- 110
scallops@MaxAge <- 20 
# max age from https://www.fisheries.noaa.gov/species/atlantic-sea-scallop#:~:text=Biology,grow%20larger%20than%206%20inches.

# check Data summary
summary(scallops, wait=FALSE, rmd=TRUE)

# Total Allowable Catch --------------------------------------------------------

# which MPs can be applied?
Can(scallops)

# get TAC for each possible MP
TACs <- TAC(scallops)  
boxplot(TACs)

# Stock object -----------------------------------------------------------------

# create and populate stock object
Scallop <- Albacore
# populate the data file
# here, I am basing it on the Albacore Stock object for simplicity,
# and filling it with known values from the datasets,
# though this is not correct!!
Scallop@Name <- "Stock"
Scallop@Common_Name <- "Scallop"
Scallop@maxage <- 20 
Scallop@Linfsd <- c(0.05, 0.1) # arbitrary range 
Scallop@Linf <- c(110, 120) # guesstimate based on max length in the Length dataset
Scallop@Fdisc <- c(0, 0.05) # upper limit: mean MOR/VIV ratio from the mortality dataset
# MaxAge was from  https://www.fisheries.noaa.gov/species/atlantic-sea-scallop#:~:text=Biology,grow%20larger%20than%206%20inches.


# Operating Model --------------------------------------------------------------

OM1 <- new("OM", 
           Scallop, 
           Generic_Fleet, 
           Generic_Obs, 
           Perfect_Imp, 
           nsim = 150)

# plot different elements of the OM
setwd("outputs/")
plot(Scallop)
plot(Generic_Fleet, Scallop)
plot(Generic_Obs)
plot(Perfect_Imp)
plot(OM)

setwd("~/Documents/GitHub/DFO-scallops/")
# run Management Strategy Evaluation
MSE1 <- runMSE(OM1)  
# get summary
summary(MSE1)

# check that the OM is representative
pdf("figures/turing_plot.pdf")
Turing(OM1, scallops, wait=FALSE)
dev.off()

# check convergence
pdf("figures/convergence_plot.pdf")
Converge(MSE1)
dev.off()

# tradeoff plot
pdf("figures/tradeoffplot.pdf")
Tplot2(MSE1)
dev.off()

# wormplot to check if 
pdf("figures/wormplot.pdf")
wormplot(MSE1)
dev.off()

# projection plot
pdf("figures/projection_plot.pdf")
Pplot(MSE1)
dev.off()

# projection plot with chosen variables
pdf("figures/projection_plot.pdf")
Pplot2(MSE1, YVar=c("B_BMSY", "F_FMSY", "Yield"))
dev.off()

# compare to current conditions
pdf("figures/comparison_plot.pdf")
Cplot(MSE1)
dev.off()