# Script to use DLM tool to evaluate strategies for scallops based on CPUE

# === Setup =====================================

rm(list=ls())

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(DLMtool)  

theme_set(ggpubr::theme_pubclean())

#setup()

# read the prepared data
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
scallops@MPeff <- 1 # set to today's effort for comparisons (?)
saveRDS(scallops, "outputs/Data_DLMtool_cpue.rds")

# see summary plot
summary(scallops, wait=FALSE, rmd=TRUE)

## applying MPS ----------------------------------------------------------------

# which MPs can be applied?
Can(scallops)
# which can't?
Cant(scallops)

## Apply MPs -------------------------------------------------------------------

TACs <- TAC(scallops)  
TACs@MPs
boxplot(TACs)
plot(TACs)
saveRDS(TACs, "outputs/TACs_DLMtool_cpue.rds")

# Apply input control MPs ------------------------------------------------------

Input(scallops)

# Sensitivity analyses on TAC recommendations ----------------------------------

scallops <- Sense(scallops, "AvC")
scallops <- Sense(scallops, "CC1")
