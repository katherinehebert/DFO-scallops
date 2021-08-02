# Script to use DLM tool to evaluate strategies for scallops based on length

# === Setup =====================================

rm(list=ls())

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(DLMtool)  

theme_set(ggpubr::theme_pubclean())

setup()

# read the prepared data
df <- read_csv("data/scallop-sizes.csv")

# make catch matrix
catch_matrix <- subset(df, select = c(Year, nlengthm2)) %>% 
  pivot_wider(names_from = Year,
              values_from = nlengthm2,
              values_fn = function(x) {sum(x, na.rm = TRUE)}) %>%
  as.matrix()

# make average catch matrix
avc_matrix <- group_by(df, Year) %>% 
  summarise(Avc = mean(nlengthm2, na.rm = TRUE))


# make relative abundance matrix
ra_matrix <- group_by(df, Year, Length) %>%
  summarise(Ind = sum(Percent, na.rm = TRUE)) %>%
  as.matrix()

# create data object
scallops_length <- new('Data')
# populate the data file
scallops_length@Name <- "Data"
scallops_length@Common_Name <- "Scallop"
scallops_length@LHYear <- max(df$Year)
scallops_length@Units <- "m2"
scallops_length@nareas <- length(unique(df$Sector))  
scallops_length@Year <- unique(df$Year)
scallops_length@LFC <- df$Length
scallops_length@Cat <- catch_matrix
scallops_length@AvC <- avc_matrix
scallops_length@Ind <- ra_matrix

summary(scallops_length, wait=FALSE, rmd=TRUE)

# which MPs can be applied?
Can(scallops_length)
# which can't?
Cant(scallops_length)

## Apply MPs -------------------------------------------------------------------

TACs <- TAC(scallops_length)  
TACs@MPs
boxplot(TACs)
plot(TACs)

# Apply input control MPs ------------------------------------------------------

Input(scallops_length)

# Sensitivity analyses on TAC recommendations ----------------------------------

scallops <- Sense(scallops_length, "AvC")
scallops <- Sense(scallops_length, "CC1")

# run MP

runMP(scallops_length, c("AvC"))


# build an operating model

OM <- testOM
OM@nsim <- 200
MSE <- runMSE(OM, parallel = TRUE)

myMSE <- runMSE(OM=myOM

avail("Stock")
avail("Fleet")
avail("Obs")
avail("Imp")
myOM <- new("OM", Blue_shark, Generic_Fleet, Generic_Obs, Perfect_Imp)


avail("OM")

MSE3<-runMSE(testOM)

NOAA_plot(MSE3)
