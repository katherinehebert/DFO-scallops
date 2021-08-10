# Script to use DLM tool to evaluate strategies for scallops based on mortality

# === Setup =====================================

rm(list=ls())

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(DLMtool)  
library(ggplot2)

theme_set(ggpubr::theme_pubclean())

setup()

# read the prepared data
df <- read_csv("data/scallop-mortality.csv")

### group by sector-zone-year to match sizes and mortality #####################

# subset to data from 2004-2019
df <- df %>% filter(annee %in% 2004:2019)

# group and summarize mean density of scallops by length

dfmean <- df %>%
  group_by(secteur, zone, annee, stat, taille) %>%
  summarise(mean_ntailm2 = mean(ntailm2, na.rm = TRUE)) %>% 
  ungroup()

# visualise to check
ggplot(dfmean) +
  geom_histogram(aes(x = mean_ntailm2, fill = stat)) +
  facet_wrap(~secteur)
# as barplot
ggplot(dfmean) +
  geom_bar(aes(x = as.character(annee), y = mean_ntailm2, fill = stat), 
           stat = "identity", position = "dodge", width = .6) +
  facet_wrap(~secteur) +
  labs(x = "Year", y = "Mean number per lengthm2", fill = "Status")

# calculate mort/vivant ratio per size
dfratio.size <- dfmean %>%
  group_by(secteur, zone, stat, annee, taille) %>%
  pivot_wider(names_from = stat, values_from = mean_ntailm2) %>%
  mutate(ratio = MOR/VIV)
dfratio.size$ratio[is.nan(dfratio.size$ratio)] <- 0
# save to outputs
saveRDS(dfratio.size, "data/MORVIVratio.rds")

# # plot by size
# ggplot(dfratio.size) +
#   #geom_bar(aes(x = annee, y = ratio), stat = "identity") +
#   geom_point(aes(x = annee, y = ratio, col = taille)) +
#   facet_wrap(~secteur) +
#   scale_color_distiller(palette = "Purples", direction = -1)
# ggplot(dfratio.size) +
#   geom_boxplot(aes(x = as.character(annee), y = ratio)) +
#   facet_wrap(~secteur)


# library(ggridges)
# ggplot(filter(dfratio.size, secteur == "A")) +
#   ggridges::geom_density_ridges(aes(y = as.character(annee), 
#                                     x = ratio, 
#                                     fill = annee), 
#                                 stat = "binline", 
#                                 alpha = .5)


# calculate overall mort/vivant ratio

dfmean.all <- df %>%
  group_by(secteur, zone, annee, stat) %>%
  summarise(mean_ntailm2 = mean(ntailm2, na.rm = TRUE)) %>% 
  ungroup()

dfratio.all <- dfmean.all %>%
  group_by(secteur, zone, annee) %>%
  pivot_wider(names_from = stat, values_from = mean_ntailm2) %>%
  mutate(ratio = MOR/VIV)

# plot by sector
ggplot(dfratio.all) +
  geom_line(aes(x = annee, y = ratio, group = zone)) +
  facet_wrap(~secteur)

#### Create Data object with mortality information #####    

# make catch matrix
catch_matrix <- subset(df, select = c(annee, ntailm2)) %>% 
  pivot_wider(names_from = annee,
              values_from = ntailm2,
              values_fn = function(x) {sum(x, na.rm = TRUE)}) %>%
  as.matrix()

# make average catch matrix
avc_matrix <- group_by(df, annee) %>% 
  summarise(Avc = mean(ntailm2, na.rm = TRUE))


# make relative abundance matrix
ra_matrix <- group_by(df, annee, taille) %>%
  summarise(Ind = sum(percent, na.rm = TRUE)) %>%
  as.matrix()

# create data object
scallops_mort <- new('Data')
# populate the data file
scallops_mort@Name <- "Data"
scallops_mort@Common_Name <- "Scallop"
scallops_mort@LHYear <- max(df$annee)
scallops_mort@Units <- "m2"
scallops_mort@nareas <- length(unique(df$secteur))  
scallops_mort@Year <- unique(df$annee)
scallops_mort@LFC <- df$taille
scallops_mort@Cat <- catch_matrix
scallops_mort@AvC <- avc_matrix
scallops_mort@Ind <- ra_matrix

# suite du copier-coller 
summary(scallops_mort, wait=FALSE, rmd=TRUE)

# which MPs can be applied?
Can(scallops_mort)
# which can't?
Cant(scallops_mort)

## Apply MPs -------------------------------------------------------------------

TACs <- TAC(scallops_mort)  
TACs@MPs
boxplot(TACs)
plot(TACs)

# Apply input control MPs ------------------------------------------------------

Input(scallops_mort)

# Sensitivity analyses on TAC recommendations ----------------------------------

scallops <- Sense(scallops_mort, "AvC")
scallops <- Sense(scallops_mort, "CC1")

# run MP

runMP(scallops_mort, c("AvC"))


# create and populate stock object
Scallop <- new('Stock')
# populate the data file
Scallop@Name <- "Stock"
Scallop@Common_Name <- "Scallop"
Scallop@maxage <- 20 # https://www.fisheries.noaa.gov/species/atlantic-sea-scallop#:~:text=Biology,grow%20larger%20than%206%20inches.
Scallop@R0 <- 1000 # suggested default value for DL fisheries in the help file
Scallop@M <- c(0.3, 0.5) # arbitrary range here, just a guess to start with
Scallop@Msd <- c(0.05, 0.1) # high-ish variance here
Scallop@h <- c(1/5, 1) # maximum allowed range here, not very informative
Scallop@SRrel <- 1 # check what this relationship should be
# some unused parameters between these two
Scallop@Linf <- c(0.15, 0.20) # https://www.dfo-mpo.gc.ca/fisheries-peches/ifmp-gmp/scallop-petoncle/scallop-petoncle2015-sec2-3-eng.html
Scallop@Linfsd <- c(0.5, 0.9) # arbitrary range here
# added to plot fleet
Scallop@D <- 0.5 # arbitrary choice
Scallop@Perr <- c(0.1, 0.5) # arbitrary choice
Scallop@AC <-   c(0.1, 0.5) # arbitrary choice
Scallop@K <- c(1, 2)  # arbitrary choice
# SEE https://academic.oup.com/icesjms/article/66/10/2165/679802 to estimate it
Scallop@Ksd <- c(0.5, 1) # arbitrary choice
Scallop@t0 <- c(0, 0.1)#   arbitrary choice
Scallop@LenCV <- c(0.1, 0.5) # arbitrary choice
Scallop@a <- 1
Scallop@b <- 1
Scallop@Size_area_1 <- 1
Scallop@Frac_area_1 <- 1
Scallop@Prob_staying <- 0.8
Scallop@Fdisc <- dfratio.all$ratio
Scallop@Linf <- 110 # gueestimate based on max length


Turing(DLMtool::testOM, DLMtool::SimulatedData, wait=FALSE)

OM1 <- new("OM", Scallop, Generic_Fleet, Generic_Obs, Perfect_Imp, nsim=150)
plot(OM1) # fails
MSE1 <- runMSE(OM1) # fails
  

# Specify historical effort trends (Fleet object) ------

MyFleet <- Generic_Fleet
#MyFleet <- ChooseEffort(MyFleet) # might have to redo this... used figure of mean effort per year to approximate

#MyFleet@EffLower
#MyFleet@EffUpper

plot(MyFleet, Scallop, 50)
