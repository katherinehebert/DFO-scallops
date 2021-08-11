# Model mortality through time

rm(list=ls())

# load packages
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(Hmisc)

theme_set(ggpubr::theme_pubclean())

# read the prepared data
df <- readRDS("data/MORVIVratio.rds")

### group by sector-zone-year to match sizes and mortality #####################

# subset to data from 2004-2019
df <- df %>% filter(annee %in% 2004:2019)
df$Total <- df$MOR + df$VIV
df$ratio[which(is.nan(df$ratio))] <- 0
hist(df$ratio)

# group and summarize mean density of scallops by length

dfmean <- df %>%
  group_by(secteur, annee) %>%
  summarise(mean_ratio = mean(ratio, na.rm = TRUE)) %>% 
  ungroup()
hist(dfmean$mean_ratio)

# visualise to check
# as barplot
ggplot(dfmean) +
  geom_bar(aes(x = as.character(annee), 
               y = mean_ratio), 
           stat = "identity", position = "dodge",
           width = .6) +
  facet_wrap(~secteur) +
  labs(x = "Year", y = "Mean ratio of mortality")


# Plot ratio over time

ggplot(dfmean) +
  geom_line(aes(y = mean_ratio, x = annee)) +
  facet_wrap(~secteur)


# calculate mean of ratio over time, per sector
# lengths are weighted by the Count of individuals at each length
dfmean <- df %>%
  group_by(secteur, annee) %>%
  # weighted average length via density!
  summarise(mean_length = wtd.mean(x = Length, weights = Count, 
                                   na.rm = TRUE, normwt = FALSE),
            # weighted variance
            var_length = wtd.var(x = Length, weights = Count, 
                                 na.rm = TRUE, normwt = FALSE)) %>%
  mutate(sd_length = sqrt(var_length))
