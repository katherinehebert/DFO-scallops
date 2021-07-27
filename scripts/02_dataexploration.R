# Script to explore the datasets

rm(list=ls())

library(ggplot2)
library(readr)

theme_set(ggpubr::theme_pubclean())

# CPUE -------------------------------------------------------------------------

# read the prepared data
cpue <- read_csv("data/scallop-cpue.csv")

# group by fishing area and zone
cpue_gr <- cpue %>%
  group_by(Div, Quadril, Zone_ges) 

## exploratory plots

# effort
ggplot(data = cpue_gr, 
       aes(x = as.factor(Year))) +
  geom_boxplot(aes(y = Effort_hm)) +
  labs(x = "Year",
       y = "Effort (hm)")

# weight
ggplot(data = cpue_gr, 
       aes(x = as.factor(Year))) +
  geom_boxplot(aes(y = Weight_full)) +
  labs(x = "Year",
       y = "Full Weight (kg)") +
  coord_trans(y = "log10")
ggplot(data = cpue_gr, 
       aes(x = as.factor(Year))) +
  geom_boxplot(aes(y = Weight_muscle)) +
  labs(x = "Year",
       y = "Muscle Weight (kg)") +
  coord_trans(y = "log10")


# annual summary statistics

cpue_summ <- cpue %>% 
  group_by(Year) %>%
  summarise(mean_effort = mean(Effort_hm, na.rm = TRUE),
            mean_cpue_hm = mean(CPUE_hm, na.rm = TRUE),
            mean_cpue_tm = mean(CPUE_tm, na.rm = TRUE),
            mean_weight_muscle = mean(Weight_muscle, na.rm = TRUE),
            sd_effort = sd(Effort_hm, na.rm = TRUE),
            sd_cpue_hm = sd(CPUE_hm, na.rm = TRUE),
            sd_cpue_tm = sd(CPUE_tm, na.rm = TRUE),
            sd_weight_muscle = sd(Weight_muscle, na.rm = TRUE))

# effort
ggplot(data = cpue_summ, 
       aes(x = Year)) +
  geom_ribbon(aes(ymin = mean_effort - 2*sd_effort,
                  ymax = mean_effort + 2*sd_effort),
              alpha = .3, fill  = "darkgreen") +
  geom_line(aes(y = mean_effort)) +
  labs(x = "Year",
       y = "Mean Effort (hm)")


# cpue

ggplot(data = cpue_summ, 
       aes(x = Year)) +
  geom_ribbon(aes(ymin = mean_cpue_hm - 2*sd_cpue_hm,
                  ymax = mean_cpue_hm + 2*sd_cpue_hm),
              alpha = .3, fill  = "navyblue") +
  geom_line(aes(y = mean_cpue_hm)) +
  labs(x = "Year",
       y = "Mean CPUE (hm)")

# weight

ggplot(data = cpue_summ, 
       aes(x = Year)) +
  geom_ribbon(aes(ymin = mean_weight_muscle - 2*sd_weight_muscle,
                  ymax = mean_weight_muscle + 2*sd_weight_muscle),
              alpha = .3, fill  = "darkred") +
  geom_line(aes(y = mean_weight_muscle)) +
  labs(x = "Year",
       y = "Mean Muscle Weight (kg)")

# Length -------------------------------------------------------------------------

# read the prepared data
sizes <- read_csv("data/scallop-sizes.csv")

# group by fishing area and zone
cpue_gr <- cpue %>%
  group_by(Div, Quadril, Zone_ges) 

## exploratory plots