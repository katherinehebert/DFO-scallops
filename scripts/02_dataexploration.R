# Script to explore the datasets

rm(list=ls())

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

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
       y = "Effort (hm)") +
  theme(
    axis.text.x = element_text(angle = 90, size = 8))

# weight
ggplot(data = cpue_gr, 
       aes(x = as.factor(Year))) +
  geom_boxplot(aes(y = Weight_full)) +
  labs(x = "Year",
       y = "Full Weight (kg)") +
  coord_trans(y = "log10") +
  theme(
    axis.text.x = element_text(angle = 90, size = 8))
ggplot(data = cpue_gr, 
       aes(x = as.factor(Year))) +
  geom_boxplot(aes(y = Weight_muscle)) +
  labs(x = "Year",
       y = "Muscle Weight (kg)") +
  coord_trans(y = "log10") +
  theme(
    axis.text.x = element_text(angle = 90, size = 8))


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

## exploratory plots

# prepare the data
sizes.toplot <- sizes
sizes.toplot$Year <- as.character(sizes$Year)
sizes.toplot$Length <- factor(sizes$Length, levels = c(1:120, NA))

# print a continuous legend separately
n1 <- 121                                        # Amount of default colors
hex_codes1 <- hue_pal()(n1)                             # Identify hex codes
names(hex_codes1) <- levels(sizes.toplot$Length)
p0 <- ggplot(sizes) +
  geom_point(aes(x = Year, 
                 y = nlengthm2, 
                 col = Length)) +
  facet_wrap(~Sector) +
  scale_color_gradientn(colors = hex_codes1) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, size = 6))
plotlegend <- cowplot::get_legend(p0)
p0 <- ggpubr::as_ggplot(plotlegend)

# stacked barchart of size categories through time
p <- ggplot(sizes.toplot) +
  geom_bar(aes(x = Year, 
               y = nlengthm2, 
               fill = Length),
           position = "stack", 
           stat = "identity",
           width = .6) +
  labs(x = "", y = "Density at each length") +
  facet_wrap(~Sector) +
  scale_fill_manual(values = hex_codes1) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, size = 6))

# patchwork the plot and its legend together
p0 / p + 
  plot_layout(heights = unit(c(1.5, 1), c('cm', 'null')))
ggsave("figures/density_lengths_persector.png", width = 8.42, height = 8.6)


# same, but percent barchart
p2 <- ggplot(sizes.toplot) +
  geom_bar(aes(x = Year, 
               y = nlengthm2, 
               fill = Length),
           position = "fill", 
           stat = "identity",
           width = .6) +
  labs(x = "", y = "Density at each length") +
  facet_wrap(~Sector) +
  scale_fill_manual(values = hex_codes1) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, size = 6))
p0 / p2 + 
  plot_layout(heights = unit(c(1.5, 1), c('cm', 'null')))
ggsave("figures/density_lengths_persector_fill.png", width = 8.42, height = 8.6)

