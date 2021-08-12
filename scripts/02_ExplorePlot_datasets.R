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

# MORTALITY RATIO --------------------------------------------------------------

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

# visualise mortality ratio as barplot
ggplot(dfmean) +
  geom_bar(aes(x = as.character(annee), y = mean_ntailm2, fill = stat), 
           stat = "identity", position = "dodge", width = .6) +
  facet_wrap(~secteur, ncol = 3) +
  labs(x = "", y = "Mean count per length", fill = "Status")
ggsave("figures/mortality_barplot.png", width = 11, height = 10)

# calculate mort/vivant ratio per size
dfratio.size <- dfmean %>%
  group_by(secteur, zone, stat, annee, taille) %>%
  pivot_wider(names_from = stat, values_from = mean_ntailm2) %>%
  mutate(ratio = MOR/VIV)
dfratio.size$ratio[is.nan(dfratio.size$ratio)] <- 0
# save to outputs
saveRDS(dfratio.size, "data/MORVIVratio.rds")

# calculate per sector mort/vivant ratio
dfmean.sector <- df %>%
  group_by(secteur, zone, annee, stat) %>%
  summarise(sum_ntailm2 = sum(ntailm2, na.rm = TRUE)) %>% 
  ungroup()

dfratio.sector <- dfmean.sector %>%
  group_by(secteur, zone, annee) %>%
  pivot_wider(names_from = stat, values_from = sum_ntailm2) %>%
  mutate(ratio = MOR/VIV)
dfratio.sector$ratio[which(is.nan(dfratio.sector$ratio))] <- 0

# plot by sector
ggplot(dfratio.sector) +
  geom_point(aes(x = annee, y = ratio)) +
  geom_smooth(aes(x = annee, y = ratio, col = secteur, fill = secteur), method = "lm") +
  facet_wrap(~secteur) +
  theme(legend.position = "none") +
  labs(y = "Ratio MOR/VIV", x = "")
ggsave("figures/mortality_persector.png", width = 7.8, height = 6.2)

# calculate OVERALL mort/vivant ratio

dfmean.overall <- df %>%
  group_by(annee, stat) %>%
  summarise(sum_ntailm2 = sum(ntailm2, na.rm = TRUE)) %>% 
  ungroup()

dfratio.all <- dfmean.overall %>%
  group_by(annee) %>%
  pivot_wider(names_from = stat, values_from = sum_ntailm2) %>%
  mutate(ratio = MOR/VIV)
dfratio.all$ratio[which(is.nan(dfratio.all$ratio))] <- 0

# plot by sector
ggplot(dfratio.all) +
  geom_point(aes(x = annee, y = ratio)) +
  geom_smooth(aes(x = annee, y = ratio), method = "lm") +
  theme(legend.position = "none") +
  labs(y = "Ratio MOR/VIV", x = "") +
  coord_cartesian(ylim = c(0,0.10))
ggsave("figures/mortality_overall.png", width = 4.96, height = 2.54)
