# Model mortality through time

library(Hmisc)

# read the prepared data
df <- readRDS("data/MORVIVratio.rds")

### group by sector-zone-year to match sizes and mortality #####################

# subset to data from 2004-2019
df <- df %>% filter(annee %in% 2004:2019)
df$Total <- df$MOR + df$VIV
hist(df$ratio)

# group and summarize mean density of scallops by length

dfmean <- df %>%
  group_by(secteur, zone, annee, taille) %>%
  summarise(mean_ratio = mean(ratio, na.rm = TRUE)) %>% 
  ungroup()
hist(dfmean$mean_ratio)

# visualise to check
# as barplot
ggplot(df) +
  geom_bar(aes(x = as.character(annee), 
               y = ratio), 
           stat = "identity", 
           width = .6) +
  facet_wrap(~secteur) +
  labs(x = "Year", y = "Mean ratio of mortality")


# Plot ratio over time

ggplot(dfmean) +
  geom_line(aes(y = mean_ratio, x = annee, group = taille)) +
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
