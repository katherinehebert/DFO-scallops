# Model length differences through time

## continue this, my brain is dying


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
sizes <- read_csv("data/scallop-sizes.csv")

# calculate weighted mean of lengths over time, per sector
# lengths are weighted by the Count of individuals at each length
meanlengths <- sizes %>%
  group_by(Sector, Year) %>%
  # weighted average length via density!
  summarise(mean_length = wtd.mean(x = Length, weights = Count, 
                                   na.rm = TRUE, normwt = FALSE),
            # weighted variance
            var_length = wtd.var(x = Length, weights = Count, 
                                 na.rm = TRUE, normwt = FALSE)) %>%
  mutate(sd_length = sqrt(var_length))

# plot time series of mean lengths per sector  
ggplot(meanlengths) +
  geom_ribbon(aes(x = Year,
                  ymin = mean_length - 2*sd_length,
                  ymax = mean_length + 2*sd_length,
                  fill = Sector),
              alpha = .1) +
  geom_line(aes(x = Year, y = mean_length, col = Sector)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Mean length") +
  facet_wrap(~Sector)
ggsave("figures/meanlengths_persector.png", width = 6.28, height = 8.74)
  

# does this require a linear mixed model? --------------------------------------

lm.test <- lm(mean_length ~ Year, data = meanlengths)
# get residuals
lm.test.resid <- resid(lm.test)

# remove NAs
temp <- meanlengths[-which(is.na(meanlengths$mean_length)),]

plot(lm.test.resid ~ as.factor(temp$Sector), 
     xlab = "Sector",
     ylab = "Standardized residuals")

abline(0, 0, lty = 2)
# there is residual variance that varies by sector, so good idea to try a mixed model

# mixed model(s) compared to basic linear model
library(lme4)
m0 <- lm(mean_length ~ Year, 
         data = meanlengths)
m1 <- lmer(mean_length ~ Year + 1|Sector, 
           data = meanlengths)
m2 <- lmer(mean_length ~ Year|Sector, 
            data = meanlengths)
hist(resid(m0)) # gaussian should be fine here, the residuals are pretty Normal
hist(resid(m1))
hist(resid(m2))

# compare
AIC.table <- MuMIn::model.sel(m0, m1, m2)
saveRDS(AIC.table, "outputs/model_length_AIC.rds")
# basic linear model still comes through as the best option
tidym0 <- broom::tidy(m0)
saveRDS(tidym0, "outputs/model_length_m0_tidy.rds")
# check out model summary
summary(m0)
saveRDS(m0, "outputs/model_length_m0.rds")


# plot the model over the time series
ggplot(meanlengths) +
  geom_line(aes(x = Year, y = mean_length, col = Sector)) +
  geom_smooth(aes(x = Year, y = mean_length), 
              method = "lm", col = "black") +
  theme(legend.position = "right") +
  labs(x = "", y = "Mean length",
       caption = paste("Adj R2 = ",signif(summary(m0)$adj.r.squared, 2),
                     " Slope =",signif(m0$coef[[2]], 5),
                     " P =",signif(summary(m0)$coef[2,4], 5)))
ggsave("figures/meanlengths_lm.png", width = 6.83, height = 5.03)


# plot the model over boxplots of the mean lengths over all sectors
# to see the variation better
ggplot(meanlengths) +
  geom_boxplot(aes(x = Year, y = mean_length, 
                   group = factor(Year), fill = mean_length)) +
  geom_smooth(aes(x = Year, y = mean_length), 
              method = "lm", col = "black") +
  theme(legend.position = "right") +
  labs(x = "", y = "Mean length",
       caption = paste("Adj R2 = ",signif(summary(m0)$adj.r.squared, 2),
                       " Slope =",signif(m0$coef[[2]], 5),
                       " P =",signif(summary(m0)$coef[2,4], 5)))
ggsave("figures/meanlengths_lm_boxplot.png", width = 6.83, height = 5.03)

broom.mixed::tidy(m2)
summary(m2)


# Prepare model outputs to plot the slopes per sector, from the mixed models, 
# to get an idea of how the sectors differ
coefs <- coef(m2)$Sector %>% as.data.frame()
coefs$sector <- rownames(coefs)
coefs <- coefs[order(coefs$Year, decreasing = FALSE),]
coefs$sector <- factor(coefs$sector, levels = coefs$sector)
# plot
ggplot(coefs) +
  geom_segment(aes(x = Year, xend = 0, y = sector, yend = sector, col = Year),
               lwd = .8)+
  geom_point(aes(x = Year, y = sector, col = Year), size = 5) +
  coord_cartesian(xlim = c(-0.006, 0.006)) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  scale_color_distiller(palette = "Spectral", 
                        direction = 1, 
                        limits = c(-0.0046,0.0046)) +
  theme(legend.position = "right") +
  labs(col = "Slope", y = "Sector", x = "Slope (Mean Length ~ Year)")
ggsave("figures/meanlengths_lmm_slopespersector.png", width = 6, height = 3.5)
