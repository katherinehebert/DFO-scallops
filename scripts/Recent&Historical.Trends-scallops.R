### Historical and recent rate of decline ###

# Use a linear regression line to obtain recent (last 10 years) and historical (all years) trends observed #
# in commercial fishery data (landings, CPUE) and abundance surveys (CPUE, recruitment, biomass)           #

## Source : FAO. 2001. Second technical consultation on the suitability of the CITES criteria for listing commercially exploited aquatic species. FAO background document for the 2nd technical consultation on the suitability of CITES criteria for listing commercially exploited aquatic species. FAO Doc. FI:SLC2/2001/2.  http://www.fao.org/3/Y1455E/Y1455E.htm ##

##################################################################################################################
### Example With NAFO 4RST Greenland halibut landings and CPUE from the DFO summer bottom trawl survey in nGSL ###
##################################################################################################################

# This script was adapted from https://github.com/MathBoud/DLM.ReferencePoint/blob/master/R/Recent%26Historical.Trends.R

rm(list=ls())

# Activate required packages
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(dplyr)

# set all ggplot themes
theme_set(ggpubr::theme_pubclean())

# function to plot linear regression with model annotation
ggplotRegression <- function (fit, ylabel) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    xlab("Year") + 
    ylab(ylabel) +
    stat_smooth(method = "lm", col = "red") +
    labs(caption = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

# Load catch and spawning biomass index time series 
# Import CPUE
CPUE <- read.csv("data/scallop-cpue.csv", header = TRUE)

# Create variable to have biomass and landings values in tons
CPUE$Land.tons <- CPUE$Weight_full*0.00110231



###############################################################################
# Visualizing the whole time series

# sum landings for each year
CPUE_landings <- 
  CPUE %>% group_by(Year) %>%
  summarise(Land.tons = sum(Land.tons, na.rm = TRUE),
            Year = Year)
# save
saveRDS(CPUE_landings, "data/cpue-landings-yearly.rds")

# plot the landings over time
(
  plot.Landings.Index <- 
    ggplot(CPUE_landings) +
  geom_bar(aes(y = Land.tons, x=Year), 
           position = "stack", 
           stat = "identity",
           color = "darkgreen") +
  ylab("Landings (t)") +
  xlab("Year") 
  )

# Take mean per year, following column names in MathBoud's repo https://github.com/MathBoud/DLM.ReferencePoint/blob/master/data/PUE.turbot.csv
PUE <- CPUE %>%
  group_by(Year) %>%
  summarise(
    Annee = Year,
    moy = mean(CPUE_hm, na.rm = TRUE),
    var = var(CPUE_hm, na.rm = TRUE),
    # 95% confidence intervals around the mean
    ICmin = mean(CPUE_hm, na.rm = TRUE) - 1.96*sd(CPUE_hm, na.rm = TRUE),
    ICmax = mean(CPUE_hm, na.rm = TRUE) + 1.96*sd(CPUE_hm, na.rm = TRUE)
  )
# plot CPU mean over time with 95% confidence intervals
(
  plot.CPUE <- 
    ggplot(PUE, aes(x = Annee, y = moy)) +
    geom_point(color="blue", size=3) +
    geom_line(color="blue", size=1.25) +
    ylab("Survey CPUE (hm)") +
    xlab("Year") +
    geom_errorbar(aes(ymin = ICmin, ymax = ICmax), 
                  width = .5, position = position_dodge(.9), 
                  color = "blue") 
)

# arrange plots together and save
ggarrange(plot.Landings.Index, plot.CPUE, ncol=1, nrow=2)
ggsave("figures/CPUE_Landings_alltime.png", width = 6, height = 5)


### LAST 5 YEARS ###############################################################

# Recent trend in CPUE from abundance survey in last 5 years
RecYearPUE <- subset(PUE, Annee >= max(PUE$Annee)-4) # Get N recent number of year, often 5 to 10 
# plot them
plot.Recent.PUE <- ggplot(RecYearPUE, aes(x=Annee, y=moy)) +
  geom_point(color="blue", size=3) +
  geom_line(color="blue", size=1.25) +
  ylab("Mean Survey CPUE (kg/tow)") +
  xlab("Year") +
  xlim(c(min(RecYearPUE$Annee), max(RecYearPUE$Annee))) +
  geom_errorbar(aes(ymin=ICmin, ymax=ICmax), width=.5, position=position_dodge(.9), color = "blue")

# Get simple linear regression result for trend in time series
fit1 <- lm(moy ~ Annee, RecYearPUE)
Regression.PUE <- ggplotRegression(fit1, "Mean Survey CPUE (kg/tow)") 

# Recent trend in commercial landings in last 5 years
RecYearLandings <- subset(CPUE_landings, Year >= max(CPUE$Year)-4) #Get N recent number of year, often 5 to 10 
# plot them
plot.Recent.Landings <- ggplot(RecYearLandings, aes(x=Year, y=Land.tons)) +
  geom_point(color="blue", size=3) +
  geom_line(color="blue", size=1.25) +
  ylab("Landings (t)") +
  xlab("Year") +
  xlim(c(min(RecYearLandings$Year), max(RecYearLandings$Year))) 

# Get simple linear regression result for trend in time series
fit1 <- lm(Land.tons~Year, RecYearLandings)
Regression.Landings <- ggplotRegression(fit1, "Landings (t)")

# arrange plots together and save
ggarrange(plot.Recent.PUE, plot.Recent.Landings, Regression.PUE, Regression.Landings, ncol=2, nrow=2)
ggsave("figures/CPUE_Landings_05Recent.png", width = 9, height = 5)

### LAST 10 YEARS ##############################################################


# Recent trend in commercial landings in last 10 years
RecYearLandings <- subset(CPUE_landings, Year >= max(CPUE_landings$Year)-9) #Get N recent number of year, often 5 to 10 

plot.Recent.Landings <- 
  ggplot(RecYearLandings, 
         aes(x = Year, y = Land.tons)) +
  geom_point(color = "blue", size=3) +
  geom_line(color = "blue", size=1.25) +
  ylab("Landings (t)") +
  xlab("Year") +
  xlim(c(min(RecYearLandings$Year), max(RecYearLandings$Year)))

# Get simple linear regression result for trend in time series
fit1 <- lm(moy ~ Annee, RecYearPUE)
Regression.PUE <- ggplotRegression(fit1, "Mean Survey CPUE (kg/tow)") 


# Recent trend in CPUE from abundance survey in last 5 years
RecYearPUE <- subset(PUE, Annee >= max(PUE$Annee)-9) # Get N recent number of year, often 5 to 10 
# plot them
plot.Recent.PUE <- ggplot(RecYearPUE, aes(x=Annee, y=moy)) +
  geom_point(color="blue", size=3) +
  geom_line(color="blue", size=1.25) +
  ylab("Mean Survey CPUE (kg/tow)") +
  xlab("Year") +
  xlim(c(min(RecYearPUE$Annee), max(RecYearPUE$Annee))) +
  geom_errorbar(aes(ymin=ICmin, ymax=ICmax), width=.5, position=position_dodge(.9), color = "blue")


#Get simple linear regression result for trend in time series
fit1 <- lm(Land.tons ~ Year, RecYearLandings)
Regression.Landings <- ggplotRegression(fit1, ylabel = "Landings (t)")

# arrange plots and rave
ggarrange(plot.Recent.PUE, plot.Recent.Landings, Regression.PUE, Regression.Landings, ncol=2, nrow=2)
ggsave("figures/CPUE_Landings_10Recent.png", width = 13.4, height = 7)


## HISTORICAL ##################################################################
# this is where i'm at

# Historical trend in CPUE from abundance survey time series
plot.Hist.PUE <- 
  ggplot(PUE, 
         aes(x=Annee, y=moy)) +
  geom_point(color="blue", size=3) +
  geom_line(color="blue", size=1.25) +
  ylab("Mean Survey CPUE (kg/tow)") +
  xlab("Year") +
  xlim(c(min(PUE$Annee), max(PUE$Annee))) +
  geom_errorbar(aes(ymin=ICmin, ymax=ICmax), 
                width=.5, position=position_dodge(.9), color = "blue")

#Get simple linear regression result for trend in time series
fit1 <- lm(moy ~ Annee, PUE)
Regression.PUE <- ggplotRegression(fit1, "Mean Survey CPUE (kg/tow)")

# Historical end in commercial landings in last 10 years
plot.Historical.Landings <- 
  ggplot(CPUE_landings,
         aes(x=Year, y=Land.tons)) +
  geom_point(color="blue", size=3) +
  geom_line(color="blue", size=1.25) +
  ylab("Landings (t)") +
  xlab("Year") +
  xlim(c(min(CPUE$Year), max(CPUE$Year)))

#Get simple linear regression result for trend in time series
fit1 <- lm(Land.tons~Year, CPUE_landings)
Regression.Landings <- ggplotRegression(fit1, "Landings (t)")

# arrange plots and save
ggarrange(plot.Hist.PUE, plot.Historical.Landings, Regression.PUE, Regression.Landings, ncol=2, nrow=2)
ggsave("figures/CPUE_Landings_Historical.png", width = 13.4, height = 7)
