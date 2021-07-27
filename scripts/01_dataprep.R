# Script to prepare datasets

library(readxl)
library(dplyr)
library(tidyr)

# read datasets
size <- read_excel("data_raw/Z16E_Survey_Length_1990-2019_Alive.xlsx")
cpue <- read_excel("data_raw/ZIFF_Data_1987-2019.xlsx")

# check them out
glimpse(size)
glimpse(cpue)

# save as .csv
write.csv(size, "data/scallop-sizes.csv", row.names = FALSE)
write.csv(cpue, "data/scallop-cpue.csv", row.names = FALSE)
