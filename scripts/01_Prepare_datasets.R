# Script to prepare datasets

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# read datasets
size <- read_excel("data_raw/Z16E_Survey_Length_1990-2019_Alive.xlsx")
cpue <- read_excel("data_raw/ZIFF_Data_1987-2019.xlsx")
mort <- read_csv("data_raw/z16E_t_1990_2019.csv", col_names = TRUE)[,-1]

# check them out
glimpse(size)
glimpse(cpue)
glimpse(mort)

# save as .csv
write.csv(size, "data/scallop-sizes.csv", row.names = FALSE)
write.csv(cpue, "data/scallop-cpue.csv", row.names = FALSE)
write.csv(mort, "data/scallop-mortality.csv", row.names = FALSE)