library(readxl)
library(dplyr)
library(tidyr)


df <- read_excel("data/raw/urbanization.rates.xls",
                 skip = 16)

df %>% 
  filter(`Region, subregion, country or area` %in% c("Viet Nam", "Myanmar")) 
