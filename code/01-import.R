## 0. preliminaries ###########################################################

library(readxl)
library(dplyr)
library(tidyr)
library(LaF)

## 1. import and clean data  ##################################################

# urbanization rates
df <- read_excel("data/raw/urbanization.rates.xls",
                 skip = 16)

df %>% 
  rename(country = `Region, subregion, country or area`) %>% 
  filter(country %in% c("Viet Nam", "Myanmar", "South-Eastern Asia")) %>% 
  gather(key = period, value = urb.rate, 5:24) %>% 
  select(2, 5:6) %>% 
  separate(period, into = c("start", "end"), remove = FALSE) %>% 
  mutate(start = as.numeric(start), end = as.numeric(end))-> urb.rates

saveRDS(urb.rates, "data/processed/urb.rates.rds")


# ruralization rates

df <- read_excel("data/raw/ruralization.rates.xls",
                  skip = 16)
df %>% 
  rename(country = `Region, subregion, country or area`) %>% 
  filter(country %in% c("Viet Nam", "Myanmar", "South-Eastern Asia")) %>% 
  gather(key = period, value = rur.rate, 5:24) %>% 
  select(2, 5:6) %>% 
  separate(period, into = c("start", "end"), remove = FALSE) %>% 
  mutate(start = as.numeric(start), end = as.numeric(end))-> rur.rates

saveRDS(rur.rates, "data/processed/rur.rates.rds")

# rural growth rates

df <- read_excel("data/raw/rural.growth.xls",
                 skip = 16)
df %>% 
  rename(country = `Region, subregion, country or area`) %>% 
  filter(country %in% c("Viet Nam", "Myanmar", "South-Eastern Asia")) %>% 
  gather(key = period, value = rur.growth, 5:24) %>% 
  select(2, 5:6) %>% 
  separate(period, into = c("start", "end"), remove = FALSE) %>% 
  mutate(start = as.numeric(start), end = as.numeric(end))-> rur.growth

saveRDS(rur.growth, "data/processed/rur.growth.rds")


# rural growth rates

df <- read_excel("data/raw/proportion.urban.xls",
                 skip = 16)
df %>% 
  rename(country = `Region, subregion, country or area`) %>% 
  filter(country %in% c("Viet Nam", "Myanmar", "South-Eastern Asia")) %>% 
  gather(key = year, value = prop.urb, 5:105) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(2, 5:6) -> prop.urb

saveRDS(prop.urb, "data/processed/prop.urb.rds")


## import and clean census data 

ipums.file <- "../../data/IPUMS/ipumsi_00003.dat"
wdts <- c(3, 4, 4, 10, 3, 8,
          2, 1, 2, 6, 1, 6,
          1, 1, 2, 3, 8,
          3, 3, 3, 2, 2, 1,
          1, 3, 3, 2, 1, 1, 1, 2,
          4, 2, 2, 2, 2, 4,
          1, 2, 2, 2, 1, 1,
          1, 1, 2, 2, 3, 1,
          1, 2,  2, 4, 3, 3, 5,
          2, 1, 1, 2, 2, 3,
          1,  1, 1, 1, 1, 1)
column.names <- c("cntry", "year", "sample", "serial", "persons", "wthh",
                  "subsamp","urban", "regionw", "geolev1", "regnv","wtf",
                  "mortnum", "anymort", "hhtype", "pernum", "wtper",
                  "momloc", "poploc", "sploc", "parrule", "srule", "relate",
                  "related", "wtf2", "age", "age2", "sex", "marst", "marstd", "wtf3",
                  "brthyr", "brthmo", "chborn", "chsurv", "lstbmth", "lstbyr",
                  "lstbsex", "chdead", "homechd", "awaychd", "school", "lit",
                  "edattan", "edatand",  "wtf2","yrschl", "educvn", "empstat",
                  "empstatd", "wtf4", "occisco", "occ", "isco88a", "indgen", "ind",
                  "wtf5", "classwk", "classwkd", "empsect", "migrates", "migvn",
                  "disable", "disemp", "disblind", "disdeaf", "dislowr","dismntl")
column.classes <- c("categorical", "categorical", "categorical", "integer", "integer", "double",
                    "integer", "categorical", "categorical","categorical", "categorical", "integer",
                    "integer", "categorical", "categorical", "integer", "double",
                    "categorical", "categorical", "categorical", "categorical","categorical","categorical",
                    "categorical", "integer", "integer", "categorical", "categorical", "categorical", "categorical", "integer",
                    "integer", "categorical", "integer", "integer", "categorical", "integer",
                    "categorical", "integer", "integer", "integer", "categorical", "categorical",
                    "categorical", "categorical", "integer", "integer", "categorical", "categorical",
                    "categorical", "integer", "categorical", "categorical", "categorical", "categorical", "categorical",
                    "integer", "categorical", "categorical", "categorical", "categorical", "categorical",
                    "categorical", "categorical", "categorical", "categorical", "categorical", "categorical")
## test it works
read.fwf(ipums.file,width=wdts, col.names=column.names,skip=30, n = 10)
## open link to file
d <- laf_open_fwf(ipums.file, column_widths=wdts,
                  column_types=column.classes, column_names = column.names)

# 1 = rural. 2 = urban
d[,c("year", "wtper", "urban", "age", "empstat")] %>%    
  mutate(youth = ifelse(age <15, 0, ifelse( age  < 65, 1, 2))) %>% 
  group_by(year, urban, youth) %>% 
  dplyr::summarize(n=sum(wtper)/100) -> x

saveRDS(x, "data/processed/dep.ratios.rds")

# marital status

d[,c("year", "wtper", "urban", "sex", "age", "marstd")] %>%    
  filter(age >= 16 ) %>% 
  group_by(year, urban, sex, marstd) %>% 
  dplyr::summarize(n=sum(wtper)/100) -> x


saveRDS(x, "data/processed/mar.status.rds")

