library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

xw <- read_csv("data/xw.csv") 
univ <- read_csv("data/base/univ.csv") 

### SOURCE: https://www.countyhealthrankings.org/sites/default/files/
df <- read_csv("data/base/source/health.csv") %>% 
  rename_all(tolower) %>% select(-pct_smokers) %>% 
  inner_join(.,xw) %>% 
  inner_join(.,univ) %>% 
  filter(!is.na(pct_low_birthweight),
         !is.na(pct_obese),
         !is.na(pct_uninsured_adults)) %>% 
  group_by(cbsa_fips) %>% 
  summarise(pct_low_inf_bw = mean(pct_low_birthweight),
            pct_obese_adult = mean(pct_obese),
            pct_uninsured_adult = mean(pct_uninsured_adults)) %>% 
  inner_join(.,univ) %>% select(1,5,2:4) %>% 
  write_csv("data/base/generated/health.csv")

