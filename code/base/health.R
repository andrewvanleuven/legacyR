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
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))

### SOURCE: https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
df <- read_delim("data/base/source/infant.txt", delim = "\t")
