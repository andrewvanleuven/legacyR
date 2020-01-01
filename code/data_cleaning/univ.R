library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)
univ <- read_csv("data/base/univ.csv") 
xw <- read_csv("data/xw.csv") 
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))

### SOURCE: https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx
zipxw <- read_csv("data/base/source/zip_xw.csv") %>% rename_all(tolower) %>% 
  mutate(zip = as.numeric(zip)) %>% 
  filter(tot_ratio > 0.5) %>% 
  rename(cbsa_fips = cbsa) %>% 
  select(1:2)

universities <- read_csv("data/base/source/univ.csv") %>% 
  mutate(zip = as.numeric(zip),
         r1 = if_else(ccbasic == 15,1,0),
         r2 = if_else(ccbasic == 16,1,0)) %>% 
  select(-enrtot) %>% 
  filter(r1 == 1 | r2 == 1,
         !str_detect(st, 'PR')) %>% 
  left_join(.,zipxw) %>% 
  filter(!is.na(cbsa_fips)) %>% 
  group_by(cbsa_fips) %>% 
  summarise(r_1 = sum(r1),
            r_2 = sum(r2)) %>% 
  left_join(univ,.) %>% 
  replace_na(list(r_1 = 0,r_2 = 0)) %>% 
  write_csv("data/base/generated/univ.csv")
  
  

  
