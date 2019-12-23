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

### SOURCE: https://github.com/CreatingData/Historical-Populations
df <- read_csv("data/base/source/hist_pop.csv") 

peak <- df %>% 
  select(-cbsa_fips,-(city_fips:city)) %>% 
  pivot_longer(-cbsa,names_to = "census_yr", values_to = "pop") %>% 
  group_by(cbsa) %>% 
  mutate(peak = ifelse(pop == max(pop), 1, 0)) %>% 
  filter(peak == 1) %>% 
  mutate(peak_yr = str_sub(census_yr,-4)) %>% 
  select(cbsa,peak_yr,pop) %>% 
  rename(peak_pop = pop) %>% 
  mutate(decades_since_peak = (2010-as.numeric(peak_yr))/10) %>% 
  left_join(.,(df %>% select(cbsa,pop_2010)), by = "cbsa") %>% 
  mutate(decline_since_peak = abs((pop_2010-peak_pop)/peak_pop))

age <- df %>% 
  select(-cbsa_fips,-(city_fips:city)) %>% 
  pivot_longer(-cbsa,names_to = "census_yr", values_to = "pop") %>% 
  mutate(over50k = if_else(pop >= 50000,1,0)) %>% 
  filter(over50k == 1) %>% #select(-pop) %>% 
  mutate(year = str_sub(census_yr,-4)) %>% 
  group_by(cbsa) %>% 
  mutate(first = ifelse(year == min(year), 1, 0)) %>% 
  filter(first == 1) %>% 
  mutate(age = 2010-as.numeric(year)) %>% 
  select(cbsa,age,census_yr,pop)

age_data <- df %>% select(1:4) %>% 
  inner_join(.,age,by = "cbsa") %>% 
  inner_join(.,peak,by = "cbsa") %>% 
  write_csv("data/base/generated/peak_pop.csv")


