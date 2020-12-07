library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ipumsr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

acs05 <- load_variables(2009, "acs5", cache = T)

us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("PR")) %>% pull(STUSPS)
univ <- read_csv("data/base/univ.csv") 

# Median Value CBSA -------------------------------------------------------
### B25077.  Median Value (Dollars) for Owner-Occupied Housing Units

h_value_city <- read_csv("data/base/source/housing_value.csv")

med_val_cbsa <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                        variables = "B25077_001",
                        year = 2013) %>% 
  mutate(cbsa_fips = as.numeric(GEOID),
         median_value_msa = estimate) %>% 
  select(cbsa_fips,median_value_msa) %>% 
  inner_join(univ, by = "cbsa_fips") %>% 
  select(1,3,2) 

med_hhinc_cbsa <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                          variables = "B19001_001",
                          year = 2013) %>% 
  mutate(cbsa_fips = as.numeric(GEOID),
         median_hhinc_msa = estimate) %>% 
  select(cbsa_fips,median_hhinc_msa) %>% 
  inner_join(univ, by = "cbsa_fips") %>% 
  select(1,2) %>% 
  inner_join(med_val_cbsa, by = "cbsa_fips") %>% 
  mutate(value_per_hh = round(median_value_msa/median_hhinc_msa,2)) %>% 
  select(cbsa_fips,name,value_per_hh) %>% 
  left_join(.,h_value_city) %>% 
  select(cbsa_fips,cbsa,city_fips,city,value_per_hh,median_value) %>% 
  rename(med_val_cbsa = value_per_hh,
         med_val_city = median_value) %>% 
  write_csv("data/base/generated/housing_values.csv")

