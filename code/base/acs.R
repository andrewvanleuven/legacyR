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
univ <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id)


gini <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                variables = c(gini_coeff = "B19083_001")) %>% 
  mutate(cbsa_fips = as.numeric(GEOID),
         gini = estimate) %>% 
  select(cbsa_fips,gini) %>% 
  inner_join(.,univ) %>% 
  select(1,3,2) %>% 
  write_csv("data/base/generated/gini.csv")


