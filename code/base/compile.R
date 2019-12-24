library(tidyverse)

univ <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id)

bridge <- read_csv("data/base/generated/bridges.csv") %>% inner_join(.,univ) 
hist_pop <- read_csv("data/base/generated/peak_pop.csv") # need to fix
health <- read_csv("data/base/generated/health.csv")

