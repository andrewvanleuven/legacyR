library(tidyverse)

univ <- read_csv("data/base/univ.csv") 

df <- read_csv("data/base/source/fbi_ucr.csv") %>% 
  rename(cbsa_fips = cbsa05,
         burglary = mburg05,
         larceny = mlarc05,
         moto_theft = mmoto05) %>% 
  inner_join(.,univ)

crimes <- read_csv("data/base/generated/pop_2005.csv") %>% 
  inner_join(.,df) %>% 
  rename(pop = population_2005) %>% 
  mutate(prop_crimes_per100k = (burglary+larceny+moto_theft)/(pop/100000)) %>% 
  select(1,2,prop_crimes_per100k,everything(),-name) %>% 
  write_csv("data/base/generated/crimes.csv")
