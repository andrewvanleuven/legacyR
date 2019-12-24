library(tidyverse)

xw <- read_csv("data/xw.csv") 
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))

df <- read_csv("data/base/source/laus.csv") %>% 
  mutate(cty_fips = str_pad(cty_fips,5,"left","0")) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(lf = sum(labor_force),
            emp = sum(employed),
            unemp = sum(unemployed)) %>% 
  inner_join(.,msaxw) %>% select(-cbsa)

lfpr <- read_csv("data/base/generated/pop_2005.csv") %>% 
  left_join(.,df) %>% 
  rename(pop = population_2005) %>% 
  mutate(lfpr = lf/pop) %>% 
  select(1,2,lfpr,everything()) %>% 
  write_csv("data/base/generated/lfpr.csv")
