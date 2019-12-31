library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
library(ggpubr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Setup the Universe ------------------------------------------------------
univ <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id)
xw <- read_csv("data/xw.csv") 
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))

# Read in Bridges Data ----------------------------------------------------
### SOURCE: https://www.fhwa.dot.gov/bridge/nbi/ascii.cfm
# DATA DICTIONARY: http://nationalbridges.com/nbiDesc.html
# NOTE -> the status field definition of bridge deficiencies is limited only to
# those bridges which are 10 years or older and are more than 20 feet in length
df <- read_delim(file = "hidden/too_big/bridge.txt", delim = ",")

bridge <- df %>% select(STATE_CODE_001,COUNTY_CODE_003,STATUS_WITH_10YR_RULE,SUFFICIENCY_RATING) %>% 
  rename_all(tolower) %>% 
  filter(!is.na(county_code_003)) %>% 
  mutate(cty_fips = paste0(state_code_001,county_code_003),
         deficient = if_else(status_with_10yr_rule != 0,1,0),
         bridge = if_else(!is.na(status_with_10yr_rule),1,0)) %>% 
  group_by(cty_fips) %>% 
  summarize(bridges = sum(bridge),
            bad_bridges = sum(deficient)) %>% 
  replace_na(list(bad_bridges = 0)) %>% 
  left_join(.,xw) %>% 
  group_by(cbsa_fips) %>% 
  summarize(total_bridges_2005 = sum(bridges),
            bad_bridges_2005 = sum(bad_bridges)) %>% 
  right_join(.,univ) %>% 
  replace_na(list(bad_bridges_2005 = 0,total_bridges_2005 = 0)) %>% 
  select(1,4,2,3) %>% 
  mutate(pct_deficient = bad_bridges_2005/total_bridges_2005) %>% 
  write_csv("data/base/generated/bridges.csv")
