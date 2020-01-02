library(tidyverse)
library(httr)
library(rleuven)

# Function & Data ---------------------------------------------------------
### SOURCE: https://nccs.urban.org/code/prep-nccs-core-file-data
core2005pf <- read_csv("hidden/too_big/core2005pf.csv")
xw <- read_csv("data/csa_xw.csv") %>% 
  select(-cbsa,-cbsa_type) %>% 
  inner_join(.,(read_csv("data/xw.csv")), by = "cbsa_fips") %>% 
  select(csa_fips,everything()) %>% arrange(csa_fips,cbsa_fips)
msaxw <- xw %>% select(cbsa_fips,cbsa,cbsa_type) %>% 
  filter(cbsa_type == "Metropolitan Statistical Area") %>% 
  group_by(cbsa) %>% 
  summarise(cbsa_fips = mean(cbsa_fips)) %>% 
  filter(!str_detect(cbsa, ', PR'))
csa <- xw %>% select(cbsa_fips,cbsa,csa_fips,csa) %>% 
  distinct() %>% 
  filter(!str_detect(cbsa, ', PR')) %>% arrange(cbsa_fips)
# Subset NCCS Data --------------------------------------------------------
useful <- core2005pf %>% 
  select(EIN,STYEAR,NAME,STATE,CITY,FIPS,NTEECC,P2TOTAST) %>%
  rename(cty_fips = FIPS) %>% rename_all(tolower) %>% 
  left_join(.,xw) %>% 
  filter(nteecc %in% c("T20", "T22", "T31", "T21", "T70", "U05", "H05", "E05", "K05", "J05", "S05"),
         cbsa_type == "Metropolitan Statistical Area") %>% 
  select(name,cbsa_fips,everything(),-ein,-styear,-cty_fips) %>% 
  arrange(cbsa_fips)
beepr::beep()

charitable <- useful %>% 
  filter(nteecc %in% c("T20", "T22", "T31", "T21", "T70"))%>% 
  group_by(cbsa_fips) %>% 
  summarize(charitable_assets = sum(p2totast)) %>% 
  left_join(msaxw,.) %>% 
  replace_na(list(charitable_assets = 0)) %>% 
  select(cbsa_fips,cbsa,charitable_assets) 

char_csa <- left_join(charitable,csa %>% select(1,3)) %>% 
  filter(!is.na(csa_fips)) %>% 
  group_by(csa_fips) %>% 
  summarise(assets = sum(charitable_assets))


pop <- read_csv("data/base/generated/populations.csv") %>% 
  inner_join(read_csv("data/base/univ.csv"), by = "cbsa_fips") %>% 
  select(1,4) %>% 
  left_join(.,(csa %>% select(1,3)), by = "cbsa_fips") 

csapop <- pop %>% 
  filter(!is.na(csa_fips)) %>% 
  group_by(csa_fips) %>% 
  summarise(csapop = sum(population_2005))

char_merge <- left_join(charitable,csa %>% select(1,3)) %>% 
  left_join(.,char_csa) %>% 
  left_join(.,pop) %>% 
  left_join(.,csapop) %>% 
  mutate(charitable_assets_csa = coalesce(assets,charitable_assets),
         population_csa = coalesce(csapop,population_2005),
         char_assets_percap_csa = charitable_assets_csa/population_csa) %>% 
  inner_join(read_csv("data/base/univ.csv"), by = "cbsa_fips") %>% 
  select(1:3,10) %>% 
  write_csv("data/base/generated/charitable.csv")

# Unused ------------------------------------------------------------------
#sci_health <- useful %>% 
#  filter(nteecc %in% c("U05", "H05", "E05", "K05")) %>% 
#  group_by(cbsa_fips) %>% 
#  summarize(sci_health_assets = sum(p2totast))
#comnty_imp <- useful %>% 
#  filter(nteecc %in% c("J05", "S05")) %>% 
#  group_by(cbsa_fips) %>% 
#  summarize(comnty_imp_assets = sum(p2totast))
#assets_990 <- full_join(charitable,sci_health, by = "cbsa_fips") %>% 
#  full_join(.,comnty_imp, by = "cbsa_fips")
