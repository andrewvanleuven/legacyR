library(tidyverse)

# Specify the universe ----------------------------------------------------
universe <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id,
         cbsa = name)
univ <- universe %>% select(1)

# Read in all variables ---------------------------------------------------
bridge <- read_csv("data/base/generated/bridges.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
hud <- read_csv("data/base/generated/hud_units.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
vacancy <- read_csv("data/base/generated/vacancy_city_msa.csv") %>% inner_join(.,univ, by = "cbsa_fips")
sfund <- read_csv("data/base/generated/npl.csv") %>% inner_join(.,univ, by = "cbsa_fips")
ages <- read_csv("data/base/generated/ages.csv") %>% inner_join(.,univ, by = "cbsa_fips") %>% 
  setNames(paste0('age_', names(.))) %>% 
  rename(cbsa_fips = 1)
poverty <- read_csv("data/base/generated/poverty.csv") %>% inner_join(.,univ, by = "cbsa_fips") %>% 
  setNames(paste0('pov_', names(.))) %>% 
  rename(cbsa_fips = 1)
education <- read_csv("data/base/generated/education.csv") %>% inner_join(.,univ, by = "cbsa_fips") %>% 
  setNames(paste0('edu_', names(.))) %>% 
  rename(cbsa_fips = 1)
nativity <- read_csv("data/base/generated/nativity.csv") %>% inner_join(.,univ, by = "cbsa_fips") %>% 
  setNames(paste0('nat_', names(.))) %>% 
  rename(cbsa_fips = 1, cbsa = 2)
housing_stock <- read_csv("data/base/generated/prewar_housing_stock.csv") %>% inner_join(.,univ, by = "cbsa_fips") %>% 
  select(-(2:4))
health <- read_csv("data/base/generated/health.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
gini <- read_csv("data/base/generated/gini.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
mfg <- read_csv("data/base/generated/mfg.csv") %>% inner_join(.,univ, by = "cbsa_fips") %>% 
  setNames(paste0('mfg_', names(.))) %>% 
  rename(cbsa_fips = 1, cbsa = 2)
capital <- read_csv("data/base/source/st_cap.csv") %>%  inner_join(.,univ, by = "cbsa_fips") 
intermodal <- read_csv("data/base/generated/freight.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
housing_value <- read_csv("data/base/generated/housing_value.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
nccs <- read_csv("data/base/generated/charitable.csv") %>% inner_join(.,univ, by = "cbsa_fips") 
hist_register <- read_csv("data/base/generated/hist_register.csv") %>% left_join(univ,., by = "cbsa_fips") %>% 
  rename(registry_total = hist_reg) %>% 
  setNames(paste0('hist_', names(.))) %>% 
  rename(cbsa_fips = 1, cbsa = 2)
access <- read_csv("data/base/generated/access.csv") %>% left_join(universe,., by = "cbsa")
univs <- read_csv("data/base/generated/univ.csv") %>% left_join(univ,., by = "cbsa_fips")
lfpr <- read_csv("data/base/generated/lfpr.csv") %>% left_join(univ,., by = "cbsa_fips") %>% 
  setNames(paste0('laus_', names(.))) %>% 
  rename(cbsa_fips = 1, cbsa = 2)
crime <- read_csv("data/base/generated/crimes.csv") %>% left_join(univ,., by = "cbsa_fips") %>% 
  select(-pop) %>% rename(per100k = prop_crimes_per100k) %>% 
  setNames(paste0('prop_crimes_', names(.))) %>% 
  rename(cbsa_fips = 1, cbsa = 2)
density <- read_csv("data/base/generated/density_2005.csv") %>% left_join(univ,., by = "cbsa_fips") %>% select(-name)
pop <- read_csv("data/base/generated/populations.csv") %>% left_join(univ,., by = "cbsa_fips")
flights <- read_csv("data/base/generated/flights.csv") %>% left_join(univ,., by = "cbsa_fips") %>% 
  replace_na(list(enplanements = 0)) 
hist_pop <- read_csv("data/base/generated/peak_pop.csv") %>% 
  left_join(univ,., by = "cbsa_fips") %>% 
  select(-city_fips,-city) %>% 
  setNames(paste0('city_', names(.))) %>% 
  rename(cbsa_fips = 1) %>% select(-city_cbsa)

# Merge everything into one DF --------------------------------------------
merge1 <- inner_join(bridge,hud, by = "cbsa_fips") %>% 
  select(cbsa_fips,cbsa,everything(),-name) %>% 
  inner_join(.,vacancy %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,sfund %>% select(-name,-sqmi), by = "cbsa_fips") %>% 
  inner_join(.,ages %>% select(-age_cbsa), by = "cbsa_fips") %>% 
  inner_join(.,poverty %>% select(-pov_cbsa), by = "cbsa_fips") %>% 
  inner_join(.,education %>% select(-edu_cbsa), by = "cbsa_fips")
merge2 <- inner_join(nativity,housing_stock, by = "cbsa_fips") %>% 
  inner_join(.,health %>% select(-name), by = "cbsa_fips") %>% 
  inner_join(.,gini %>% select(-name), by = "cbsa_fips") %>% 
  inner_join(.,mfg %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,capital %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,intermodal %>% select(1,3), by = "cbsa_fips") %>% 
  inner_join(.,housing_value %>% select(1,5,6), by = "cbsa_fips") %>% 
  inner_join(.,nccs %>% select(-cbsa), by = "cbsa_fips")
merge3 <- inner_join(hist_register,access %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,univs %>% select(-name), by = "cbsa_fips") %>% 
  inner_join(.,lfpr %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,crime %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,density, by = "cbsa_fips") %>% 
  inner_join(.,pop %>% select(1,5), by = "cbsa_fips") %>% 
  inner_join(.,flights, by = "cbsa_fips") %>% 
  inner_join(.,hist_pop, by = "cbsa_fips") 

master_merge <- inner_join(merge1,merge2 %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,merge3 %>% select(-cbsa), by = "cbsa_fips")

master <- inner_join(merge1,merge2 %>% select(-cbsa), by = "cbsa_fips") %>% 
  inner_join(.,merge3 %>% select(-cbsa), by = "cbsa_fips") %>% 
  select(1:2,            # IDs
         70:72,          # Density
         13:17,          # U18/O65
         30:34,          # Nativity
         61:65,          # LFPR
         58,             # Access Index
         80,             # City Age
         59:60,          # R1/R2 Universities
         47,             # State Capital
         41:46,          # MFG Base
         51,             # Nonprofit Assets
         48,             # Intermodal Freight
         74,             # Enplanements
         52:57,          # Hist. Registry
         75:79,          # Decline/Peak
         73,             # 2000-05 % Chg.
         9:10,           # Vacancy
         35:36,          # Pre-war Housing
         6:8,            # Public Housing
         49:50,          # Housing Value
         66:69,          # Prop. Crimes
         37:39,          # Health Measures
         18:20,          # Poverty
         40,             # Gini Coeff.
         3:5,            # Deficient Bridges
         11:12,          # Brownfields
         21:29,          # Education Stock
         ) %>% 
  distinct() %>% 
  mutate_if(is.numeric, round, 4) %>% 
  write_csv("data/master.csv")
