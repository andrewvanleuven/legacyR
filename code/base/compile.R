library(tidyverse)

# Specify the universe ----------------------------------------------------
universe <- read_csv("data/clustrdata.csv") %>% 
  select(1:2) %>% rename_all(tolower) %>% 
  rename(cbsa_fips = id,
         cbsa = name)

# Read in all variables ---------------------------------------------------
bridge <- read_csv("data/base/generated/bridges.csv") %>% 
  inner_join(.,universe, by = "cbsa_fips") %>% 
  select(-name)
hud <- read_csv("data/base/generated/hud_units.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips") %>% 
  select(-cbsa)
vacancy <- read_csv("data/base/generated/vacancy_city_msa.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips")  %>% 
  select(-cbsa)
sfund <- read_csv("data/base/generated/npl.csv") %>% 
  inner_join(.,universe, by = "cbsa_fips")  %>% 
  select(-cbsa)
ages <- read_csv("data/base/generated/ages.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips")  %>% 
  select(-cbsa)
poverty <- read_csv("data/base/generated/poverty.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips")  %>% 
  select(-cbsa)
education <- read_csv("data/base/generated/education.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips")  %>% 
  select(-cbsa)
nativity <- read_csv("data/base/generated/nativity.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips") 
housing_stock <- read_csv("data/base/generated/prewar_housing_stock.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips") 
health <- read_csv("data/base/generated/health.csv") %>%
  inner_join(.,universe, by = "cbsa_fips") 
gini <- read_csv("data/base/generated/gini.csv") %>% 
  inner_join(.,universe, by = "cbsa_fips") 
mfg <- read_csv("data/base/generated/mfg.csv") %>% 
  select(-cbsa,-name) %>% 
  inner_join(.,universe, by = "cbsa_fips") 
capital <- read_csv("data/base/source/st_cap.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips") 
intermodal <- read_csv("data/base/generated/freight.csv") %>% 
  inner_join(.,universe, by = "cbsa_fips") 
housing_value <- read_csv("data/base/generated/housing_value.csv") %>% 
  select(-city_fips,-city,-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips") 
flights <- read_csv("data/base/generated/flights.csv") %>% 
  left_join(universe,., by = "cbsa_fips") %>% 
  replace_na(list(enplanements = 0)) 
nccs <- read_csv("data/base/generated/charitable.csv") %>% 
  select(-cbsa) %>% 
  inner_join(.,universe, by = "cbsa_fips") 
hist_register <- read_csv("data/base/generated/hist_register.csv") %>% 
  left_join(universe,., by = "cbsa_fips") %>% 
  select(-name)
access <- read_csv("data/base/generated/access.csv") %>% 
  left_join(universe,., by = "cbsa")
univ <- read_csv("data/base/generated/univ.csv") %>% 
  left_join(universe,., by = "cbsa_fips") %>% 
  select(-name)
lfpr <- read_csv("data/base/generated/lfpr.csv") %>% 
  left_join(universe,., by = "cbsa_fips")
crime <- read_csv("data/base/generated/crimes.csv") %>% 
  left_join(universe,., by = "cbsa_fips")
hist_pop <- read_csv("data/base/generated/peak_pop.csv") %>% 
  left_join(universe,., by = "cbsa_fips") %>% 
  select(-city_fips,-city) %>% 
  setNames(paste0('city_', names(.))) %>% 
  rename(cbsa_fips = 1)
density <- read_csv("data/base/generated/density_2005.csv") %>% 
  left_join(universe,., by = "cbsa_fips") %>% select(-name)
pop <- read_csv("data/base/generated/populations.csv") %>% 
  left_join(universe,., by = "cbsa_fips") %>% select(-name)

# Merge everything into one DF --------------------------------------------
merge <- inner_join(bridge,hud, by = "cbsa_fips") %>% 
  select(cbsa_fips,cbsa,everything()) %>% 
  inner_join(.,vacancy, by = "cbsa_fips") %>% 
  inner_join(.,sfund, by = "cbsa_fips") %>% 
  inner_join(.,ages, by = "cbsa_fips") %>% 
  inner_join(.,poverty, by = "cbsa_fips") %>% 
  inner_join(.,education, by = "cbsa_fips")
