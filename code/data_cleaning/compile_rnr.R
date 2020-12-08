library(tidyverse)

df <- read_csv("data/master.csv") %>% 
  select(-med_val_cbsa)

housing_value <- read_csv("data/base/generated/housing_values.csv") %>% 
  select(1,5) 

df_new <- df %>% inner_join(housing_value, by = "cbsa_fips") %>% 
  write_csv("data/master_rnr.csv")

