library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

# Cluster-Discriminant ----------------------------------------------------
clustr_assign <- dget("code/functions/clustr_assign.R")
df <- read_csv("data/master.csv") %>% 
  filter(!cbsa_fips %in% c(11260,21820,46520)) 
id <- df %>% select(1:2)
df_spec <- df %>% select(read_csv("data/analysis/specifications/vars_02.csv") %>% pull())  
clusters <- (clustr_assign(df_spec))$combined %>% 
  select(1,2,7,8)
rm(df_spec,id,df,clustr_assign)

# Outcome 1: Population Growth --------------------------------------------
p15 <- read_csv("data/base/generated/pop15.csv")
p10 <- read_csv("data/base/generated/pop10.csv")
pop <- read_csv("data/base/generated/populations.csv") %>% 
  select(-pctchg_00_05,-name) %>% 
  left_join(p10, by = "cbsa_fips") %>% 
  left_join(p15, by = "cbsa_fips")

cluster_pop <- clusters %>% select(-clusterm_18) %>% 
  left_join(pop, by = "cbsa_fips")

# Outcome 2: Employment Growth --------------------------------------------
emp <- read_csv("hidden/confidential/emp_moodys.csv") %>% 
  select(cbsa_fips:emp2014,-metro_name) %>% 
  filter(cbsa_fips < 99999)

cluster_emp <- clusters %>% select(-clusterm_18) %>% 
  left_join(emp, by = "cbsa_fips")

# Outcome 3: GDP Per-Capita -----------------------------------------------


# Outcome 4: Income Per-Capita --------------------------------------------


